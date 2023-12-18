pcaServer <- function(id, valid_datasets, selected_valid_dataset, current_data_obj_names, file_format) {

    ns <- NS(id)

    stopifnot(is.reactive(valid_datasets))
    stopifnot(is.reactive(selected_valid_dataset))
    stopifnot(is.reactive(current_data_obj_names))
    stopifnot((is.reactive(file_format)))

    moduleServer(id, function(input, output, session) {

        pca <- reactiveValues(valid_datasets = NULL,
                              selected_valid_dataset = NULL,
                              vars_to_remove = NULL,
                              file_format = "tsv",
                              pca_result = NULL,
                              scores_to_export = NULL,
                              transferred_pcs = NULL
                              )

        # Valid datasets and removable variables
        observe({

            pca$valid_datasets <- valid_datasets()
            pca$selected_valid_dataset <- selected_valid_dataset()
            pca$file_format <- file_format()

        })

        observe({

            pca$vars_to_remove <- colnames(valid_datasets()[[input$dataset_select]][["dataset"]])

        })

        observe({

            if (length(pca$valid_datasets) == 0) return()

            updateSelectInput(session = session,
                              inputId = "vars_to_remove",
                              choices = pca$vars_to_remove,
                              selected = "")

        })

        observe({

            updateSelectInput(session = session,
                              inputId = "dataset_select",
                              choices = names(pca$valid_datasets),
                              selected = get_selected_dataset(pca$selected_valid_dataset,
                                                              pca$valid_datasets))

        })

        # Perform PCA and save results
        observeEvent(input$apply_pca, {

            if (length(pca$valid_datasets) == 0) return()

            data <- pca[["valid_datasets"]][[isolate(input$dataset_select)]][["dataset"]]

            pca$pca_result <- mvpa::perform_pca(data,
                                                standardize = isolate(input$standardize),
                                                exclude_variables = isolate(input$vars_to_remove))

            pca$pca_result$dataset_name <- isolate(input$dataset_select)

            showNotification("Principal component analysis has been performed.")

        })

        #

        # Scree plot + options
        observeEvent(input$apply_pca, {

            if (is.null(pca$pca_result)) return()

            pcs <- colnames(pca$pca_result$scores)

            pcs_to_plot <- ifelse(trunc(length(pca$pca_result$explained_variance)) > 10,
                                  "PC10",
                                  tail(pcs, n=1))

            output$scree_plot_options <- renderUI({

                tagList(
                        column(width = 12,

                               selectInput(inputId = NS(id, "pc_count"),
                                           label = "Select number of principal components",
                                           choices = pcs,
                                           selected = pcs_to_plot,
                                           multiple = FALSE),

                               checkboxInput(inputId = NS(id, "add_cumulative_perc"),
                                             label = "Add cumulative percentage",
                                             value = FALSE)
                        )
                )

            })


        })

        observe({

            if (is.null(pca$pca_result)) {
                output$scree_plot <- plotly::renderPlotly({

                    no_data_plot()

                })
            }

            if (is.null(input$pc_count) | is.null(input$add_cumulative_perc)) {
                return()
            } else {

                if (is.na(input$pc_count)) {
                    output$scree_plot <- plotly::renderPlotly({

                        no_data_plot()

                    })
                    return()
                }

                output$scree_plot <- plotly::renderPlotly({

                    filename <- paste(isolate(pca$pca_result$dataset_name), "Scree-plot", Sys.Date(), sep = "_")

                    fig <- mvpa::plot_scree_plot(pca_result = pca$pca_result,
                                                 how_many_PCs = get_pc_numeric(input$pc_count),
                                                 add_cumulative_perc = input$add_cumulative_perc,
                                                 rel_font_size = input$rel_font_size)

                    plotly_helper(fig, filename)

                })
            }

        })

        # Scores plot
        observe({

            if (is.null(pca$pca_result)) return()

            pcs <- colnames(pca$pca_result$scores)

            output$scores_plot_options <- renderUI({

                tagList(
                    column(width = 4,

                           selectInput(inputId = NS(id, "pc_x_axis"),
                                       label = "Select principal component (x axis)",
                                       selected = "PC1",
                                       choices = pcs,
                                       multiple = FALSE,
                                       ),

                           selectInput(inputId = NS(id, "pc_y_axis"),
                                        label = "Select principal component (y axis)",
                                        selected = "PC2",
                                        choices = pcs,
                                        multiple = FALSE,
                                        ),
                     ),

                    column(width = 4,

                           selectInput(inputId = NS(id, "pc_select"),
                                       label = "Select principal component(s)",
                                       choices = pcs,
                                       multiple = TRUE),

                           actionButton(inputId = NS(id, "apply_pc_transfer"),
                                        label = "Transfer PC(s) to current dataset")

                    ),

                    column(width = 4,

                           sliderInput(inputId = NS(id, "rel_point_size"),
                                       label = "Relative point size",
                                       min = 1,
                                       max = 3,
                                       value = 1,
                                       step = .1),

                           checkboxInput(inputId = NS(id, "df_switch_scores"),
                                         label = "Show dataframe",
                                         value = FALSE)

                    ),

                    uiOutput(NS(id, "df_scores_output"))

                )

            })

        })

        observeEvent(input$apply_pc_transfer, {

            selected_pcs <- isolate(input$pc_select)
            if (length(selected_pcs) == 0) {
                showNotification("No principal component has been selected.", type = "message")
                return()
            }

            pc_df <- as.data.frame(pca$pca_result$scores[, selected_pcs])
            object_names_current_data <- current_data_obj_names()

            if (length(object_names_current_data) != length(rownames(pc_df))) {
                showNotification("The number of objects between the PCs and the current dataset differs. Please revise current dataset!", type = "error")
                return()
            }

            if (!all(rownames(pc_df) %in% object_names_current_data)) {
                showNotification("The object names between the PCs and the current data do not match. Please revise current dataset!", type = "error")
                return()
            }

            if (length(selected_pcs) == 1) {
                colnames(pc_df) <- selected_pcs
            }

            pca$transferred_pcs <- list("counter" = as.numeric(input$apply_pc_transfer) , "data" =  pc_df)
            showNotification("PC(s) have been transferred to the current dataset.", type = "message")

        })

        # Plot PCA scores
        observe({

            if (is.null(pca$pca_result)) {
                output$scores_plot <- plotly::renderPlotly({

                    no_data_plot()

                })
                return()
            }

            if (!is.null(input$rel_point_size) & !is.null(input$pc_x_axis) & !is.null(input$pc_y_axis)) {

                if (is.na(input$pc_x_axis) | is.na(input$pc_y_axis)) {
                    output$scores_plot <- plotly::renderPlotly({

                        no_data_plot()

                    })

                    showNotification("Missing principal component(s) to plot", type = "error")

                    return()
                }

                output$scores_plot <- plotly::renderPlotly({

                    filename <- paste(isolate(pca$pca_result$dataset_name), "PCA-scores", Sys.Date(), sep = "_")

                    fig <- mvpa::plot_pca_2d(pca_result = pca$pca_result,
                                             plot = "scores",
                                             PCs_to_plot = c(get_pc_numeric(input$pc_x_axis), get_pc_numeric(input$pc_y_axis)),
                                             rel_font_size = input$rel_font_size,
                                             rel_point_size = input$rel_point_size)

                    plotly_helper(fig, filename)

                })

            }

        })

        # Scores dataframe
        observe({

            if (is.null(pca$pca_result)) return()
            if (is.null(input$df_switch_scores)) return()

            if (!input$df_switch_scores) {
                output$df_scores_output <- renderUI({})
                return()
            }

            data <- pca$pca_result$scores

            output$df_scores_output <- renderUI({
                tagList(
                    column(width = 12,

                           hr(),

                           downloadButton(NS(id, "download_scores"),
                                          label = "Download table"),

                           actionButton(NS(id, "copy_scores"),
                                        label = "Copy to clipboard"),

                           DT::renderDT({

                               DT::datatable(adjust_decimals(data),
                                             extensions = c("Scroller"),
                                             options = list(scrollY = 450,
                                                            scrollX = 500,
                                                            deferRender = TRUE,
                                                            scroller = TRUE,
                                                            dom = "Bfrtip",
                                                            columnDefs = list(list(className = 'dt-center', targets = "_all"))),
                                             selection = 'none')

                           })
                    )
                )
            })


        })

        # Download & copy scores
        output$download_scores <- downloadHandler(
            filename = function() {
                paste0(isolate(pca$pca_result$dataset_name),"_PCA-scores_", Sys.Date(), ".", isolate(pca$file_format))
            },
            content = function(file) {
                write_table(data = pca$pca_result$scores,
                            format = isolate(pca$file_format),
                            file = file)
            }
        )

        observeEvent(input$copy_scores, {
            # The number behind clipboard indicates the size limit
            # I used 5 MB
            if (!is.null(pca$pca_result)) {

                clipr::clear_clip()
                clipr::write_clip(pca$pca_result$scores,
                                  allow_non_interactive = TRUE)

                showNotification("The PCA scores have been copied to the clipboard",
                                 type = "message")
            }
        })

        # Loadings plot
        observe({

            if (is.null(pca$pca_result)) return()

            pcs <- colnames(pca$pca_result$scores)

            output$loadings_plot_options <- renderUI({

                tagList(
                    column(width = 4,

                           selectInput(inputId = NS(id, "pc_x_axis_loadings"),
                                       label = "Select principal component (x axis)",
                                       selected = "PC1",
                                       choices = pcs,
                                       multiple = FALSE),

                           selectInput(inputId = NS(id, "pc_y_axis_loadings"),
                                       label = "Select principal component (y axis)",
                                       selected = "PC2",
                                       choices = pcs,
                                       multiple = FALSE),
                    ),

                    column(width = 4,

                           sliderInput(inputId = NS(id, "rel_point_size_loadings"),
                                       label = "Relative point size",
                                       min = 1,
                                       max = 3,
                                       value = 1,
                                       step = .1),

                           checkboxInput(inputId = NS(id, "df_switch_loadings"),
                                         label = "Show dataframe",
                                         value = FALSE)

                    ),

                    column(width = 4),

                    uiOutput(NS(id, "df_loadings_output"))
                )

            })

        })

        observe({

            if (is.null(pca$pca_result)) {
                output$loadings_plot <- plotly::renderPlotly({

                    no_data_plot()

                })
                return()
            }
            if (!is.null(input$rel_point_size_loadings) & !is.null(input$pc_x_axis_loadings) & !is.null(input$pc_y_axis_loadings)) {

                if (is.na(input$pc_x_axis_loadings) | is.na(input$pc_y_axis_loadings)) {
                    output$scores_plot <- plotly::renderPlotly({

                        no_data_plot()

                    })

                    showNotification("Missing principal component(s) to plot", type = "error")

                    return()
                }

                output$loadings_plot <- plotly::renderPlotly({

                    filename <- paste(isolate(pca$pca_result$dataset_name), "PCA-loadings", Sys.Date(), sep = "_")

                    fig <- mvpa::plot_pca_2d(pca_result = pca$pca_result,
                                             plot = "loadings",
                                             PCs_to_plot = c(get_pc_numeric(input$pc_x_axis_loadings), get_pc_numeric(input$pc_y_axis_loadings)),
                                             rel_font_size = input$rel_font_size,
                                             rel_point_size = input$rel_point_size_loadings)

                    plotly_helper(fig, filename)

                })
            }

        })


        # Loadings dataframe
        observe({

            if (is.null(pca$pca_result)) return()
            if (is.null(input$df_switch_loadings)) return()

            if (!input$df_switch_loadings) {
                output$df_loadings_output <- renderUI({})
                return()
            }

            data <- pca$pca_result$loadings

            output$df_loadings_output <- renderUI({
                tagList(
                    column(width = 12,

                           hr(),

                           downloadButton(NS(id, "download_loadings"),
                                          label = "Download table"),

                           actionButton(NS(id, "copy_loadings"),
                                        label = "Copy to clipboard"),

                           DT::renderDT({

                               DT::datatable(adjust_decimals(data),
                                             extensions = c("Scroller"),
                                             options = list(scrollY = 450,
                                                            scrollX = 500,
                                                            deferRender = TRUE,
                                                            scroller = TRUE,
                                                            dom = "Bfrtip",
                                                            columnDefs = list(list(className = 'dt-center', targets = "_all"))),
                                             selection = 'none')

                           })

                    )
                )
            })

        })

        # Download & copy loadings
        output$download_loadings <- downloadHandler(
            filename = function() {
                paste0(isolate(pca$pca_result$dataset_name),"_PCA-loadings_", Sys.Date(), ".", isolate(pca$file_format))
            },
            content = function(file) {

                write_table(data = pca$pca_result$loadings,
                            format = isolate(pca$file_format),
                            file = file)
            }
        )

        observeEvent(input$copy_loadings, {
            # The number behind clipboard indicates the size limit
            # I used 5 MB
            if (!is.null(pca$pca_result)) {

                clipr::clear_clip()
                clipr::write_clip(pca$pca_result$loadings,
                                  allow_non_interactive = TRUE)
                showNotification("The PCA loadings have been copied to the clipboard",
                                 type = "message")
            }
        })

        # Variable variation plot

        # Plot UI
        observe({

            if (is.null(pca$pca_result)) return()

            output$var_variation_plot_options <- renderUI({

                tagList(
                    column(width = 6,

                           selectizeInput(inputId = NS(id, "var_select_variation_plot"),
                                          label = "Show selected variable(s)",
                                          choices = rownames(pca$pca_result$loadings),
                                          multiple = TRUE),

                           checkboxInput(inputId = NS(id, "rotate"),
                                         label = "Rotate plot"),

                    ),

                    column(width = 6,

                           checkboxInput(inputId = NS(id, "df_switch_var_variation"),
                                         label = "Show dataframe"),

                    ),

                    uiOutput(NS(id, "df_var_variation_output"))

                )

            })
        })

        observe({

            if (is.null(pca$pca_result)) return()

            updateSelectizeInput(session = session,
                                 inputId = "var_select_variation_plot",
                                 choices = rownames(pca$pca_result$loadings))

        })

        # Plot
        observe({

            if (is.null(pca$pca_result)) {
                output$var_variation_plot <- plotly::renderPlotly({

                    no_data_plot()

                })
                return()
            }

            if (is.null(input$rotate) | is.null(input$rel_font_size)) return()

            output$var_variation_plot <- plotly::renderPlotly({

                filename <- paste(isolate(pca$pca_result$dataset_name), "PCA-variable-variation", Sys.Date(), sep = "_")

                fig <- mvpa::plot_variable_variation_pca(pca$pca_result,
                                                         rotate = input$rotate,
                                                         rel_font_size = input$rel_font_size,
                                                         x_filter = input$var_select_variation_plot,
                                                         select_PCs = 5)

                plotly_helper(fig, filename)

            })
        })

        # Variable variation dataframe
        observe({

            if (is.null(pca$pca_result)) return()
            if (is.null(input$df_switch_var_variation)) return()

            if (!input$df_switch_var_variation) {
                output$df_var_variation_output <- renderUI({})
                return()
            }

            data <- pca$pca_result$variable_variation

            output$df_var_variation_output <- renderUI({
                tagList(
                    column(width = 12,

                           hr(),

                           downloadButton(NS(id, "download_var_variation"),
                                          label = "Download table"),

                           actionButton(NS(id, "copy_var_variation"),
                                        label = "Copy to clipboard"),

                           DT::renderDT({

                               DT::datatable(adjust_decimals(data),
                                             extensions = c("Scroller"),
                                             options = list(scrollY = 450,
                                                            scrollX = 500,
                                                            deferRender = TRUE,
                                                            scroller = TRUE,
                                                            dom = "Bfrtip",
                                                            columnDefs = list(list(className = 'dt-center', targets = "_all"))),
                                             selection = 'none')

                           })

                    )
                )
            })

        })


        # Download & copy loadings
        output$download_var_variation <- downloadHandler(
            filename = function() {
                paste0(isolate(pca$pca_result$dataset_name),"_PCA-variable-variation_", Sys.Date(), ".", isolate(pca$file_format))
            },
            content = function(file) {
                write_table(data = pca$pca_result$variable_variation,
                            format = isolate(pca$file_format),
                            file = file)
            }
        )

        observeEvent(input$copy_var_variation, {
            # The number behind clipboard indicates the size limit
            # I used 5 MB
            if (!is.null(pca$pca_result)) {

                clipr::clear_clip()
                clipr::write_clip(pca$pca_result$variable_variation,
                                  allow_non_interactive = TRUE)

                showNotification("The variable variation table has been copied to the clipboard",
                                 type = "message")
            }
        })

        # PCA value bar plot

        # options
        observeEvent(input$apply_pca, {

            if (is.null(pca$pca_result)) return()

            pcs <- colnames(pca$pca_result$scores)

            data <- pca$pca_result$loadings
            min_y <- floor(min(data[,"PC1"]))
            max_y <- ceiling(max(data[,"PC1"]))
            x_vals <- rownames(data)

                output$pca_value_plot_options <- renderUI({

                    tagList(
                        column(width = 4,

                               radioButtons(inputId = NS(id, "pca_value_type"),
                                            label = "Choose PCA value type",
                                            inline = FALSE,
                                            choiceNames = list(HTML("<b>Loadings</b>"),
                                                               HTML("<b>Scores</b>")),
                                                               choiceValues = c("loadings",
                                                                                "scores"),
                                                               selected = "loadings"),

                               selectInput(inputId = NS(id, "pc_select_value_plot"),
                                           label = "Select principal component",
                                           choices = pcs,
                                           multiple = FALSE),

                        ),

                        column(width = 4,

                               selectizeInput(inputId = NS(id, "x_filter_pca_value_plot"),
                                              label = "Select X-axis variables",
                                              choices = x_vals,
                                              multiple = TRUE),

                               sliderInput(inputId = NS(id, "y_filter_pca_value_plot"),
                                           label = "Trim PCA value",
                                           min = min_y,
                                           max = max_y,
                                           step = 0.05,
                                           value = c(min_y, max_y))

                        ),

                        column(width = 4,

                               sliderInput(inputId = NS(id, "rel_font_size_pca_value_plot"),
                                           label = "Change font size",
                                           min = 1,
                                           max = 3,
                                           value = 1,
                                           step = .1),

                               checkboxInput(inputId = NS(id, "rotate_pca_value_plot"),
                                             label = "Rotate plot",
                                             FALSE)
                        ),

                        br(),

                        uiOutput(NS(id, "df_pca_value_plot_output"))
                    )
                })

        })

        # plot
        observe({
            if (is.null(pca$pca_result)) return()

            if (is.null(input$pc_select_value_plot)) return()

            output$pca_value_plot <- plotly::renderPlotly({

                    filename = paste(isolate(pca$pca_result$dataset_name),
                                     "PCA",
                                     isolate(input$pca_value_type),
                                     isolate(input$pc_select_value_plot),
                                     Sys.Date(),
                                     sep = "_")

                    fig <- mvpa::plot_pca_value(pca_result = pca$pca_result,
                                                PC_to_plot = get_pc_numeric(input$pc_select_value_plot),
                                                plot = input$pca_value_type,
                                                x_filter = input$x_filter_pca_value_plot,
                                                rel_font_size = input$rel_font_size_pca_value_plot,
                                                rotate = input$rotate_pca_value_plot)

                    plotly_helper(fig,filename)

                })

        })

        pca

    })

}
