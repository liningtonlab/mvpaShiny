covariateServer <- function (id, valid_datasets, selected_valid_dataset, file_format) {

    stopifnot(is.reactive(valid_datasets))
    stopifnot(is.reactive(selected_valid_dataset))
    stopifnot(is.reactive(file_format))

    ns <- NS(id)

    moduleServer(id, function(input, output, session) {

        # Reactive values
        covariate <- reactiveValues(vars_to_select = NULL,
                                    valid_datasets = list(),
                                    selected_valid_dataset = NULL,
                                    file_format = "tsv",
                                    covariate_proj_result = NULL,
                                    new_dataset = NULL,
                                    x = NULL,
                                    y = NULL)

        # Dataset and variable selection
        observe({
            covariate$file_format <- file_format()
            covariate$valid_datasets <- valid_datasets()
            covariate$selected_valid_dataset <- selected_valid_dataset()
            covariate$vars_to_select <- colnames(valid_datasets()[[input$dataset_select]][["dataset"]])

        })

        observe({

            if (length(covariate$valid_datasets) == 0) return()

            updateSelectInput(session = session,
                              inputId = "var_select_covariate",
                              choices = covariate$vars_to_select,
                              selected = "")

        })

        observe({

            updateSelectInput(session = session,
                              inputId = "dataset_select",
                              choices = names(covariate$valid_datasets),
                              selected = get_selected_dataset(covariate$selected_valid_dataset,
                                                              covariate$valid_datasets))

        })

        ## Perform covariate projection
        observeEvent(input$apply_covariate_projection, {

            if (is.null(covariate$vars_to_select) & (length(covariate$valid_datasets) == 0)) return()

            if(is.null(input$var_select_covariate)) {
                showNotification("Please select at least one putative covariate", type = "message")
                return()
            }

            dataset <- covariate[["valid_datasets"]][[input$dataset_select]][["dataset"]]
            selection <- isolate(input$var_select_covariate)
            covariate$covariate_proj_result <- mvpa::perform_covariate_projection(X_aug = dataset,
                                                                                     covariates = selection,
                                                                                     standardize = isolate(input$standardize))

            covariate$covariate_proj_result$dataset_name <- isolate(input$dataset_select)


        })

        ## Show dataframe
        output$covariate_projection_df <- DT::renderDT({
           if (is.null(covariate$covariate_proj_result)) return()


           DT::datatable(adjust_decimals(covariate$covariate_proj_result$residual_variance_df),
                         extensions = c("Scroller"),
                         options = list(scrollY = 450,
                                        scrollX = 500,
                                        deferRender = TRUE,
                                        scroller = TRUE,
                                        dom = "Bfrtip",
                                        columnDefs = list(list(className = 'dt-center', targets = "_all"))),
                         selection = 'none')

        })

        # Download & copy residual variance dataframe
        output$download_residual <- downloadHandler(

            filename = function() {
                paste0(isolate(input$dataset_select),"_covariate-residual-variance_", Sys.Date(), ".", covariate$file_format)
            },
            content = function(file) {
                write_table(data = covariate$covariate_proj_result$residual_variance_df,
                            format = isolate(covariate$file_format),
                            file = file)
            }
        )

        observeEvent(input$copy_residual, {
            # The number behind clipboard indicates the size limit
            # I used 5 MB
            if (is.null(covariate$covariate_proj_result)) return()

            clipr::clear_clip()
            clipr::write_clip(covariate$covariate_proj_result$residual_variance_df,
                              allow_non_interactive = TRUE)

            showNotification("Residual variances table after covariate projection have been copied to the clipboard",
                             type = "message")

        })

        # Plot covariate plot
        observe({

            if (is.null(covariate$covariate_proj_result)) return()

            output$explained_var_plot_options <- renderUI({
                tagList(

                    column(width = 4,
                        checkboxInput(inputId = NS(id, "rotate"),
                                      label = "Rotate plot",
                                      value = FALSE)
                        ),

                    column(width = 4,
                        sliderInput(inputId = NS(id, "rel_font_size"),
                                    label = "Change font size",
                                    min = 1,
                                    max = 3,
                                    value = 1,
                                    step = .1)
                        ),

                    column(width = 4,
                         checkboxInput(inputId = NS(id, "df_switch"),
                                       label = "Show dataframe",
                                       value = FALSE)
                    ),

                    conditionalPanel(
                        condition = "input.df_switch", ns = ns,

                        column(width = 12,

                            hr(),

                            downloadButton(NS(id, "download_explained"),
                                           label = "Download table"),

                            actionButton(NS(id, "copy_explained"),
                                         label = "Copy to clipboard")

                        )

                    )

                )
            })

        })

        observe({

            if (is.null(covariate$covariate_proj_result)) return()

            if (is.null(input$rotate) & is.null(input$rel_font_size)) return()

            output$covariate_plot <- plotly::renderPlotly({

                filename <- paste(isolate(covariate$covariate_proj_result$dataset_name), "covariate-projection", Sys.Date(), sep = "_")

                fig <- mvpa::plot_explained_var_by_covariates(covariate$covariate_proj_result,
                                                                   rel_font_size = input$rel_font_size,
                                                                   rotate = input$rotate)

                plotly_helper(fig, filename)

            })

        })

        observeEvent(input$df_switch, {
            if (is.null(covariate$covariate_proj_result)) return()

            if (isolate(input$df_switch)) {
                output$explained_var_df <- DT::renderDT({

                    DT::datatable(adjust_decimals(covariate$covariate_proj_result$explained_variance_by_covariates,
                                                  ignore_column = 'covariate'),
                                  extensions = c("Scroller"),
                                  options = list(scrollY = 250,
                                                 scrollX = 500,
                                                 deferRender = TRUE,
                                                 scroller = TRUE,
                                                 dom = "Bfrtip",
                                                 columnDefs = list(list(className = 'dt-center', targets = "_all"))),
                                  selection = 'none')

                })
            } else {
                output$explained_var_df <- DT::renderDT({
                    DT::datatable(NULL)
                })
            }

        })

        # Download & copy explained variance dataframe
        output$download_explained <- downloadHandler(
            filename = function() {
                paste0(isolate(covariate$covariate_proj_result$dataset_name),"_covariate-explained-variance_", Sys.Date(), ".", isolate(covariate$file_format))
            },
            content = function(file) {
                write_table(data = covariate$covariate_proj_result$explained_variance_by_covariates,
                            format = isolate(covariate$file_format),
                            file = file)
            }
        )

        observeEvent(input$copy_explained, {
            # The number behind clipboard indicates the size limit
            # I used 5 MB
            if (is.null(covariate$covariate_proj_result)) return()

            clipr::clear_clip()
            clipr::write_clip(covariate$covariate_proj_result$explained_variance_by_covariates,
                              allow_non_interactive = TRUE)

            showNotification("Explained variances table after covariate projection have been copied to the clipboard",
                             type = "message")

        })

        # Add dataset to valid datasets after covariate projection
        observe({

            if (is.null(covariate$covariate_proj_result)) return()
            output$residual_variance_df_options <- renderUI(

                tagList(
                    actionButton(NS(id, "add_dataset"),
                                 label = "Send dataset to current dataset"),
                )
            )

        })

        observeEvent(input$add_dataset, {

            if(is.null(covariate$covariate_proj_result)) return()

            # Add dataset check again
            covariate$new_dataset <- covariate$covariate_proj_result$residual_variance_df
            showNotification("The dataset replaced the current dataset and is ready for processing", type = "message")
            showNotification("Dataset still needs to be verified before further usage - Go back to Datasets tab", type = "warning", duration = 8)

        })

        covariate

    })
}
