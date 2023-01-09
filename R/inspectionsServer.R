inspectionsServer <- function(id, valid_datasets, selected_valid_dataset, file_format) {

    ns <- NS(id)

    stopifnot(is.reactive(valid_datasets))
    stopifnot(is.reactive(file_format))
    stopifnot(is.reactive(selected_valid_dataset))

    moduleServer(id, function(input, output, session) {

        inspection <- reactiveValues(valid_datasets = NULL,
                                     selected_valid_dataset = NULL,
                                     file_format = "tsv",
                                     correlation_df = NULL)

        # Dataset and variable selection
        observe({

            inspection$valid_datasets <- valid_datasets()
            inspection$selected_valid_dataset <- selected_valid_dataset()
            inspection$file_format <- file_format()

            })

        observe({

            updateSelectInput(session = session,
                              inputId = "dataset_select",
                              choices = names(inspection$valid_datasets),
                              selected = get_selected_dataset(inspection$selected_valid_dataset,
                                                              inspection$valid_datasets))

        })

        observe({

            req(input$dataset_select)

            if (length(isolate(inspection$valid_datasets)) == 0) return()

            if (is.null(isolate(input$var_select_qqplot)) | is.null(isolate(input$response_select))) return()

            updateSelectInput(session = session,
                              inputId = "var_select_qqplot",
                              choices = colnames(valid_datasets()[[input$dataset_select]][["dataset"]]),
                              selected = "")

            updateSelectInput(session = session,
                              inputId = "response_select",
                              choices = colnames(valid_datasets()[[input$dataset_select]][["dataset"]]),
                              selected = inspection[["valid_datasets"]][[input$dataset_select]][["response_var"]])

        })

        # QQ-plot options
        observe({
            if(length(inspection$valid_datasets) == 0 | is.null(input$dataset_select)) {

                output$qq_plot_options <- renderUI({})
                output$qq_plot <- plotly::renderPlotly({

                    no_data_plot()

                })

                return()
            }

            output$qq_plot_options <- renderUI({

                tagList(

                    selectInput(inputId = NS(id, "var_select_qqplot"),
                                label = "Select variable",
                                choices = colnames(valid_datasets()[[input$dataset_select]][["dataset"]]),
                                multiple = TRUE),

                    actionButton(inputId = NS(id, "show_qq"),
                                 label = "Show QQ-plot(s)")

                )

            })

        })

        # QQ-plot method
        observeEvent(input$show_qq, {

            if (length(inspection$valid_datasets) == 0 & is.null(isolate(input$var_select_qqplot))) return()

            if (length(input$var_select_qqplot) == 0) {
                showNotification("No variable has been selected.", type = "message")
                return()
            }

            data_list <- list("data" = inspection[["valid_datasets"]][[input$dataset_select]][["dataset"]],
                              "dataset_name" = isolate(input$dataset_select))

            output$qq_plot <- plotly::renderPlotly({

                filename <- paste(isolate(data_list$dataset_name), "QQ-plot", Sys.Date(), sep = "_")

                fig <- mvpa::qq_plot(data = data_list$data,
                                     vars_to_plot = isolate(input$var_select_qqplot),
                                     rel_font_size = input$rel_font_size)

                plotly_helper(fig, filename)

            })

        })

        # Correlation options
        observe({

            if(length(inspection$valid_datasets) == 0 | is.null(input$dataset_select)) {

                output$correlation_options <- renderUI({})
                output$correlation_plot <- plotly::renderPlotly({

                    no_data_plot()

                })

                return()

            }

            output$correlation_options <- renderUI({

                tagList(

                    column(width = 4,

                           selectInput(inputId = NS(id, "correlation_variant"),
                                       label = "What shall be correlated?",
                                       multiple = FALSE,
                                       choices = c("Variables with response" = "vars_with_response",
                                                   "Variables with each other" = "correlation_matrix")),

                           conditionalPanel(
                               condition = "input.correlation_variant == 'vars_with_response'", ns = ns,

                               selectInput(inputId = NS(id, "response_select"),
                                           label = "Select response",
                                           choices = colnames(valid_datasets()[[input$dataset_select]][["dataset"]]),
                                           selected = inspection[["valid_datasets"]][[input$dataset_select]][["response_var"]],
                                           multiple = FALSE)
                           )
                    ),

                    column(width = 4,

                           selectInput(inputId = NS(id, "var_select_correlation"),
                                       label = "Select variable",
                                       choices = colnames(valid_datasets()[[input$dataset_select]][["dataset"]]),
                                       multiple = TRUE),

                           actionButton(inputId = NS(id, "show_correlation"),
                                        label = "Show correlation plot")
                    ),

                    column(width = 4,

                           conditionalPanel(
                               condition = "input.correlation_variant == 'vars_with_response'", ns = ns,


                               checkboxInput(inputId = NS(id, "rotate"),
                                             label = "Rotate plot",
                                             value = FALSE),

                           ),

                           checkboxInput(inputId = NS(id, "df_switch_correlation"),
                                         label = "Show dataframe",
                                         value = FALSE),

                    ),

                    br(),

                    uiOutput(NS(id, "correlation_df_ui"))

                )
            })

        })

        # Correlation method
        observe({

            if (is.null(input$correlation_variant)) return()

            all_vars <- colnames(inspection[["valid_datasets"]][[input$dataset_select]][["dataset"]])
            if (input$correlation_variant == "vars_with_response") {
				if (!is.null(input$var_select_correlation)) {
					if ("All variables" %in% input$var_select_correlation) {
						updateSelectInput(session,
						                  inputId = "var_select_correlation",
										  choices = setdiff(c("All variables", all_vars), input$response_select),
										  selected = "All variables")
					}
				} else {
					updateSelectInput(session,
					                  inputId = "var_select_correlation",
									  choices = setdiff(c("All variables", all_vars), input$response_select),
									  selected = isolate(input$var_select_correlation))
				}
            } else {
                if (!is.null(input$var_select_correlation)) {
                    if ("All variables" %in% input$var_select_correlation) {
                        updateSelectInput(session,
                                          inputId = "var_select_correlation",
                                          choices = c("All variables", all_vars),
                                          selected = "All variables")
                    }
                } else {
                    updateSelectInput(session,
                                      inputId = "var_select_correlation",
                                      choices = c("All variables", all_vars),
                                      selected = isolate(input$var_select_correlation))
                }
            }

        })

        # Correlation plot
        observeEvent(input$show_correlation, {

            if (length(inspection$valid_datasets) == 0 | is.null(input$rel_font_size)) return()

            if (is.null(isolate(input$var_select_correlation))) {
                showNotification("No variable has been selected.", type = "message")
                return()
            }

            inspection$dataset_name <- isolate(input$dataset_select)

            if (isolate(input$correlation_variant == "vars_with_response")) {

                dataset <- inspection[["valid_datasets"]][[input$dataset_select]][["dataset"]]
                selection <- isolate(input$var_select_correlation)
                if ("All variables" %in% selection) selection <- NULL

                corr_list <- mvpa::correlation(dataset,
                                               response = isolate(input$response_select),
                                               correlate = "vars_w_response",
                                               selection = selection)

                output$correlation_plot <- plotly::renderPlotly({

                    filename <- paste(isolate(inspection$dataset_name), "correlation_vars_with_response", Sys.Date(), sep = "_")

                    fig <- mvpa::plot_correlation(corr_list,
                                                  rotate = input$rotate,
                                                  rel_font_size = input$rel_font_size)

                    plotly_helper(fig, filename)

                })

                inspection$correlation_df <- corr_list$correlation_matrix

            } else if (isolate(input$correlation_variant == "correlation_matrix")) {
                dataset <- inspection[["valid_datasets"]][[input$dataset_select]][["dataset"]]
                selection <- isolate(input$var_select_correlation)
                if ("All variables" %in% selection) selection <- NULL

                corr_list <- mvpa::correlation(dataset,
                                               response = isolate(input$response_select),
                                               correlate = "variables",
                                               selection = selection)

                output$correlation_plot <- plotly::renderPlotly({

                    filename <- paste(isolate(inspection$dataset_name), "correlation-matrix", Sys.Date(), sep = "_")

                    fig <- mvpa::plot_correlation(corr_list,
                                                  rotate = FALSE,
                                                  rel_font_size = input$rel_font_size)

                    plotly_helper(fig, filename)

                })

                inspection$correlation_df <- corr_list$correlation_matrix

            }

        })

        # Correlation dataframe

        observe({

            if (length(inspection$valid_datasets) == 0) return()
            if (is.null(inspection$correlation_df)) return()

            if (!input$df_switch_correlation) {
                output$correlation_df_ui <- renderUI({})
                return()
            }


            data <- inspection$correlation_df

            output$correlation_df_ui <- renderUI({

                tagList(
                    column(width = 12,
                           hr(),

                           downloadButton(NS(id, "download"),
                                          label = "Download table"),

                           actionButton(NS(id, "copy"),
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

        # Download & copy Target correlation data
        output$download <- downloadHandler(
            filename = function() {
                paste0(isolate(inspection$dataset_name),"_correlation_",
                       ifelse(isolate(input$correlation_variant) == "vars_with_response", "single-response_", "matrix_"),
                       Sys.Date(), ".", isolate(inspection$file_format))
            },
            content = function(file) {
                write_table(data = inspection$correlation_df,
                            format = isolate(inspection$file_format),
                            file = file)
            }
        )

        observeEvent(input$copy, {
            if (is.null(inspection$correlation_df)) return()

            clipr::write_clip(inspection$correlation_df,
                              allow_non_interactive = TRUE)

            showNotification("Correlation values have been copied to the clipboard",
                             type = "message")
        })

    })
}
