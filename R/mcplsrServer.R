mcplsrServer <- function(id, valid_datasets, selected_valid_dataset, current_data_obj_names, file_format) {

    ns <- NS(id)

    stopifnot(is.reactive(valid_datasets))
    stopifnot(is.reactive(selected_valid_dataset))
    stopifnot(is.reactive(current_data_obj_names))
    stopifnot((is.reactive(file_format)))

    moduleServer(id, function(input, output, session) {

        mcplsr <- reactiveValues(vars_to_select = NULL,
                                 valid_datasets = list(),
                                 selected_valid_dataset = NULL,
                                 file_format = "tsv",
                                 results = NULL,
                                 cl_range = c(.025, .975),
                                 tp_validated_val_type = "selectivity_ratio",
                                 sr_name = list("wTP" = "wTP",
                                                "pTP" = "pTP",
                                                "tTP" = "tTP",
                                                "selectivity_ratio" = "Selectivity ratio",
                                                "selectivity_fraction" = "Selectivity fraction",
                                                "MCorrC" = "Multivariate correlation coefficient",
                                                "MCovC" = "Multivariate covariance coefficient")
                                 )

        # Dataset and response selection
        observe({

            mcplsr$valid_datasets <- valid_datasets()
            mcplsr$selected_valid_dataset <- selected_valid_dataset()
            mcplsr$vars_to_select <- colnames(valid_datasets()[[input$dataset_select]][["dataset"]])
            mcplsr$file_format <- file_format()

        })

        observe({

            if (length(mcplsr$valid_datasets) == 0) return()

            updateSelectInput(session = session,
                              inputId = "response_select",
                              choices = mcplsr$vars_to_select,
                              selected = mcplsr[["valid_datasets"]][[input$dataset_select]][["response_var"]])

        })

        observe({

            updateSelectInput(session = session,
                              inputId = "dataset_select",
                              choices = names(mcplsr$valid_datasets),
                              selected = get_selected_dataset(mcplsr$selected_valid_dataset,
                                                              mcplsr$valid_datasets))

        })

        observeEvent(input$use_sd_full_dataset, {

            showNotification("This option is only recommended if PLS-model generation fails without it.", type = "warning")

        })

        # Monte Carlo + PLS-R calculation
        iv_mcplsr <- InputValidator$new()

        iv_mcplsr$add_rule("nr_components", sv_integer())

        iv_mcplsr$add_rule("seed", sv_integer())

        iv_mcplsr$add_rule("nr_repetitions", sv_integer())

        iv_mcplsr$add_rule("cal_ratio", sv_between(left = 0,
                                                   right = 100,
                                                   inclusive = c(FALSE, FALSE),
                                                   message_fmt = "Must be between {left} and {right} - Consider object count"))

        iv_mcplsr$add_rule("validation_threshold", sv_between(left = 0,
                                                 right = 1,
                                                 inclusive = c(FALSE, FALSE),
                                                 message_fmt = "Validation threshold must be between {left} and {right}"))

        iv_mcplsr$enable()

        # MC-PLS-R
        observeEvent(input$apply_mcplsr, {

            if (length(mcplsr$valid_datasets) == 0) return()

            if (!iv_mcplsr$is_valid()) {
                showNotification(
                    "Please fix input errors. Calculation will not proceed",
                    type = "error"
                )

                return()
            }

            # Collect parameter
            data <- mcplsr[["valid_datasets"]][[isolate(input$dataset_select)]][["dataset"]]
            response <- isolate(input$response_select)
            nr_components <- abs(isolate(input$nr_components))
            mc_bool <- isolate(input$mc_bool)
            standardize <- isolate(input$standardize)
            sd_full_dataset <- isolate(input$sd_full_dataset)
            use_seed <- isolate(input$use_seed)
            seed <- abs(isolate(input$seed))
            nr_repetitions <- abs(isolate(input$nr_repetitions))
            cal_ratio <- abs(isolate(input$cal_ratio)) / 100
            validation_threshold <- abs(isolate(input$validation_threshold))
            cost_function <- isolate(input$cost_function)

            if (use_seed) {
                set.seed(seed)
            }

            if (mc_bool) {
                calibration_sample_count <- trunc(nrow(data) * cal_ratio)

                if (calibration_sample_count <= 2) {
                    showNotification("Chosen cal_ratio results in less than 2 calibration / validation samples. Please adjust cal_ratio..
                                      Please choose a higher ratio.", type = "error")
                    return()
                }

                showNotification("Build PLS-R model, using Monte Carlo resampling.
                                 This may take a while")
            }

            if (nr_components > dim(data)[2]) {
                nr_components <- dim(data)[2] - 1
                showNotification(paste("Chosen number of components was greater than number of variables.
                                       Argument has been changed to number of variables:", nr_components))
            }

            # Disable button and wait until calculation is finished
            shinyjs::disable(id = "apply_mcplsr")

            mcplsr$results <- tryCatch(
                {
                    mvpa::perform_mc_pls_tp(data = data,
                                            response = response,
                                            nr_components = nr_components,
                                            standardize = standardize,
                                            use_sd_full_dataset = sd_full_dataset,
                                            mc_resampling = mc_bool,
                                            nr_repetitions = nr_repetitions,
                                            cal_ratio = cal_ratio,
                                            validation_threshold = validation_threshold,
                                            cost_function_type = cost_function)
                },
                    error = function(cond) {
                        showNotification(paste0(cond), duration = 20, type = "error")
                        return(NULL)

                },
                    finally = {
                        if (use_seed) set.seed(NULL)
                        shinyjs::enable(id = "apply_mcplsr")
                    }
            )

            # re-initialize seed after it has been used
            if (!is.null(mcplsr$results)) {
                mcplsr$results$dataset_name <- isolate(input$dataset_select)
                showNotification("PLS-R model has been built", type = "message")
            }

        })

        # Model info. text output
        observe({

            if (is.null(mcplsr$results)) return()

            if (mcplsr$results$A_optimal == 0) {

                output$model_info <- renderUI({

                    tagList(

                        br(),

                        HTML("The optimal number of components is 0. Target projection was not performed!"),

                        hr()
                    )

                })

                return()

            }

            expl_X <- as.numeric(round(mcplsr$results$explained_variance_in_X * 100, 2))
            expl_X_sum <- as.numeric(round(sum(mcplsr$results$explained_variance_in_X) * 100, 2))
            expl_y <- as.numeric(round(mcplsr$results$explained_variance_in_y * 100, 2))
            expl_y_sum <- as.numeric(round(sum(mcplsr$results$explained_variance_in_y) * 100, 2))
            expl_tTP <- as.numeric(round(mcplsr$results$explained_variance_tTP * 100, 2))
            components <- paste0(' (', seq(length(expl_X)), ' comp.)')

            text_component <- ifelse("tested_components" %in% names(mcplsr$results),
                                     "The optimal number of components is: ",
                                     "The selected number of components is: ")

            output$model_info <- renderUI({

                tagList(

                    br(),

                    HTML(paste0(text_component, mcplsr$results$A_optimal, "<br>")),
                    HTML(paste0("Explained variance in the explanatory variables (X): ", paste0(expl_X, "%", components, collapse = ", "), " (total: ", expl_X_sum,"%)", "<br>")),
                    HTML(paste0("Explained variance in the response (y): ", paste0(expl_y, "%", components, collapse = ", "), " (total: ", expl_y_sum, "%)", "<br>")),
                    HTML(paste0("Explained variance in the response (y) by the target score tTP: ", expl_tTP, "%", "<br>")),

                    hr()
                )

            })

        })

        # Plots
        # Model info: Optimal number of components
        observe({

            req(input$apply_mcplsr)
            if(is.null(mcplsr$results)) return()

            if (!("tested_components" %in% names(mcplsr$results))) {
                output$cost_function_plot_options <- renderUI({})
                return()
            }

            output$cost_function_plot_options <- renderUI({
                tagList(
                    column(width = 4,
                           radioButtons(inputId = NS(id, "bars_points_switch"),
                                        label = "Switch plots",
                                        choices = c("Fraction distribution plot" = "fd_plot",
                                                    "Classic view" = "classic"),
                                        selected = "fd_plot")

                           ),

                    column(width = 4,
                           sliderInput(inputId = NS(id, "rel_font_size_opt_number"),
                                       label = "Change font size",
                                       min = 1,
                                       max = 3,
                                       value = 1,
                                       step = .1)
                           )
                )
            })

        })

        observe({

            if (is.null(mcplsr$results)) return()

            if (!("tested_components" %in% names(mcplsr$results))) {
                output$cost_function_plot <- plotly::renderPlotly(
                    no_data_plot()
                )

                return()
            }
            req(input$rel_font_size_opt_number)
            if (!is.null(input$rel_font_size_opt_number)) {

                if (input$bars_points_switch == "classic") {

                    output$cost_function_plot <- plotly::renderPlotly({

                        filename <- paste(isolate(mcplsr$results$dataset_name), "MC-PLS-R-repeated-cost-function", Sys.Date(), sep = "_")

                        fig <- mvpa::plot_cost_function(mcplsr$results,
                                                        validation_threshold = isolate(input$validation_threshold),
                                                        show_confidence_limits = FALSE,
                                                        rel_font_size = input$rel_font_size_opt_number)

                        plotly_helper(fig, filename)

                        })

                } else {
                    output$cost_function_plot <- plotly::renderPlotly({

                        filename <- paste(isolate(mcplsr$results$dataset_name), "MC-PLS-R-repeated-cost-function-distribution", Sys.Date(), sep = "_")

                        fig <- mvpa::plot_cost_function_values_distribution(mcplsr$results,
                                                                            rel_font_size = input$rel_font_size_opt_number)


                        plotly_helper(fig, filename)

                        })
                }
            }

        })

        # Target projection - Monte Carlo resampling values. With confidence limits
        # Plot options
        observeEvent(input$apply_mcplsr, {

            if (is.null(mcplsr$results)) return()

            if (!("tested_components" %in% names(mcplsr$results)) | mcplsr$results$A_optimal == 0) {
                 output$tp_repeated_plot_options <- renderUI({})

                 return()
            }

            component <- paste0(isolate(mcplsr$results$A_optimal), "_component_model")

            data <- isolate(mcplsr[["results"]][["tested_components"]][[component]][["selectivity_ratio"]])
            data <- mvpa:::median_cl(data, pretty_column_names = TRUE)

            min_y <- min(abs(data[["Median"]]))
            max_y <- max(abs(data[["Median"]]))

            min_y <- floor(min_y)
            max_y <- ceiling(max_y)
            x_vals <- data$value_to_plot

            output$tp_repeated_plot_options <- renderUI({

                tagList(
                    column(width = 4,

                           sliderInput(inputId = NS(id, "component_select"),
                                       label = "Select model with:",
                                       min = 1,
                                       max = length(isolate(mcplsr$results$tested_components)),
                                       value = isolate(mcplsr$results$A_optimal),
                                       step = 1,
                                       post = " component(s)",
                                       ticks = FALSE),

                           radioButtons(inputId = NS(id, "tp_repeated_val_type"),
                                        label = "Choose target projection (TP) value",
                                        inline = FALSE,
                                        choiceNames = list(HTML("<b>TP weights</b> (wTP)"),
                                                           HTML("<b>TP loadings</b> (pTP)"),
                                                           HTML("<b>Selectivity ratio</b> (explained/<i>residual</i> variance of the predicted outcome)"),
                                                           HTML("<b>Selectivity fraction</b> (explained/<i>total</i> variance of the predicted outcome"),
                                                           HTML("<b>Multivariate correlation coefficient</b> (<i>standardized</i> association with the actual outcome)"),
                                                           HTML("<b>Multivariate covariance coefficient</b> (<i>unstandardized</i> association with the actual outcome)")),
                                        choiceValues = c("wTP",
                                                         "pTP",
                                                         "selectivity_ratio",
                                                         "selectivity_fraction",
                                                         "MCorrC",
                                                         "MCovC"),
                                        selected = "selectivity_ratio")
                           ),

                    column(width = 4,

                           selectizeInput(inputId = NS(id, "x_filter_repeated"),
                                          label = "Select variables",
                                          choices = x_vals,
                                          multiple = TRUE),

                           sliderInput(inputId = NS(id, "y_filter_repeated"),
                                       label = "Trim target projection value",
                                       min = min_y,
                                       max = max_y,
                                       value = c(min_y, max_y))

                        ),

                    column(width = 4,

                           sliderInput(inputId = NS(id, "cl_range_repeated"),
                                       label = "Confidence limits",
                                       min = 0,
                                       max = 100,
                                       value = c(2.5, 97.5),
                                       post = "%"),

                           sliderInput(inputId = NS(id, "rel_font_size_repeated"),
                                       label = "Change font size",
                                       min = 1,
                                       max = 3,
                                       value = 1,
                                       step = .1),

                           checkboxInput(inputId = NS(id, "df_switch_repeated"),
                                         label = "Show dataframe",
                                         FALSE),

                           checkboxInput(inputId = NS(id, "rotate_repeated"),
                                         label = "Rotate plot",
                                         FALSE)
                           ),
                    br(),

                    uiOutput(NS(id, "df_repeated_output"))
                )
            })

        })

        observe({

            req(input$tp_repeated_val_type)
            mcplsr$tp_repeated_val_type <- input$tp_repeated_val_type
            x_vals <- colnames(mcplsr$results$tested_components$component_1$selectivity_ratio)

            updateSelectizeInput(session = session,
                                 inputId = "x_filter_repeated",
                                 choices = x_vals,
                                 server = TRUE)

        })

        observe({

            if (is.null(mcplsr$results)) return()

            if("tested_components" %in% names(mcplsr$results)) {
                if(!is.null(input$tp_repeated_val_type)) {
                    component <- paste0(input$component_select, "_component_model")

                    data <- mcplsr[["results"]][["tested_components"]][[component]][[input$tp_repeated_val_type]]
                    data <- mvpa:::median_cl(data, pretty_column_names = TRUE)

                    min_y <- min(abs(data[["Median"]]))
                    max_y <- max(abs(data[["Median"]]))

                    min_y <- floor(min_y)
                    max_y <- ceiling(max_y)

                    updateSliderInput(session = session,
                                      inputId = "y_filter_repeated",
                                      min = min_y,
                                      max = max_y,
                                      value = c(min_y, max_y))

                }
            }

        }, priority = 2)

        observe({

            if (is.null(mcplsr$results)) return()

            if (!("tested_components" %in% names(mcplsr$results))) {
                output$tp_repeated_plot <- plotly::renderPlotly(
                    no_data_plot())

                return()
            }

            if (is.null(input$tp_repeated_val_type) | is.null(input$y_filter_repeated | is.null(input$x_filter_repeated) | is.null(input$rel_font_size_repeated))) return()

            filename = paste(isolate(mcplsr$results$dataset_name), "MC-PLS-R-repeated", input$tp_repeated_val_type, Sys.Date(), sep = "_")

            output$tp_repeated_plot <- plotly::renderPlotly({

                fig <- mvpa::plot_tp_value_mc(mcplsr$results,
                                              tp_value_to_plot = input$tp_repeated_val_type,
                                              component = input$component_select,
                                              y_filter = input$y_filter_repeated,
                                              x_filter = input$x_filter_repeated,
                                              confidence_limits = input$cl_range_repeated / 100,
                                              rel_font_size = input$rel_font_size_repeated,
                                              rotate = input$rotate_repeated)

                plotly_helper(fig, filename)
            })


        })

        # MC-PLSR Dataframe repeated
        observe({

            if(is.null(mcplsr$results)) return()

            if ("tested_components" %in% names(mcplsr$results)) {
                if (is.null(input$df_switch_repeated)) return()

                if (!input$df_switch_repeated) {
                    output$df_repeated_output <- renderUI({})
                    return()
                }
                component <- paste0(input$component_select, "_component_model")
                data <- mcplsr[["results"]][["tested_components"]][[component]][[mcplsr$tp_repeated_val_type]]
                data <- mvpa:::median_cl(data, pretty_column_names = TRUE)

                output$df_repeated_output <- renderUI({

                    tagList(
                        column(width = 12,
                               hr(),

                               downloadButton(NS(id, "download_repeated"),
                                              label = "Download table"),

                               actionButton(NS(id, "copy_repeated"),
                                            label = "Copy to clipboard"),

                               DT::renderDT({

                                   DT::datatable(adjust_decimals(data, ignore_column = 'Variable'),
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
            }

        })

        # Download & copy Target projection values (repeated analysis)
        output$download_repeated <- downloadHandler(
            filename = function() {
                paste0(isolate(mcplsr$results$dataset_name),"_", isolate(mcplsr$sr_name)[[isolate(mcplsr$tp_repeated_val_type)]], "_MC-PLSR_", Sys.Date(), ".", isolate(mcplsr$file_format))
            },
            content = function(file) {
                write_table(data = mvpa:::median_cl(mcplsr[["results"]][["tested_components"]]
                                                    [[paste0(isolate(input$component_select), "_component_model")]][[isolate(mcplsr$tp_repeated_val_type)]],
                                                    pretty_column_names = TRUE),
                            format = isolate(mcplsr$file_format),
                            file = file)
            }
        )

        observeEvent(input$copy_repeated, {

            if (is.null(mcplsr$results)) return()

            clipr::clear_clip()
            clipr::write_clip(mvpa:::median_cl(mcplsr[["results"]][["tested_components"]]
                                               [[paste0(isolate(input$component_select), "_component_model")]][[isolate(mcplsr$tp_repeated_val_type)]],
                                               pretty_column_names = TRUE),
                              allow_non_interactive = TRUE)

            showNotification(paste(isolate(mcplsr$tp_repeated_val_type), "values have been copied to the clipboard"),
                             type = "message")

        })

        # Target projection values plot (on full dataset, after validation, if Monte Carlo resampling was performed). Or if Monte Carlo is not selected.
        observe({

            if (is.null(mcplsr$results)) return()

            if (!is.null(mcplsr$results$target_projection_A_opt)) {
                min_y <- min(abs(mcplsr$results$target_projection_A_opt$selectivity_ratio))
                max_y <- max(abs(mcplsr$results$target_projection_A_opt$selectivity_ratio))

                min_y <- floor(min_y)
                max_y <- ceiling(max_y)

                x_vals <- rownames(mcplsr$results$target_projection_A_opt$selectivity_ratio)

                output$tp_validated_plot_options <- renderUI({

                    tagList(

                        column(width = 4,

                               radioButtons(inputId = NS(id, "tp_validated_val_type"),
                                            label = "Choose target projection (TP) value",
                                            inline = FALSE,
                                            choiceNames = list(HTML("<b>TP weights</b> (wTP)"),
                                                               HTML("<b>TP scores</b> (tTP)"),
                                                               HTML("<b>TP loadings</b> (pTP)"),
                                                               HTML("<b>Selectivity ratio</b> (explained/<i>residual</i> variance of the predicted outcome)"),
                                                               HTML("<b>Selectivity fraction</b> (explained/<i>total</i> variance of the predicted outcome"),
                                                               HTML("<b>Multivariate correlation coefficient 1</b> (<i>standardized</i> association with the actual outcome)"),
                                                               HTML("<b>Multivariate covariance coefficient 2</b> (<i>unstandardized</i> association with the actual outcome)")),
                                            choiceValues = c("wTP",
                                                             "tTP",
                                                             "pTP",
                                                             "selectivity_ratio",
                                                             "selectivity_fraction",
                                                             "MCorrC",
                                                             "MCovC"),
                                            selected = "selectivity_ratio")
                               ),

                        column(width = 4,

                               selectizeInput(inputId = NS(id, "x_filter"),
                                              label = "Select variables",
                                              choices = x_vals,
                                              multiple = TRUE),

                               sliderInput(inputId = NS(id, "y_filter"),
                                           label = "Trim target projection value",
                                           min = min_y,
                                           max = max_y,
                                           value = c(min_y, max_y),
                                           round = FALSE)

                               ),

                        column(width = 4,

                               sliderInput(inputId = NS(id, "rel_font_size_validated"),
                                           label = "Change font size",
                                           min = 1,
                                           max = 3,
                                           value = 1,
                                           step = .1),

                               checkboxInput(inputId = NS(id, "tp_validated_switch"),
                                             label = "Show dataframe",
                                             value = FALSE),

                               checkboxInput(inputId = NS(id, "rotate_validated"),
                                             label = "Rotate plot",
                                             value = FALSE),

                               actionButton(inputId = NS(id, "apply_tscores_transfer"),
                                            label = "Transfer T scores to current dataset")

                               ),
                        br(),

                        uiOutput(outputId = NS(id, "df_validated_output"))
                    )
                })
            }

        })

        observe({

            req(input$tp_validated_val_type)
            mcplsr$tp_validated_val_type <- input$tp_validated_val_type
            if (input$tp_validated_val_type == "tTP") {
                x_vals <- rownames(mcplsr$results$target_projection_A_opt$tTP)

                updateSelectizeInput(session = session,
                                     inputId = "x_filter",
                                     label = "Select objects",
                                     choices = x_vals,
                                     server = TRUE)
            } else {
                x_vals <- rownames(mcplsr$results$target_projection_A_opt$selectivity_ratio)

                updateSelectizeInput(session = session,
                                     inputId = "x_filter",
                                     label = "Select variables",
                                     choices = x_vals,
                                     server = TRUE)
            }

        })

        observe({

            if (is.null(mcplsr$results)) return()

            if (is.null(mcplsr$results$target_projection_A_opt)) return()

            if (is.null(input$tp_validated_val_type)) return()

            min_y <- min(abs(mcplsr[["results"]][["target_projection_A_opt"]][[input$tp_validated_val_type]]))
            max_y <- max(abs(mcplsr[["results"]][["target_projection_A_opt"]][[input$tp_validated_val_type]]))

            min_y <- floor(min_y)
            max_y <- ceiling(max_y)

            updateSliderInput(session = session,
                              inputId = "y_filter",
                              min = min_y,
                              max = max_y,
                              value = c(min_y, max_y))



        }, priority = 2)

        # Semi-interactive plotting in order to enhance speed
        # X filtering will be applied elsewhere
        observe({

            if (is.null(mcplsr$results)) return()

            if (is.null(input$tp_validated_val_type) & is.null(input$rel_font_size_validated) & is.null(input$rotate_validated)) return()

            if (mcplsr$results$A_optimal == 0) return()

            filename = paste(isolate(mcplsr$results$dataset_name), "MC-PLS-R-full-data", input$tp_validated_val_type, Sys.Date(), sep = "_")

            output$tp_validated_plot <- plotly::renderPlotly({

               fig <- mvpa::plot_tp_value(mcplsr$results,
                                          tp_value_to_plot = input$tp_validated_val_type,
                                          y_filter = input$y_filter,
                                          x_filter = input$x_filter,
                                          rel_font_size = input$rel_font_size_validated,
                                          rotate = input$rotate_validated
                                          )

               plotly_helper(fig,filename)


            })

        })

        # Send T scores to current dataset
        observeEvent(input$apply_tscores_transfer, {

            tscore_df <- as.data.frame(mcplsr$results$target_projection_A_opt$tTP)
            object_names_current_data <- current_data_obj_names()

            if (length(object_names_current_data) != length(rownames(tscore_df))) {
                showNotification("The number of objects between the T scores and the current dataset differs. Please revise current dataset!", type = "error")
                return()
            }

            if (!all(rownames(tscore_df) %in% object_names_current_data)) {
                showNotification("The object names between the T scores and the current data do not match. Please revise current dataset!", type = "error")
                return()
            }

            mcplsr$transferred_tscores <- list("counter" = as.numeric(input$apply_tscores_transfer), "data" = tscore_df)
            showNotification("T scores have been transferred to the current dataset.", type = "message")

        })

        # Dataframe PLS-R full dataset
        observe({

            if (is.null(mcplsr$results)) return()

            if (is.null(mcplsr$results$target_projection_A_opt)) {
                output$df_validated_output <- renderUI({})
                return()
            }
            if (is.null(input$tp_validated_switch)) return()

            if (input$tp_validated_switch) {
                data <- mcplsr[["results"]][["target_projection_A_opt"]][[mcplsr$tp_validated_val_type]]

                output$df_validated_output <- renderUI({

                    tagList(
                        column(width = 12,

                               hr(),

                               downloadButton(NS(id, "download_full_data"),
                                              label = "Download table"),

                               actionButton(NS(id, "copy_full_data"),
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

            } else {
                output$df_validated_output <- renderUI({})
            }


        })

        # Download & copy Target projection values from full dataset analysis
        output$download_full_data <- downloadHandler(
            filename = function() {
                paste0(isolate(mcplsr$results$dataset_name), "_", isolate(mcplsr$sr_name)[[isolate(mcplsr$tp_validated_val_type)]], "_PLS-R_", Sys.Date(), ".", isolate(mcplsr$file_format))
            },
            content = function(file) {
                write_table(data = mcplsr[["results"]][["target_projection_A_opt"]][[isolate(mcplsr$tp_validated_val_type)]],
                            format = isolate(mcplsr$file_format),
                            file = file)
            }
        )

        observeEvent(input$copy_full_data, {
            if (is.null(mcplsr$results)) return()

            clipr::clear_clip()
            clipr::write_clip(mcplsr[["results"]][["target_projection_A_opt"]][[isolate(mcplsr$tp_validated_val_type)]],
                              allow_non_interactive = TRUE)

            showNotification(paste(isolate(mcplsr$tp_validated_val_type), "values have been copied to the clipboard"),
                             type = "message")

        })


        # # Variable variance distribution plot

        # UI options

        observe({

            if (is.null(mcplsr$results)) return()

            if (is.null(mcplsr$results$target_projection_A_opt)) return()

            x_vals <- rownames(mcplsr$results$target_projection_A_opt$selectivity_ratio)

            output$var_variance_distribution_plot_options <- renderUI({

                tagList(

                    column(width = 6,

                           selectizeInput(inputId = NS(id, "x_filter_variance_distribution"),
                                          label = "Select variables",
                                          choices = x_vals,
                                          multiple = TRUE)
                    ),

                    column(width = 6,

                           sliderInput(inputId = NS(id, "rel_font_size_variance_distribution"),
                                       label = "Change font size",
                                       min = 1,
                                       max = 3,
                                       value = 1,
                                       step = .1),

                           checkboxInput(inputId = NS(id, "rotate_variance_distribution"),
                                         label = "Rotate plot",
                                         value = FALSE),

                           checkboxInput(inputId = NS(id, "variance_distribution_switch"),
                                         label = "Show dataframe",
                                         value = FALSE),

                    ),

                    br(),

                    uiOutput(outputId = NS(id, "df_variance_distribution_output"))
                )
            })

        })

        observe({

            x_vals <- rownames(mcplsr$results$target_projection_A_opt$selectivity_ratio)

            updateSelectizeInput(session = session,
                                 inputId = "x_filter_variance_distribution",
                                 label = "Select variables",
                                 choices = x_vals,
                                 server = TRUE)

        })


        # Plot
        observe({

            if (is.null(mcplsr$results)) return()

            if (is.null(input$rotate_variance_distribution) | is.null(input$rel_font_size_variance_distribution)) return()

            if (mcplsr$results$A_optimal == 0) return()

            filename <- paste(isolate(mcplsr$results$dataset_name), "MC-PLS-R-variable-variance-distribution", Sys.Date(), sep = "_")

            output$var_variance_distribution_plot <- plotly::renderPlotly({

                fig <- mvpa::plot_variable_variance_distribution(result_list = mcplsr$results,
                                                                 rotate = input$rotate_variance_distribution,
                                                                 x_filter = input$x_filter_variance_distribution,
                                                                 rel_font_size = input$rel_font_size_variance_distribution)

                plotly_helper(fig, filename)

            })

        })


        # Dataframe target projection variable variance distribution
        observe({

            if (is.null(mcplsr$results)) return()

            if (is.null(mcplsr$results$target_projection_A_opt)) {
                output$df_validated_output <- renderUI({})
                return()
            }

            if (is.null(input$variance_distribution_switch)) return()

            if (input$variance_distribution_switch) {
                data <- data.frame(mcplsr$results$target_projection_A_opt$explained_variance,
                                   mcplsr$results$target_projection_A_opt$residual_variance)

                output$df_variance_distribution_output <- renderUI({

                    tagList(
                        column(width = 12,

                               hr(),

                               downloadButton(NS(id, "download_variance_distribution"),
                                              label = "Download table"),

                               actionButton(NS(id, "copy_variance_distribution"),
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

            } else {
                output$df_variance_distribution_output <- renderUI({})
            }


        })

        # Download & copy target projection variable variance distribution values
        output$download_variance_distribution <- downloadHandler(
            filename = function() {
                paste0(isolate(mcplsr$results$dataset_name), "_MC-PLS-R-variable-variance-distribution_", Sys.Date(), ".", isolate(mcplsr$file_format))
            },
            content = function(file) {
                write_table(data = data.frame(mcplsr$results$target_projection_A_opt$explained_variance,
                                              mcplsr$results$target_projection_A_opt$residual_variance),
                            format = isolate(mcplsr$file_format),
                            file = file)
            }
        )

        observeEvent(input$copy_variance_distribution, {

            if (is.null(mcplsr$results)) return()

            clipr::clear_clip()
            clipr::write_clip(data.frame(mcplsr$results$target_projection_A_opt$explained_variance,
                                         mcplsr$results$target_projection_A_opt$residual_variance),
                              allow_non_interactive = TRUE)

            showNotification("Variable variance distribution values have been copied to the clipboard",
                             type = "message")

        })

        mcplsr

    })
}
