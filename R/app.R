#' Starts mvpa.shiny
#'
#' @param run_in_browser Shall mvpa.shiny be opened in the default web browser (default = TRUE).
#'
#' @import shiny
#' @import shinyvalidate
#'
#' @export
mvpaApp <- function(run_in_browser = TRUE) {

    ui <- navbarPage(" mvpa - multivariate pattern analysis", id="nav",

                     tabPanel(title = "Datasets",
                              datasetsUI("datasets"),
                              icon = icon("box-archive") # Those icon names might be deprecated in future shiny releases
                              ),

                     tabPanel(title = "Download options",
                              downloadOptionsUI("download"),
                              icon = icon("file-export")
                     ),

                     tabPanel(title = "Inspection",
                              inspectionsUI("inspection"),
                              icon = icon("magnifying-glass")
                              ),

                     tabPanel(title = "Principal component analysis",
                              pcaUI("pca"),
                              icon = icon("border-none")
                              ),

                     tabPanel(title = "Covariate projection",
                              covariateUI("covariate"),
                              icon = icon("diagram-project")
                              ),

                     tabPanel(title = "PLS-R / Selectivty Ratios",
                              mcplsrUI("mcplsr"),
                              icon = icon("chart-bar")
                              ),

                     tabPanel(title=HTML("</a></li><li><a href='https://tim-b90.github.io/mvpaShiny_documentation/' target='_blank'><b>&#128214; Online documentation</b>"))
                     )


    server <- function(input, output, session) {

        storage <- reactiveValues("valid_datasets" = list(),
                                  "selected_valid_dataset" = NULL,
                                  "update_current_data" = list(),
                                  "enhance_current_data" = list(),
                                  "current_data_obj_names" = c(),
                                  "file_format" = "tsv")

        # Dataset handling
        dataset_handling <- datasetsServer(id = "datasets",
                                           update_current_data = reactive(storage$update_current_data),
                                           enhance_current_data = reactive(storage$enhance_current_data),
                                           file_format = reactive(storage$file_format))

        observe({

            storage$valid_datasets <- dataset_handling$valid_datasets
            storage$selected_valid_dataset <- dataset_handling$selected_valid_dataset
            storage$current_data_obj_names <- dataset_handling$current_data_obj_names

        })

        # Table download options
        download_options <- downloadOptionsServer(id = "download")

        observe({

            storage$file_format <- download_options$file_format

        })

        # Dataset inspection
        inspectionsServer(id = "inspection",
                          valid_datasets = reactive(storage$valid_datasets),
                          selected_valid_dataset = reactive(storage$selected_valid_dataset),
                          file_format = reactive(storage$file_format))

        # Principle component analysis
        pca_results <- pcaServer(id = "pca",
                                 valid_datasets = reactive(storage$valid_datasets),
                                 selected_valid_dataset = reactive(storage$selected_valid_dataset),
                                 current_data_obj_names = reactive(storage$current_data_obj_names),
                                 file_format = reactive(storage$file_format))

        observe({

            storage$enhance_current_data <- pca_results$transferred_pcs

        })

        # Covariate projection
        covariate_results <- covariateServer(id = "covariate",
                                             valid_datasets = reactive(storage$valid_datasets),
                                             selected_valid_dataset = reactive(storage$selected_valid_dataset),
                                             file_format = reactive(storage$file_format))

        observe({

            storage$update_current_data <- covariate_results$new_dataset

        })

        # Monte Carlo PLS-R
        mcplsr_results <- mcplsrServer(id = "mcplsr",
                          valid_datasets = reactive(storage$valid_datasets),
                          selected_valid_dataset = reactive(storage$selected_valid_dataset),
                          current_data_obj_names = reactive(storage$current_data_obj_names),
                          file_format = reactive(storage$file_format))

        observe({

            storage$enhance_current_data <- mcplsr_results$transferred_tscores

        })

    }

    # shinyApp(ui, server)
    runApp(list(ui = ui, server = server), launch.browser = run_in_browser)
}

