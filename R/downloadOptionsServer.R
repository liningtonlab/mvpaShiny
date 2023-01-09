downloadOptionsServer <- function(id) {

    ns <- NS(id)

    moduleServer(id, function(input, output, session) {

        download_options <- reactiveValues(file_format = "tsv")

        observeEvent(input$file_format, {
            download_options$file_format <- input$file_format
        })

        download_options

    })
}
