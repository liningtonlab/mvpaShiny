downloadOptionsUI <- function(id) {

    ns <- NS(id)

    fluidPage(

             radioButtons(inputId = NS(id, "file_format"),
                          label = "Select table download format:",
                          choices = c("tab-delimited values (.tsv)" = "tsv",
                                      "comma-delimited values (.csv)" = "csv",
                                      "Excel table (.xlsx)" = "xlsx"),
                          selected = "tsv")

    )
}
