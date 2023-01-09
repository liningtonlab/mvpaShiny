inspectionsUI <- function(id) {

    ns <- NS(id)

    fluidPage(
        sidebarLayout(
            sidebarPanel(width = 3,

                         selectInput(inputId = NS(id, "dataset_select"),
                                     label = "Select valid dataset",
                                     choices =  "",
                                     multiple = FALSE),

                         hr(),

                         sliderInput(inputId = NS(id, "rel_font_size"),
                                     label = "Change font size",
                                     min = 1,
                                     max = 3,
                                     value = 1,
                                     step = .1)

            ),

            # Dataframes
            mainPanel(width = 9,

                      tabsetPanel(type = "tabs",

                                  tabPanel(title = "Normality check / QQ-plot",


                                          plotly::plotlyOutput(outputId = NS(id, "qq_plot"),
                                                               width = "100%",
                                                               height = "100%"
                                                              ),

                                          hr(),

                                          uiOutput(outputId = NS(id, "qq_plot_options"))
                                  ),

                                  tabPanel(title = "Variable correlation",

                                           plotly::plotlyOutput(outputId = NS(id, "correlation_plot"),
                                                                width = "100%",
                                                                height = "100%"
                                           ),

                                           hr(),

                                           uiOutput(outputId = NS(id, "correlation_options"))

                                  )
                      )
            )
        )
    )
}
