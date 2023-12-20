pcaUI <- function(id) {

    ns <- NS(id)

    fluidPage(
        sidebarLayout(
            sidebarPanel(width = 3,

                         selectInput(inputId = NS(id, "dataset_select"),
                                     label = "Select valid dataset",
                                     choices = "",
                                     multiple = FALSE),

                         selectInput(inputId = NS(id, "vars_to_remove"),
                                     label = "Ignore variables",
                                     choices = "",
                                     multiple = TRUE),

                         checkboxInput(inputId = NS(id, "standardize"),
                                                  label = "Standardize",
                                                  value = FALSE),

                        actionButton(inputId = NS(id, "apply_pca"),
                                                 label = "Perform principal component analysis"
                         ),

                        hr(),

                        sliderInput(inputId = NS(id, "rel_font_size"),
                                    label = "Change font size",
                                    min = 1,
                                    max = 3,
                                    value = 1,
                                    step = .1)
            ),

            mainPanel(width = 9,

                      tabsetPanel(type = "tabs",

                                  tabPanel(title = "Scree plot",

                                           plotly::plotlyOutput(outputId = NS(id, "scree_plot"),
                                                                width = "100%",
                                                                height = "100%"
                                                                ),
                                           hr(),

                                           uiOutput(outputId = NS(id, "scree_plot_options"))

                                           ),

                                  tabPanel(title = "Scores plot",

                                           plotly::plotlyOutput(outputId = NS(id, "scores_plot"),
                                                                width = "100%",
                                                                height = "100%"
                                                                ),
                                           hr(),

                                           uiOutput(outputId = NS(id, "scores_plot_options"))

                                           ),

                                  tabPanel(title = "Loadings plot",

                                           plotly::plotlyOutput(outputId = NS(id, "loadings_plot"),
                                                                width = "100%",
                                                                height = "100%"
                                                                ),

                                           hr(),

                                           uiOutput(outputId = NS(id, "loadings_plot_options"))

                                           ),

                                  tabPanel(title = "PCA values bar plot",

                                           plotly::plotlyOutput(outputId = NS(id, "pca_value_plot"),
                                                                width = "100%",
                                                                height = "100%"
                                                                ),

                                           hr(),

                                           uiOutput(outputId = NS(id, "pca_value_plot_options"))

                                           ),

                                  tabPanel(title = "Variable variation plot",

                                           plotly::plotlyOutput(outputId = NS(id, "var_variation_plot"),
                                                                width = "100%",
                                                                height = "100%"),

                                           hr(),

                                           uiOutput(outputId = NS(id, "var_variation_plot_options"))

                                           )
                      )

            )
        )
    )

}
