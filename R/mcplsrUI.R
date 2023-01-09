mcplsrUI <- function(id) {

    ns <- NS(id)

    fluidPage(
        sidebarLayout(
            sidebarPanel(width = 3,

                         selectInput(inputId = NS(id, "dataset_select"),
                                     label = "Select valid dataset",
                                     choices = "",
                                     multiple = FALSE),

                         hr(style = "margin-top:5px; margin-bottom:5px"),

                         selectInput(inputId = NS(id, "response_select"),
                                     label = "Select reponse",
                                     choices = "",
                                     multiple = FALSE),

                         numericInput(inputId = NS(id, "nr_components"),
                                      label = "Select number of components",
                                      value = 6),

                         div(style = "display:inline-block",
                             actionButton(inputId = NS(id, "apply_mcplsr"),
                                          label = "Perform PLS-R")),

                         div(style = "display: inline-block; float:right",
                             checkboxInput(inputId = NS(id, "mc_bool"),
                                           label = "Monte Carlo resampling",
                                           value = TRUE)),

                         checkboxInput(inputId = NS(id, "standardize"),
                                       label = "Standardize",
                                       value = FALSE),

                         hr(style = "margin-top:15px; margin-bottom:5px"),

                         conditionalPanel(
                                condition = "input.mc_bool", ns = ns,

                                HTML("<b>Monte Carlo resampling options"),

                                br(),

                                div(style = "display:inline-block",
                                    checkboxInput(inputId = NS(id, "use_seed"),
                                                  label = "Use seed for reproducibility",
                                                  value = FALSE)),

                                div(style = "display:inline-block; margin-bottom:0px",
                                    conditionalPanel(
                                        condition = "input.use_seed", ns = ns,
                                         numericInput(inputId = NS(id, "seed"),
                                                      label = "",
                                                      value = 5,
                                                      width = 75))
                                ),

                                conditionalPanel(
                                    condition = "input.standardize", ns = ns,

                                    checkboxInput(inputId = NS(id, "sd_full_dataset"),
                                                  label = "Standard deviation based on full dataset",
                                                  value = FALSE)

                                ),

                                numericInput(inputId = NS(id, "nr_repetitions"),
                                             label = "Number of repetitions",
                                             value = 100,
                                             step = 100),

                                numericInput(inputId = NS(id, "cal_ratio"),
                                             label = "Proportion of objects in calibration dataset (%)",
                                             value = 50,
                                             step = 5),

                                numericInput(inputId = NS(id, "validation_threshold"),
                                             label = "Validation threshold",
                                             value = 0.5,
                                             step = 0.05),

                                radioButtons(inputId = NS(id, "cost_function"),
                                             label = "Cost function",
                                             choices = c("Root-mean-square error of prediction (RMSEP)" = "rmsep",
                                                         "Mean absolute error (MAE)" = "mae"))
                        )
            ),

            # Dataframes
            mainPanel(width = 9,

                      tabsetPanel(type = "tabs", id = NS(id, "tabset"),

                                  tabPanel(title = "Model information",

                                           uiOutput(outputId = NS(id, "model_info")),

                                           plotly::plotlyOutput(NS(id, "cost_function_plot"),
                                                                width = "100%",
                                                                height = "100%"
                                                                ),

                                           hr(),

                                           uiOutput(outputId = NS(id, "cost_function_plot_options"))
                                  ),
                                  tabPanel(title = "Target projection - Repeated sampling",

                                           plotly::plotlyOutput(outputId = NS(id, "tp_repeated_plot"),
                                                                width = "100%",
                                                                height = "100%"
                                                                ),

                                           hr(),

                                           uiOutput(outputId = NS(id, "tp_repeated_plot_options"))
                                  ),

                                  tabPanel(title = "Target projection - Full dataset",

                                           plotly::plotlyOutput(outputId = NS(id, "tp_validated_plot"),
                                                                width = "100%",
                                                                height = "100%"
                                                                ),

                                           hr(),

                                           uiOutput(outputId = NS(id, "tp_validated_plot_options"))

                                  ),

                                  tabPanel(title = "Target projection - Variable variance distribution",

                                           plotly::plotlyOutput(outputId = NS(id, "var_variance_distribution_plot"),
                                                                width = "100%",
                                                                height = "100%"
                                                                ),

                                           hr(),

                                           uiOutput(outputId = NS(id, "var_variance_distribution_plot_options"))
                                  )
                      )
            )

        )
    )

}
