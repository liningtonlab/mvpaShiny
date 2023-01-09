covariateUI <- function(id) {

    ns <- NS(id)

    fluidPage(
        sidebarLayout(
            sidebarPanel(width = 3,

                         selectInput(inputId = NS(id, "dataset_select"),
                                     label = "Select valid dataset",
                                     choices = "",
                                     multiple = FALSE),

                         selectInput(inputId = NS(id, "var_select_covariate"),
                                     label = "Select covariate(s)",
                                     choices = "",
                                     multiple = TRUE),

                         checkboxInput(inputId = NS(id, "standardize"),
                                       label = "Standardize",
                                       value = FALSE),

                         actionButton(inputId = NS(id, "apply_covariate_projection"),
                                      label = "Perform covariate projection")

            ),

            # Dataframes
            mainPanel(width = 9,

                      tabsetPanel(type = "tabs",

                                  tabPanel(title = "Dataset after covariate projection",

                                           column(width = 12,

                                                  downloadButton(NS(id, "download_residual"),
                                                                 label = "Download table"),

                                                  actionButton(NS(id, "copy_residual"),
                                                               label = "Copy to clipboard"),

                                                  DT::DTOutput(outputId = NS(id, "covariate_projection_df")),

                                                  ),

                                           br(),

                                           uiOutput(NS(id, "residual_variance_df_options"))
                                  ),

                                  tabPanel(title = "Explained variance by covariate - Plot",
                                           value = "plot_tab",

                                           plotly::plotlyOutput(outputId = NS(id, "covariate_plot"),
                                                                width = "100%",
                                                                height = "100%"
                                                                ),

                                           uiOutput(outputId = NS(id, "explained_var_plot_options")),

                                           DT::DTOutput(outputId = NS(id, "explained_var_df"))
                                  )
                      )
            )
        )
    )

}
