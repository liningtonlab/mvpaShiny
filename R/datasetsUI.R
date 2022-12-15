datasetsUI <- function(id) {

    ns <- NS(id)

    fluidPage(
        shinyjs::useShinyjs(),
        sidebarLayout(
            sidebarPanel(width = 3,

                         fileInput(
                             inputId = NS(id, "upload"),
                             label = "Load dataset",
                             accept = c(".csv", ".tsv", ".xls", ".xlsx")),

                         uiOutput(outputId = NS(id, "dataset_info")),

                         hr(style = "margin-top:5px; margin-bottom:5px"),

                         selectInput(inputId = NS(id, "df_functions"),
                                     label = "",
                                     choices = c("Select method",
                                                 "Transpose dataset" = "transpose",
                                                 "Select a subset of the current dataset" = "subset",
                                                 "Missing value methods" = "na_vals",
                                                 "Remove invariant variables" = "invariant",
                                                 "String value methods" = "string_vals",
                                                 "Introduce offset to avoid negative or zero values" = "offset_meth",
                                                 "Introduce unique variable combinations" = "combinations",
                                                 "Scaling / Normalization methods" = "scale_meth")),

                         conditionalPanel(
                             condition = "input.df_functions == 'transpose'", ns = ns,

                             actionButton(inputId = NS(id, "apply_transpose"),
                                          label = "Apply")
                         ),

                         conditionalPanel(
                             condition = "input.df_functions == 'subset'", ns = ns,

                             selectizeInput(inputId = NS(id, "select_var_subset"),
                                            label = "Select variable for subsetting",
                                            choices = ""),

                             uiOutput(outputId = NS(id, "subset_ui"))
                         ),

                         conditionalPanel(
                             condition = "input.df_functions == 'na_vals'", ns = ns,

                             radioButtons(inputId = NS(id, "remove_or_impute"),
                                          label = "Remove or impute missing values",
                                          inline = TRUE,
                                          choices = c("Impute" = "impute",
                                                      "Remove variables" = "remove_na_vars",
                                                      "Remove objects" = "remove_na_objs")
                                          ),

                             conditionalPanel(
                                 condition = "input.remove_or_impute == 'impute'", ns = ns,

                                 radioButtons(inputId = NS(id, "impute_choice"),
                                              label = "Missing value impute method",
                                              inline =TRUE,
                                              choices = c("Variable mean" = "mean",
                                                          "Variable median" = "median",
                                                          "Custom value" = "custom"),
                                              selected = "mean"),

                                 conditionalPanel(
                                     condition = "input.impute_choice == 'custom'", ns = ns,

                                     numericInput(inputId = NS(id, "impute_custom"),
                                                  label = "Enter imputation value",
                                                  value = 0)

                                 ),

                                 actionButton(inputId = NS(id, "apply_impute"),
                                              label = "Apply")

                                 ),
                             conditionalPanel(
                                 condition = "input.remove_or_impute == 'remove_na_vars'", ns = ns,

                                 actionButton(inputId = NS(id, "apply_remove_na_vars"),
                                              label = "Apply")

                             ),
                             conditionalPanel(
                                 condition = "input.remove_or_impute == 'remove_na_objs'", ns = ns,

                                 actionButton(inputId = NS(id, "apply_remove_na_objs"),
                                              label = "Apply")
                             )
                         ),

                         conditionalPanel(
                             condition = "input.df_functions == 'string_vals'", ns = ns,

                             checkboxInput(inputId = NS(id, "add_indicator"),
                                           label = "Indicate which level means '1'",
                                           value = FALSE),

                             actionButton(inputId = NS(id, "apply_strings_to_binary"),
                                          label = "Apply")
                         ),

                         conditionalPanel(
                             condition = "input.df_functions == 'invariant'", ns = ns,

                             HTML("<b>Current invariant variables</b>"),

                             hr(),

                             uiOutput(outputId = NS(id, "invariant_text")),

                             hr(),

                             actionButton(inputId = NS(id, "apply_remove_invariants"),
                                          label = "Apply")
                         ),

                         conditionalPanel(
                             condition = "input.df_functions == 'offset_meth'", ns = ns,

                             numericInput(inputId = NS(id, "offset"),
                                          label = "Set new minimum value",
                                          value = 0),

                             actionButton(inputId = NS(id, "apply_offset"),
                                          label = "Apply")
                         ),

                         conditionalPanel(
                             condition = "input.df_functions == 'scale_meth'", ns = ns,

                             radioButtons(inputId = NS(id, "scale_choice"),
                                          label = "Select function",
                                          choices = c("log10" = "log10",
                                                      "log2" = "log2",
                                                      "ln" = "ln",
                                                      "nth root" = "nth_root",
                                                      "Standardization" = "sd",
                                                      "Normalize to max" = "normalize_to_max",
                                                      "Min-max scaling" = "min_max"),
                                          selected = "log10"),

                             conditionalPanel(
                                 condition = "input.scale_choice == 'nth_root'", ns = ns,

                                 numericInput(inputId = NS(id, "nth_root_nr"),
                                              label = "Choose n (default 2 -> square root)",
                                              value = 2)

                             ),
                             div(style = "display:inline-block",
                                 checkboxInput(inputId = NS(id, "ignore_response"),
                                               label = "Ignore response variable",
                                               value = FALSE)),

                             div(style = "display:inline-block",
                                 actionButton(inputId = NS(id, "apply_scale"),
                                              label = "Apply"))

                         ),

                         conditionalPanel(
                             condition = "input.df_functions == 'combinations'", ns = ns,

                             radioButtons(inputId = NS(id, "combinations_method"),
                                          label = "Select method:",
                                          choices = c("Multiplication ( * )" = "*",
                                                      "Addition ( + )" = "+",
                                                      "Substraction ( - )" = "-",
                                                      "Division ( / )" = "/"),
                                          selected = "*"),

                             checkboxInput(inputId = NS(id, "ignore_response_combinations"),
                                           label = "Ignore response",
                                           value = TRUE),

                             actionButton(inputId = NS(id, "apply_combinations"),
                                          label = "Apply")
                         ),

                         hr(style = "margin-top:15px; margin-bottom:5px"),

                         textInput(inputId = NS(id, "new_dataset_name"),
                                   label = "Name of new dataset"),

                         div(style = "display:inline-block;",
                             selectizeInput(inputId = NS(id, "response_var"),
                                            label = "Select response",
                                            choices = "")),

                         div(style = "display:inline-block;",
                             actionButton(inputId = NS(id, "add_valid_df"),
                                          label = "Verify and store current dataset",
                                          style = 'margin-bottom:25px')),

                         hr(style = "margin-top:5px; margin-bottom:5px"),

                         selectInput(inputId = NS(id, "selected_valid_dataset"),
                                     label = "Available valid datasets",
                                     choices = "",
                                     multiple = FALSE,
                                     selectize = TRUE),

                         actionButton(inputId = NS(id, "use_as_current"),
                                      label = "Use as current dataset"),

                         checkboxInput(inputId = NS(id, "delete_bool"),
                                       label = "Consider dataset deletion"),

                         conditionalPanel(
                             condition = "input.delete_bool", ns = ns,

                                 actionButton(inputId = NS(id, "delete_apply"),
                                              label = "Confirm deletion")

                         ),

                         hr(),

                         actionButton(inputId = NS(id, "load_demo_data"),
                                      label = "Load demo dataset")

            ),

            # Dataframes
            mainPanel(width = 9,

                      tabsetPanel(type = "tabs",
                                  tabPanel("Current dataset",

                                           column(width = 12,
                                                  downloadButton(NS(id, "download_current"),
                                                                 label = "Download table"),

                                                  actionButton(NS(id, "copy_current"),
                                                               label = "Copy to clipboard"),

                                                  uiOutput(NS(id, "info_text")),

                                                  DT::DTOutput(outputId = NS(id, "current_df")),

                                           ),

                                           column(width = 12,
                                                  style = "padding: 0px",

                                                  hr(style = "margin-top:5px; margin-bottom:5px"),

                                                  div(style="display:inline-block; float:left",

                                                      uiOutput(outputId = NS(id, "chooser_vars_ui"))

                                                      ),

                                                  div(style="display:inline-block; float:right",

                                                      uiOutput(outputId = NS(id, "chooser_objs_ui"))

                                                      )
                                           ),

                                           column(width = 12,
                                                  style = "padding: 0px",

                                                  hr(style = "margin-top:10px; margin-bottom:10px"),

                                                  div(style = "display:inline-block",
                                                      actionButton(inputId = NS(id, "apply_removal"),
                                                                   label = "Remove")),

                                                  div(style = "display:inline-block",
                                                      actionButton(inputId = NS(id, "apply_keep"),
                                                                   label = "Keep")),

                                                  div(style = "display:inline-block",
                                                      actionButton(inputId = NS(id, "undo"),
                                                                   label = "Undo last operation")),


                                                  div(style = "display:inline-block; float:right",
                                                      actionButton(inputId = NS(id, "reset_df"),
                                                                   label = "Reset dataset"))
                                           ),

                                  ),
                                  tabPanel("Valid dataset",

                                           column(width = 12,
                                                  downloadButton(NS(id, "download_valid"),
                                                                 label = "Download table"),

                                                  actionButton(NS(id, "copy_valid"),
                                                               label = "Copy to clipboard"),

                                                  # uiOutput(outputId = NS(id, "copy_valid")),

                                                  DT::DTOutput(NS(id, "valid_df")),
                                           ),

                                           column(width = 12,
                                                  style = "padding: 0px",

                                                  div(style = "float:left",
                                                      checkboxInput(inputId = NS(id, "stats"),
                                                                    label = "Show statistics",
                                                                    value = FALSE))
                                           ),

                                           column(width = 12,
                                                  style = "padding: 0px",

                                                  hr(),
                                                  uiOutput(outputId = NS(id, "stats_output"))
                                           )
                                  )

                      )
            )
        )
    )
}
