datasetsServer <- function(id, update_current_data, enhance_current_data, file_format) {

    ns <- NS(id)

    stopifnot(is.reactive(update_current_data))
    stopifnot(is.reactive(enhance_current_data))
    stopifnot((is.reactive(file_format)))

    moduleServer(id, function(input, output, session) {

        # Reactive values
        datasets <- reactiveValues("current_data" = NULL,
                                  "file_format" = "tsv",
                                  "undo_df" = NULL,
                                  "valid_datasets" = list(),
                                  "selected_valid_dataset" = NULL,
                                  "dtype" = NULL,
                                  "stats" = NULL,
                                  "current_data_obj_names" = NULL,
                                  "enhance_current_data" = NULL)

        # Export options
        observe({

            datasets$file_format <- file_format()

        })

        # Chooser inputs
        observe({

            if (is.null(datasets$current_data)) return()

            output$chooser_vars_ui <- renderUI({

                tagList(

                    HTML("<b>Variables</b>"),

                    chooserInput(inputId = NS(id, "chooser_vars"),
                                 leftLabel = "unselected",
                                 rightLabel = "selected",
                                 leftChoices = colnames(datasets$current_data),
                                 rightChoices = c(),
                                 size = 5,
                                 multiple = TRUE)

                )

            })

            output$chooser_objs_ui <- renderUI({

                tagList(

                    HTML("<b>Objects</b>"),

                    chooserInput(inputId = NS(id, "chooser_objs"),
                                 leftLabel = "unselected",
                                 rightLabel = "selected",
                                 leftChoices = rownames(datasets$current_data),
                                 rightChoices = c(),
                                 size = 5,
                                 multiple = TRUE)

                )

            })

        })

        # Info text in case no dataset has been loaded yet
        observe({
            if (is.null(datasets$current_data)) {
                output$info_text <- renderUI({
                    tagList(
                            hr(),
                            HTML(
                            "<p><h3><b>How to work with mvpaShiny</b></p>
                            <p>(1) Load your own dataset or the demo dataset</p>
                            <p>(optional) Scale, transform or subset the dataset</p>
                            <p>(2) Name it</p>
                            <p>(3) Select the response variable</p>
                            <p>(4) Validate its integrity and make it an available dataset</p>
                            <p>(5) Apply the available methods (tabs) to the dataset</p></h3>"
                            ),

                            hr()
                        )

                    })
            } else {
                output$info_text <- renderUI({})
            }
        })

        # Dataframe CSV upload
        observeEvent(input$upload,{

            tryCatch({
                file_extension <- tools::file_ext(input$upload$datapath)
                if (file_extension == "tsv" | file_extension == "csv") {
                    temp_df <- suppressMessages(
                        vroom::vroom(input$upload$datapath,
                                     show_col_types = FALSE) %>%
                        data.frame(check.names = TRUE)
                    )
                } else if (file_extension == "xlsx" | file_extension == "xls") {
                    temp_df <- suppressMessages(
                        readxl::read_excel(input$upload$datapath) %>%
                            data.frame(check.names = TRUE)
                    )
                } else {
                    showNotification("Selected file format is not supported", type = "error", duration = 15)
                    return()
                }

                temp_df <- data.frame(temp_df[,-1], row.names = temp_df[,1])

                datasets$original_data <- temp_df
                datasets$current_data <- datasets$original_data
                datasets$undo_df <- NULL
            },
                error = function(e){

                    showNotification('Selected file could not be processed (see error message) - Please upload another file', type = 'warning', duration = 10)
                    showNotification(paste0(e), type = "error", duration = 25)


            })

        })

        # Current dataset info
        observe({

            if (!is.null(datasets$current_data)) {
                output$dataset_info <- renderUI(
                    HTML("Dataset info: ", ncol(datasets$current_data)," Variables | ", nrow(datasets$current_data), "Objects")
                )
            }

        })

        # # Sidebar panel methods

        # Transpose dataset
        observeEvent(input$apply_transpose, {

            if (is.null(datasets$current_data)) return()

            temp_df <- as.data.frame(t(datasets$current_data))
            temp_df <- utils::type.convert(temp_df, as.is = TRUE)

            datasets$current_data <- temp_df

        })

        # Subsetting the current dataset
        observe({

            if (is.null(datasets$current_data)) return()

            updateSelectizeInput(session = session,
                                 inputId = "select_var_subset",
                                 choices = colnames(datasets$current_data),
                                 server = TRUE)

        })

        observe({

            if (is.null(datasets$current_data)) return()
            if (is.null(input$select_var_subset)) return()

            datasets$dtype <- class(datasets$current_data[[input$select_var_subset]])

            output$subset_ui <- renderUI({

                if (datasets$dtype == "character") {

                    tagList(
                        column(width = 12, style = "padding: 0px; margin:0px",

                               radioButtons(inputId = NS(id, "subset_char_keep_or_remove"),
                                            label = "Keep or remove levels?",
                                            choices = c("Keep" = "keep",
                                                        "Remove" = "remove"),
                                            selected = "keep",
                                            inline = TRUE),

                               selectizeInput(inputId = NS(id, "subset_char_level_select"),
                                              label = "Select levels",
                                              choices = "",
                                              multiple = TRUE),

                               div(style = "margin-bottom:15px",
                                   actionButton(inputId = NS(id, "apply_subset"),
                                                label = "Create subset"))

                               )
                    )

                } else if (datasets$dtype == "numeric" | datasets$dtype == "integer") {

                    tagList(
                        column(width = 12, style = "padding:0px",

                               radioButtons(inputId = NS(id, "subset_numeric_condition"),
                                            label = "Keep if value is:",
                                            choices = c("<" = "smaller",
                                                        ">" = "greater",
                                                        "<=" = "smaller_equal",
                                                        ">=" = "greater_equal",
                                                        "=" = "equal",
                                                        "not equal to" = "unequal",
                                                        "keep [ ] to [ ]" = "between",
                                                        "exclude [ ] to [ ]" = "not_between"
                                                        ),
                                            selected = "equal",
                                            inline = FALSE),



                               conditionalPanel(
                                   condition = "input.subset_numeric_condition != 'between' && input.subset_numeric_condition != 'not_between'", ns = ns,

                                   numericInput(inputId = NS(id, "subset_numeric_number"),
                                                label = "Value",
                                                value = 0),

                                   column(width = 6, style = "padding:0px",

                                          checkboxInput(inputId = NS(id, "use_stats_value"),
                                                        label = "Use descriptive statistics",
                                                        value = FALSE)
                                   ),

                                   column(width = 6, style = "padding:0px",

                                          conditionalPanel(
                                              condition = "input.use_stats_value", ns = ns,


                                              div(style = "margin-bottom:0px",
                                                  selectInput(inputId = NS(id, "subset_descr_statistic"),
                                                              label = "Choose statistic",
                                                              choices = c("Mean" = "mean",
                                                                          "Median" = "median"),
                                                              selected = "median"))
                                          )
                                   ),

                               ),

                               conditionalPanel(
                                   condition = "input.subset_numeric_condition == 'between' || input.subset_numeric_condition == 'not_between'", ns = ns,

                                   column(width = 6, style = "padding-left:0px",

                                   numericInput(inputId = NS(id, "subset_numeric_number_left"),
                                                label = "Smaller value",
                                                value = 0)

                                   ),

                                   column(width = 6, style = "padding-right:0px",

                                   numericInput(inputId = NS(id, "subset_numeric_number_right"),
                                                label = "Greater value",
                                                value = 1)

                                   ),

                               ),

                              column(width = 12, style = "padding:0px",

                                   div(style = "margin-bottom:15px",
                                       actionButton(inputId = NS(id, "apply_subset"),
                                                    label = "Create subset"))
                              )

                        )

                    )

                } else {
                    return()
                }

            })

        })

        observe({

            if (is.null(datasets$dtype)) return()
            if (datasets$dtype != "character") return()

            levels <- unique(datasets$current_data[[input$select_var_subset]])

            updateSelectizeInput(session = session,
                                 inputId = "subset_char_level_select",
                                 choices = levels,
                                 server = TRUE)

        })

        observe({

            if (is.null(input$use_stats_value)) return()

            if (input$use_stats_value) {
                value <- switch(input$subset_descr_statistic,

                                mean = mean(datasets$current_data[[input$select_var_subset]], na.rm = TRUE),

                                median = stats::median(datasets$current_data[[input$select_var_subset]], na.rm = TRUE)
                                )

                updateNumericInput(inputId = "subset_numeric_number",
                                   value = value)
            }
        })

        #initiate
        iv_subset <- InputValidator$new()

        # rules
        iv_subset$add_rule("subset_numeric_number", sv_numeric())
        iv_subset$add_rule("subset_numeric_number_left", sv_numeric())
        iv_subset$add_rule("subset_numeric_number_right", sv_numeric())

        # enable
        iv_subset$enable()

        observe({
            req(input$subset_numeric_number_left)
            req(input$subset_numeric_number_right)

            if (input$subset_numeric_number_left > input$subset_numeric_number_right) {

                updateNumericInput(inputId = "subset_numeric_number_left",
                                   value = input$subset_numeric_number_right)

                updateNumericInput(inputId = "subset_numeric_number_right",
                                   value = input$subset_numeric_number_left)
            }

        })

        observeEvent(input$apply_subset, {

            if (is.null(datasets$current_data)) return()

            if (!iv_subset$is_valid()) {
                showNotification(
                    "Please fix input errors. Calculation will not proceed.",
                    type = "error"
                )

                return()
            }

            if (datasets$dtype == "character") {
                levels = isolate(input$subset_char_level_select)
                if (is.null(levels)) return()

                datasets$undo_df <- datasets$current_data

                subset <- switch(isolate(input$subset_char_keep_or_remove),

                                 remove = datasets$current_data %>%
                                          dplyr::filter(!(.data[[isolate(input$select_var_subset)]]) %in% levels),

                                 keep = datasets$current_data %>%
                                        dplyr::filter(.data[[isolate(input$select_var_subset)]] %in% levels)

                                 )

                datasets$current_data <- subset

            } else {
                number <- isolate(input$subset_numeric_number)
                number_left <- isolate(input$subset_numeric_number_left)
                number_right <- isolate(input$subset_numeric_number_right)

                if (is.null(number)) return()

                datasets$undo_df <- datasets$current_data

                subset <- switch(isolate(input$subset_numeric_condition),

                                 smaller = datasets$current_data %>%
                                           dplyr::filter(.data[[isolate(input$select_var_subset)]] < number),

                                 smaller_equal = datasets$current_data %>%
                                                 dplyr::filter(.data[[isolate(input$select_var_subset)]] <= number),

                                 equal = datasets$current_data %>%
                                         dplyr::filter(.data[[isolate(input$select_var_subset)]] == number),

                                 unequal = datasets$current_data %>%
                                             dplyr::filter(.data[[isolate(input$select_var_subset)]] != number),

                                 between = datasets$current_data %>%
                                           dplyr::filter(dplyr::between(.data[[isolate(input$select_var_subset)]], number_left, number_right)),

                                 not_between = datasets$current_data %>%
                                               dplyr::filter(!(.data[[isolate(input$select_var_subset)]] %in% number_left:number_right)),

                                 greater_equal = datasets$current_data %>%
                                                 dplyr::filter(.data[[isolate(input$select_var_subset)]] >= number),

                                 greater = datasets$current_data %>%
                                           dplyr::filter(.data[[isolate(input$select_var_subset)]] > number),

                                 )

                datasets$current_data <- subset
            }

        })


        # Missing vals / imputation methods
        observeEvent(input$apply_impute, {

            if (is.null(datasets$current_data)) return()

            if (identical(input$chooser_vars$right, character(0))) {
                test_df <- datasets$current_data
            } else {
                test_df <- as.data.frame(datasets$current_data[, input$chooser_vars$right])
            }

            checklist <- check_dataset(test_df, specific_check = "empty")
            checklist <- check_dataset(test_df, specific_check = "missings", checklist)
            if (checklist$empty) return()

            if (!checklist$missing_vals) {
                showNotification("No missing values detected.", type = "message")
                return()
            }

            if (!checklist$missing_vals_all_vars) {
                datasets$undo_df <- datasets$current_data
                method <- input$impute_choice
                impute_custom <- input$impute_custom
                datasets$current_data <- mvpa::impute_missing_values(data = datasets$current_data,
                                                                    method = method,
                                                                    custom_value = impute_custom)
            } else {
                showNotification("Variables present, where all values are missing - Please remove those variables.", type = "error")
            }

        })

        observeEvent(input$apply_remove_na_vars, {

            if (is.null(datasets$current_data)) return()

            if (is.null(input$chooser_vars$right)) {
                test_df <- datasets$current_data
            } else {
                test_df <- as.data.frame(datasets$current_data[, input$chooser_vars$right])
            }

            checklist <- check_dataset(test_df, specific_check = "missings")
            if (checklist$missing_vals){
                datasets$undo_df <- datasets$current_data
                datasets$current_data <- mvpa::remove_variables(data = datasets$current_data,
                                                                   remove_na = TRUE)
            } else {
                showNotification("No missing values detected.", type = "message")
                return()
            }

        })

        observeEvent(input$apply_remove_na_objs, {

            if (is.null(datasets$current_data)) return()

            if (identical(input$chooser_vars$right, character(0))) {
                test_df <- datasets$current_data
            } else {
                test_df <- as.data.frame(datasets$current_data[, input$chooser_vars$right])
            }

            checklist <- check_dataset(test_df, specific_check = "missings")
            if (checklist$missing_vals){
                datasets$undo_df <- datasets$current_data
                datasets$current_data <- mvpa::remove_objects(data = datasets$current_data,
                                                                 remove_na = TRUE)
            } else {
                showNotification("No missing values detected.", type = "message")
                return()
            }

        })

        # Remove invariant variables
        observe({

            if (is.null(datasets$current_data)) return()

            invariants <- name_invariants(datasets$current_data, prune_list = TRUE)
            if (length(invariants) != 0){
                output$invariant_text <- renderUI(HTML(paste0("<b>", paste(invariants, collapse=" | "), "</b>")))
            } else {
                output$invariant_text <- renderUI(HTML("<b>No invariants detected</b>"))
            }

        })

        observeEvent(input$apply_remove_invariants, {

            if (is.null(datasets$current_data)) return()

            invariants <- name_invariants(datasets$current_data)
            if (length(invariants) != 0) {
                datasets$undo_df <- datasets$current_data

                datasets$current_data <- mvpa::remove_variables(data = datasets$current_data,
                                                               remove_invariants = TRUE)
                showNotification("Invariant variables have been removed.", type = "message")
            } else {
                showNotification("No invariants detected.", type = "message")
                return()
            }

        })

        # Categorical strings to numerical values
        observeEvent(input$apply_strings_to_binary, {

            if (is.null(datasets$current_data)) return()

            if (identical(input$chooser_vars$right, character(0))) {
                test_df <- datasets$current_data
                col_names <- NULL
            } else {
                col_names <- input$chooser_vars$right
                test_df <- as.data.frame(datasets$current_data[, col_names])
            }

            checklist <- check_dataset(test_df, specific_check = "strings")
            if (checklist$string_vals){
                datasets$undo_df <- datasets$current_data
                datasets$current_data <- mvpa::introduce_dummies(data = datasets$current_data,
                                                                 col_names = col_names,
                                                                 add_indicator = input$add_indicator)
                showNotification("Strings have been binarized.")
            } else {
                showNotification("No string values detected.", type = "message")
                return()
            }

        })

        # Input validation apply_offset

        #initiate
        iv_offset <- InputValidator$new()

        # rules
        iv_offset$add_rule("offset", sv_numeric())

        # enable
        iv_offset$enable()

        # Introduce offset
        observeEvent(input$apply_offset, {

            if (is.null(datasets$current_data)) return()

            if (!iv_offset$is_valid()) {
                showNotification(
                    "Please fix input errors. Calculation will not proceed.",
                    type = "error"
                )

                return()
            }

            if (identical(input$chooser_vars$right, character(0))) {
                test_df <- datasets$current_data
                col_names <- NULL
            } else {
                col_names <- input$chooser_vars$right
                test_df <- as.data.frame(datasets$current_data[, col_names])
            }

            checklist <- list()
            checklist <- check_dataset(test_df, specific_check = "zeros", checklist)
            checklist <- check_dataset(test_df, specific_check = "negatives", checklist)
            if (checklist$negative_vals | checklist$zero_vals) {
                datasets$undo_df <- datasets$current_data
                datasets$current_data <- mvpa::introduce_offset(data = isolate(datasets$current_data),
                                                               col_names = col_names,
                                                               new_min = isolate(input$offset))
                showNotification("Offset has been introduced.", type = "message")
            } else {
                showNotification("No negative or zero values present.", type = "message")
            }


        })


        # # Introduce unique combinations
        observeEvent(input$apply_combinations, {

            if (is.null(datasets$current_data)) return()

            checklist <- check_dataset(datasets$current_data)

            if (checklist$empty) {
                showNotification("Dataset is empty - Please load another dataset.", type = "error")
                return()
            }
            if (checklist$missing_vals) {
                showNotification("Dataset contains missing values - They need to be fixed before methods can be applied.", type = "warning")
                return()
            }
            if (checklist$string_vals) {
                showNotification("These methods cannot be applied to string values.", type = "error")
                return()
            }

            shinyjs::disable("apply_combinations")

            datasets$undo_df <- datasets$current_data

            if (input$ignore_response_combinations) {
                response_variable <- isolate(input$response_var)
            } else {
                response_variable <- NULL
            }

            datasets$current_data <- mvpa::introduce_unique_combinations(data = isolate(datasets$current_data),
                                                                        method = isolate(input$combinations_method),
                                                                        ignore_variables = response_variable)

            shinyjs::enable("apply_combinations")

        })



        # # Input validation apply_scale
        #initiate
        iv_scale <- InputValidator$new()

        # rules
        iv_scale$add_rule("nth_root_nr", sv_integer())

        # enable
        iv_scale$enable()


        # Scaling / Normalization methods
        observeEvent(input$apply_scale, {

            if (is.null(datasets$current_data)) return()

            if (!iv_scale$is_valid()) {
                showNotification(
                    "Please fix input errors. Calculation will not proceed.",
                    type = "error"
                )

                return()
            }

            if (check_dataset(datasets$current_data, specific_check = "empty")$empty) {
                showNotification("Dataset is empty - Please load another dataset.", type = "error")
                return()
            }
            if (!identical(input$chooser_vars$right, character(0))) {
                data <- datasets$current_data %>% dplyr::select(isolate(input$chooser_vars$right))
                vars_selected <- isolate(input$chooser_vars$right)
            } else if (input$ignore_response) {
                data <- datasets$current_data %>% dplyr::select(-isolate(input$response_var))
                vars_selected <- colnames(data)
            } else {
                data <- datasets$current_data
                vars_selected <-NULL
            }

            # Check if method might be not applicable to selected data
            checklist <- check_dataset(data)

            if (checklist$missing_vals) {
                showNotification("Dataset contains missing values - They need to be fixed before methods can be applied.", type = "warning")
                return()
            }
            if (checklist$string_vals) {
                showNotification("These methods cannot be applied to string values.", type = "message")
                return()
            }
            if ((checklist$negative_vals | checklist$zero_vals) & (isolate(input$scale_choice) == "log10" | isolate(input$scale_choice) == "log2" | isolate(input$scale_choice) == "ln")){
                showNotification("Log of negative or zero values is not defined.", type = "error")
                return()
            } else if (isolate(input$scale_choice) == "nth_root" & isolate(input$nth_root_nr) %% 2 == 0 & checklist$negative_vals){
                showNotification("Even root of negative values is not supported by mvpa.shiny.", type = "error")
                return()
            } else if (isolate(input$scale_choice) == "sd" & checklist$invariant_vars) {
                showNotification("Dataset contains invariant variables - Data can not be standardized - Remove invariants before standardization.", type = "error")
            } else {
                datasets$undo_df <- datasets$current_data

                datasets$current_data <- mvpa::scale_data(data = isolate(datasets$current_data),
                                                          col_names = vars_selected,
                                                          method = isolate(input$scale_choice),
                                                          n_for_root = isolate(abs(input$nth_root_nr)))
            }

        })

        # Load previously validated dataset from list
        observeEvent(input$use_as_current, {

            if (is.null(input$selected_valid_dataset)) return()

            if (input$selected_valid_dataset == "") {
                showNotification("No dataset has been validated yet", type = "message")
                return()
            }

            name <- input$selected_valid_dataset

            datasets$undo_df <- datasets$current_data
            datasets$current_data <- datasets$valid_datasets[[name]]$dataset

        })

        # Delete a validated dataset
        observeEvent(input$delete_apply, {

            if (is.null(input$selected_valid_dataset) & is.null(datasets$list_validated_datasets)) return()

            if (input$selected_valid_dataset == "") {
                showNotification("No valid dataset present.", type = "message")
                return()
            }

            datasets[["valid_datasets"]][[isolate(input$selected_valid_dataset)]] <- NULL

            # Reset consider removal input
            updateCheckboxInput(inputId = "delete_bool",
                                value = FALSE)

        })

        # Load demo dataset
        observeEvent(input$load_demo_data, {

            datasets$current_data <- mvpa::HOMA_IR
            datasets$original_data <- mvpa::HOMA_IR

        })

        # # Main panel
        observe({

            remaining_vars <- colnames(datasets$current_data)

            if (length(remaining_vars) == 0) {
                remaining_vars <- ""
            }
            # update response select input
            updateSelectizeInput(session = session,
                                 inputId = "response_var",
                                 choices = remaining_vars,
                                 server = TRUE)

        })

        # Removal button for objs and vars
        observeEvent(input$apply_removal, {

            if (identical(input$chooser_vars$right, character(0)) & identical(input$chooser_objs$right, character(0))) {
                showNotification("No variables or objects selected.", type = "message")
                return()
            }

            datasets$undo_df <- datasets$current_data
            temp_df <- mvpa::remove_variables(data = datasets$current_data,
                                              vars_to_remove = input$chooser_vars$right)
            if (!identical(input$chooser_objs$right, character(0))) {
                # If objects are characters, the whole column will be characters. Therefore, check if removed
                # object(s) are characters, then type convert dataset.
                if (any(apply(temp_df[input$chooser_objs$right, , drop = FALSE], 1, is.character))) {
                    temp_df <- mvpa::remove_objects(data = temp_df,
                                                    objs_to_remove = input$chooser_objs$right)

                    temp_df <- utils::type.convert(temp_df, as.is = TRUE)
                } else {
                    temp_df <- mvpa::remove_objects(data = temp_df,
                                                    objs_to_remove = input$chooser_objs$right)
                }
            }

            datasets$current_data <- temp_df

        })

        # Keep button for objs and vars
        observeEvent(input$apply_keep, {

            if (identical(input$chooser_vars$right, character(0)) & identical(input$chooser_objs$right, character(0))) {
                showNotification("No variables or objects selected.", type = "message")
                return()
            }

            datasets$undo_df <- datasets$current_data

            if (!identical(input$chooser_vars$right, character(0))) {
            temp_df <- mvpa::keep_variables(data = datasets$current_data,
                                            vars_to_keep = input$chooser_vars$right)
            } else {
                temp_df <- datasets$current_data
            }

            if (!identical(input$chooser_objs$right, character(0))) {

                test_df <- mvpa::remove_objects(data = datasets$current_data,
                                                objs_to_remove = input$chooser_objs$right)

                # If objects are characters, the whole column will be characters. Therefore, check if removed
                # object(s) are characters, then type convert dataset.
                temp_df <- mvpa::keep_objects(data = temp_df,
                                              objs_to_keep = input$chooser_objs$right)
                if (any(apply(test_df, 1, is.character))) {
                    temp_df <- utils::type.convert(temp_df, as.is = TRUE)
                }
            }
            datasets$current_data <- temp_df

            temp_df <- mvpa::keep_objects(data = temp_df,
                                          objs_to_keep = input$chooser_objs$right)

        })

        # Undo and reset button
        observeEvent(input$undo, {

            if (is.null(datasets$undo_df)) return()
            datasets$current_data <- datasets$undo_df

        })

        observeEvent(input$reset_df, {

            datasets$current_data <- datasets$original_data
            showNotification("Original dataset has been loaded again.", type = "message")

        })

        # Dataframe outputs
        # Current dataset rendering
        output$current_df <- DT::renderDT({

            if (is.null(datasets$current_data)) return()

            DT::datatable(adjust_decimals(datasets$current_data),
                          extensions = c("Scroller"),
                          options = list(scrollY = 350,
                                         scrollX = 500,
                                         deferRender = TRUE,
                                         scroller = TRUE,
                                         dom = "tip",
                                         columnDefs = list(list(className = 'dt-center', targets = "_all"))),
                          selection = 'none')


        })

        # Download current data
        observe({

            shinyjs::toggleState(id = "download_current",
                                 condition = !is.null(datasets$current_data))
            shinyjs::toggleState(id = "copy_current",
                                 condition = !is.null(datasets$current_data))

        })

        output$download_current <- downloadHandler(
            filename = function() {
                paste0("current-data_", Sys.Date(), ".", isolate(datasets$file_format))
            },
            content = function(file) {
                write_table(data = datasets$current_data,
                            format = isolate(datasets$file_format),
                            file = file)
            }
        )

        observeEvent(input$copy_current, {

            if (!is.null(datasets$current_data)) {

                clipr::clear_clip()
                clipr::write_clip(datasets$current_data,
                                  allow_non_interactive = TRUE)

                showNotification("Current dataset has been copied to the clipboard.",
                                 type = "message")
            }
        })

        # Valid dataset methods
        observe({

            datasets$new_dataset_name <- stringr::str_trim(input$new_dataset_name)

        })

        # Get dataset from other tabs (like covariate analysis)
        observe({

            if(!is.null(update_current_data)) {
                if(is.data.frame(update_current_data())){
                    datasets$undo_df <- isolate(datasets$current_data)
                    datasets$current_data <- update_current_data()
                }
            }

        })

        # Enhance current dataset (add PCs, or t scores)
        observe({

            if(length(enhance_current_data()) == 0) return()

            if(is.data.frame(enhance_current_data()[["data"]])){
                datasets$undo_df <- isolate(datasets$current_data)

                enhanced_data <- mvpa::enhance_dataset(data = isolate(datasets$current_data),
                                                       data_to_add = enhance_current_data()[["data"]])

                datasets$current_data <- enhanced_data
            }

        })

        observeEvent(input$add_valid_df, {


            if (is.null(datasets$current_data)) {
                showNotification("No dataset has been loaded", type = "message")
                return()
            }

            if (datasets$new_dataset_name == ""){
                showNotification("You have to provide a name for the dataset.", type = "warning")
                return()
            }

            if (datasets$new_dataset_name %in% names(datasets$valid_datasets)) {
                showNotification("Name already in use - Please choose another name.", type = "warning")
                return()
            }

            if (any(dim(datasets$current_data) > 100)) {
                showNotification("Dataset is checked for integrity. This may take a moment!", type = "message")
            }
            checklist <- check_dataset(datasets$current_data)
            if (checklist$empty) {
                showNotification("Dataset is empty - Please load another dataset.", type = "error")
                return()
            }
            if (checklist$missing_vals){
                showNotification("Dataset contains missing values - Apply imputation method or delete variables or objects.", type = "error")
                return()
            }
            if (checklist$string_vals) {
                showNotification("Dataset contains string values - Please change to numerical values.", type = "error")
                return()
            }
            if (checklist$invariant_vars) {
                showNotification("Dataset contains invariant variables - Please delete these variables.", type = "error")
                return()
            }

            if (!any(c(checklist$missing_vals, checklist$string_vals, checklist$invariant_vars))){

                    datasets$valid_datasets[[datasets$new_dataset_name]][["dataset"]] <- datasets$current_data
                    datasets$valid_datasets[[datasets$new_dataset_name]][["response_var"]] <- input$response_var
                    showNotification("Dataset is ready for analysis and has been added", type = "message")

            }

        })

        observe({

            if (length(datasets$valid_datasets) == 0) {

                updateSelectInput(session = session,
                                  inputId = "selected_valid_dataset",
                                  choices = "",
                                  selected = "")
                return()

            }

            updateSelectInput(session = session,
                              inputId = "selected_valid_dataset",
                              choices = names(datasets$valid_datasets),
                              selected = tail(names(datasets$valid_datasets), 1)
            )

        })

        # Valid dataset rendering
        observe({

            selection <- input$selected_valid_dataset
            if (selection == "") return()

            datasets$selected_valid_dataset <- input$selected_valid_dataset
            output$valid_df <- DT::renderDT({

                DT::datatable(adjust_decimals(datasets[["valid_datasets"]][[selection]][["dataset"]]),
                              extensions = c("Scroller"),
                              options = list(scrollY = 450,
                                             scrollX = 500,
                                             deferRender = TRUE,
                                             scroller = TRUE,
                                             dom = "Bfrtip",
                                             columnDefs = list(list(className = 'dt-center', targets = "_all"))),
                              selection = 'none')

            })

        })

        # Download valid data
        observe({

            shinyjs::toggleState(id = "download_valid",
                                 condition = (input$selected_valid_dataset != ""))
            shinyjs::toggleState(id = "copy_valid",
                                 condition = (input$selected_valid_dataset != ""))

        })

        output$download_valid <- downloadHandler(
            filename = function() {
                paste0(isolate(input$selected_valid_dataset), "_", Sys.Date(), ".", isolate(datasets$file_format))
            },
            content = function(file) {
                write_table(data = datasets[["valid_datasets"]][[isolate(input$selected_valid_dataset)]][["dataset"]],
                            format = isolate(datasets$file_format),
                            file = file)
            }
        )

        observeEvent(input$copy_valid, {

            if (is.null(datasets$current_data))  return()

            clipr::clear_clip()
            clipr::write_clip(datasets[["valid_datasets"]][[isolate(input$selected_valid_dataset)]][["dataset"]],
                              allow_non_interactive = TRUE)

            showNotification("Dataset has been copied to the clipboard.",
                             type = "message")

        })

        # Update rownames of current data. Necessary to check if PCs or T scores can be added to the current data.
        observe({

            if (is.null(datasets$current_data)) return()

            datasets$current_data_obj_names <- rownames(datasets$current_data)

        })

        # Show simple statistics
        observe({

            if (!input$stats) {
                output$stats_output <- renderUI({})
            } else {
                selection <- input$selected_valid_dataset
                if (selection != ""){

                    # Edge case where valid dataset gets deleted
                    if (is.null(datasets[["valid_datasets"]][[selection]][["dataset"]])) {
                        output$stats_output <- renderUI({})
                        return()
                    }

                    datasets$stats <- t(mvpa::data_summary(datasets[["valid_datasets"]][[selection]][["dataset"]]))
                    output$stats_output <- renderUI({
                        tagList(
                            HTML("<b>Statistics summary</b>"),

                            downloadButton(NS(id, "download_stats"),
                                           label = "Download table"),

                            actionButton(NS(id, "copy_stats"),
                                         label = "Copy to clipboard"),

                            column(width = 12,

                                   DT::renderDT({

                                       DT::datatable(adjust_decimals(datasets$stats),
                                                     extensions = c("Scroller"),
                                                     options = list(scrollY = 250,
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
            }

        })

        # Download simple statistics
        observe({

            shinyjs::toggleState(id = "download_valid",
                                 condition = !is.null(datasets$stats))
            shinyjs::toggleState(id = "copy_valid",
                                 condition = !is.null(datasets$stats))

        })

        output$download_stats <- downloadHandler(
            filename = function() {
                paste0(isolate(input$selected_valid_dataset), "_statistics_", Sys.Date(), ".", isolate(datasets$file_format))
            },
            content = function(file) {
                write_table(data = datasets$stats,
                            format = isolate(datasets$file_format),
                            file = file)
            }
        )

        observeEvent(input$copy_stats, {
            if (is.null(datasets$stats)) return()

            clipr::clear_clip()
            clipr::write_clip(datasets$stats,
                              allow_non_interactive = TRUE)

            showNotification("Dataset has been copied to the clipboard.",
                             type = "message")

        })

        datasets

    })
}
