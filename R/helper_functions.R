# Helper functions

# Find invariant variable names to display
name_invariants <- function(data, prune_list = TRUE) {
    invariants <- data %>%
                  dplyr::select_if(is.numeric) %>%
                  dplyr::select_if(function(x) all(stats::var(x) == 0)) %>%
                  colnames()


    if (prune_list) {
        if (length(invariants) >= 10) {
            remaining <- length(invariants) - 10
            invariants <- c(invariants[1:15], paste("+", remaining, "more"))
        }
    }

    return(invariants)
}

# Placeholder plot
no_data_plot <- function() {
    p <- plotly::plotly_empty(type = 'scatter', mode = 'markers') %>%
         plotly::config(displayModeBar = FALSE) %>%
         plotly::layout(title = list(text = "NO DATA",
                                     yref = "paper",
                                     y = 0.5))
    return(p)
}

# Show selected valid dataset (from datasets tab)
get_selected_dataset <- function(selected_dataset, valid_datasets) {

    if (is.null(selected_dataset)) return()
    if (is.null(valid_datasets)) return()

    available_datasets <- names(valid_datasets)

    if (selected_dataset %in% available_datasets) {
        return(selected_dataset)
    } else {
        return("")
    }

}

# Parse principal component number from PC1 notation
get_pc_numeric <- function(pc_string) {

    if (is.null(pc_string)) return()

    number_from_string <- stringr::str_remove(pc_string, "PC")

    return(as.integer(number_from_string))

}

# Trim dataframes for plotting
adjust_decimals <- function(data, decimals = 3) {
    data <- as.data.frame(data)
    non_numeric_cols <- data %>% dplyr::select_if(purrr::negate(is.numeric)) %>% colnames()
    if (!identical(non_numeric_cols, character(0))) {
        original_order <- colnames(data)
        string_part <- data[, non_numeric_cols, drop=FALSE]
        numeric_part <- data[, !(colnames(data) %in% c(non_numeric_cols))]
        numeric_part <- adjust_decimals(numeric_part)
        data <- cbind(string_part, numeric_part)
        return(data[original_order])
    }
    return(format(round(data, 3), nsmall = decimals))
}


# Lightweight check_dataset function
#' @importFrom rlang .data
check_dataset <- function(data = NULL, specific_check = NULL, checklist = NULL){

    # Empty dataset
    check_empty <- function (data, checklist) {
        check_empty <- nrow(data) == 0 | ncol(data) == 0

        if (check_empty) {
            checklist["empty"] <- TRUE
            checklist["missing_vals"] <- FALSE
            checklist["missing_vals_all_vars"] <- FALSE
            checklist["missing_vals_all_objs"] <- FALSE
            checklist["invariant_vars"] <- FALSE
            checklist["invariant_objs"] <- FALSE
            checklist["zero_vals"] <- FALSE
            checklist["zero_vals_all_vars"] <- FALSE
            checklist["zero_vals_all_objs"] <- FALSE
            checklist["negative_vals"] <- FALSE
            checklist["categorical_vals"] <- FALSE

            return(checklist)
        } else {
            checklist["empty"] <- FALSE
        }
        return(checklist)
    }

    # Missing values
    check_missings <- function(data, checklist) {
        missing_vals_any <- data %>%
            dplyr::select_if(function(x) any(is.na(x)))
        missing_vals_all_vars <- data %>%
            dplyr::select_if(function(x) all(is.na(x)))
        missing_vals_all_objs <- data %>%
            is.na %>%
            apply(., 1, all)


        if (ncol(missing_vals_any) > 0 | ncol(missing_vals_all_vars) > 0 | any(missing_vals_all_objs, na.rm = TRUE)) {

            checklist["missing_vals"] <- TRUE
            if (ncol(missing_vals_all_vars) > 0 & any(missing_vals_all_objs == 0, na.rm = TRUE)) {
                checklist["missing_vals_all_vars"] <- TRUE
                checklist["missing_vals_all_objs"] <- TRUE
            } else if (ncol(missing_vals_all_vars) > 0) {
                checklist["missing_vals_all_vars"] <- TRUE
                checklist["missing_vals_all_objs"] <- FALSE
            } else if (any(missing_vals_all_objs, na.rm = TRUE)) {
                checklist["missing_vals_all_vars"] <- FALSE
                checklist["missing_vals_all_objs"] <- TRUE
            } else {
                checklist["missing_vals_all_vars"] <- FALSE
                checklist["missing_vals_all_objs"] <- FALSE
            }

        } else {
            checklist["missing_vals"] <- FALSE
            checklist["missing_vals_all_vars"] <- FALSE
            checklist["missing_vals_all_objs"] <- FALSE
        }
        return(checklist)
    }

    # String variables
    check_strings <- function(data, checklist) {
        string_vars <- data %>%
            dplyr::select_if(function(x) is.character(x) | is.factor(x))
        if (ncol(string_vars) > 0){
            checklist["string_vals"] <- TRUE
        } else {
            checklist["string_vals"] <- FALSE
        }
        return(checklist)
    }

    # Zero values
    check_zeros <- function(data, checklist) {
        zero_vals_any <- data %>%
            dplyr::select_if(function(x) is.numeric(x) & any(x == 0, na.rm = TRUE))
        zero_vals_all_vars <- data %>%
            dplyr::select_if(function(x) is.numeric(x) & all(x == 0, na.rm = TRUE))
        zero_vals_all_objs <- data %>%
            dplyr::select_if(is.numeric) %>%
            rowSums

        if (ncol(zero_vals_any)){
            checklist["zero_vals"] <- TRUE
            if (ncol(zero_vals_all_vars) > 0 & any(zero_vals_all_objs == 0, na.rm = TRUE)) {
                checklist["zero_vals_all_vars"] <- TRUE
                checklist["zero_vals_all_objs"] <- TRUE
            } else if (ncol(zero_vals_all_vars) > 0) {
                checklist["zero_vals_all_vars"] <- TRUE
                checklist["zero_vals_all_objs"] <- FALSE
            } else if (any(zero_vals_all_objs == 0, na.rm = TRUE)) {
                checklist["zero_vals_all_vars"] <- FALSE
                checklist["zero_vals_all_objs"] <- TRUE
            }
        } else {
            checklist["zero_vals_all_vars"] <- FALSE
            checklist["zero_vals_all_objs"] <- FALSE
            checklist["zero_vals"] <- FALSE
        }
        return(checklist)
    }

    # Invariant variables and objects
    check_invariants <- function(data, checklist) {
        invariant_vars <- data %>%
            dplyr::select_if(is.numeric) %>%
            apply(., 2, stats::var, na.rm = TRUE) == 0

        invariant_objs <- data %>%
            dplyr::select_if(is.numeric) %>%
            apply(., 1, stats::var, na.rm = TRUE) == 0

        if (any(invariant_vars, na.rm = TRUE) | any(invariant_objs, na.rm = TRUE)) {

            if (any(invariant_vars, na.rm = TRUE) & any(invariant_objs, na.rm = TRUE)) {
                checklist["invariant_vars"] <- TRUE
                checklist["invariant_objs"] <- TRUE
            } else if (any(invariant_vars, na.rm = TRUE)) {
                checklist["invariant_vars"] <- TRUE
                checklist["invariant_objs"] <- FALSE
            } else if (any(invariant_objs, na.rm = TRUE)) {
                checklist["invariant_vars"] <- FALSE
                checklist["invariant_objs"] <- TRUE
            }

        } else {
            checklist["invariant_vars"] <- FALSE
            checklist["invariant_objs"] <- FALSE
        }
        return(checklist)
    }

    # Negative values
    check_negatives <- function(data, checklist) {
        negative_vals <- data %>%
            dplyr::select_if(is.numeric) %>%
            dplyr::select_if(function(x) any(x < 0, na.rm = TRUE))
        if (ncol(negative_vals) > 0){
            checklist["negative_vals"] <- TRUE

        } else checklist["negative_vals"] <- FALSE

        return(checklist)
    }


    if (is.null(specific_check)) {
        checklist <- list()
        checklist <- check_empty(data, checklist)
        checklist <- check_missings(data, checklist)
        checklist <- check_strings(data, checklist)
        checklist <- check_zeros(data, checklist)
        checklist <- check_invariants(data, checklist)
        checklist <- check_negatives(data, checklist)
        return(checklist)
    }

    if (is.null(checklist)) {
        checklist <- list()
    }
    checklist <- switch(specific_check,
                        empty = check_empty(data, checklist),
                        missings = check_missings(data, checklist),
                        strings = check_strings(data, checklist),
                        zeros = check_zeros(data, checklist),
                        invariants = check_invariants(data, checklist),
                        negatives = check_negatives(data, checklist)
    )

    return(checklist)
}

# Table writer
write_table <- function(data = NULL, format = "tsv", file) {

    switch (format,
        tsv = utils::write.table(data, file, sep = "\t", row.names = TRUE, col.names = NA),
        csv = utils::write.table(data, file, sep = ",", row.names = TRUE, col.names = NA),
        xlsx = openxlsx::write.xlsx(as.data.frame(data), file, rowNames = TRUE, colNames = TRUE, append = FALSE)
    )

}

# Optimize plots from the base package for the shiny package
plotly_helper <- function(fig, filename = "not_set_yet") {

    # margins = list(
    #     l = 1,
    #     r = 1,
    #     b = 1,
    #     t = 1,
    #     p = 0
    # )

    # add tooltip = "text" to enable better readable tooltips
    fig <- fig %>%
           plotly::layout(autosize = TRUE) %>%
           plotly::config(displaylogo = FALSE) %>%
           plotly::config(modeBarButtonsToRemove = c("lasso2d",
                                                     "select2d",
                                                     "toggleSpikelines",
                                                     "zoomIn2d",
                                                     "zoomOut2d")) %>%
           plotly::config(toImageButtonOptions = list(
               filename = filename,
               format = "svg"
           ))

    return(fig)

}

# Dual list chooser
# @ https://github.com/rstudio/shiny-examples/blob/main/036-custom-input-control/chooser.R

chooserInput <- function(inputId, leftLabel, rightLabel, leftChoices, rightChoices,
                         size = 5, multiple = FALSE, session, inputname) {

    addResourcePath(
        prefix='js',
        directoryPath = system.file('js',
                                    package='mvpaShiny')
    )

    leftChoices <- lapply(leftChoices, tags$option)
    rightChoices <- lapply(rightChoices, tags$option)

    if (multiple)
        multiple <- "multiple"
    else
        multiple <- NULL

    tagList(
        singleton(tags$head(
            tags$script(src="js/chooser-binding.js"),
            tags$style(type="text/css",
                       HTML(".chooser-container { display: inline-block; }")
            )
        )),
        div(id=inputId, class="chooser",
            div(class="chooser-container chooser-left-container",
                tags$h5(leftLabel, style = "font-weight: bold"),
                tags$select(class="left", size=size, multiple=multiple, leftChoices)
            ),
            div(class="chooser-container chooser-center-container",
                icon("circle-right", "right-arrow fa-3x"), # Those icon names might be deprecated in future releases
                tags$br(),
                icon("circle-left", "left-arrow fa-3x")
            ),
            div(class="chooser-container chooser-right-container",
                tags$h5(rightLabel, style = "font-weight: bold"),
                tags$select(class="right", size=size, multiple=multiple, rightChoices)
            )
        )
    )
}

# Use onLoad function to register Chooser handler
.onLoad <- function(libname, pkgname) {
    registerInputHandler("shinyjsexamples.chooser", function(data, ...) {
        if (is.null(data))
            NULL
        else
            list(left=as.character(data$left), right=as.character(data$right))
    }, force = TRUE)
}
