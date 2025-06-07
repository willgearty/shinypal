# common UI components ####

#' @title Accordion panel that includes a remove button
#' @param ind The index of the step.
#' @param ... Additional arguments passed to [bslib::accordion_panel()].
#' @importFrom shiny actionButton
#' @importFrom bslib accordion_panel
#' @export
accordion_panel_remove_button <- function(ind, ...) {
  req(ind)
  tmp <- accordion_panel(
    ...,
    actionButton(paste0("remove_step_", ind), "Remove this step"),
    value = paste0("step_", ind)
  )
  # need this attribute for sortable_js_capture_input
  tmp$attribs$`data-rank-id` <- paste0("step_", ind)
  tmp
}

#' @title Select input to choose a shinypal intermediate dataset
#' @param ind The index of the step.
#' @param label The label for the select input.
#' @importFrom shiny selectInput
#' @export
select_dataset_input <- function(ind, label = "Choose a dataset:") {
  req(ind, label)
  df_names <- get_int_dfs(ind)
  # TODO: selected should be the most recent dataset?
  selectInput(paste0("dataset_", ind), label, choices = df_names)
}

#' @title Select input to choose a column from a specified dataset
#' @description This should be paired with [select_dataset_input()].
#' @param ind The index of the step.
#' @param label The label for the select input.
#' @param default The default value for the select input.
#' @param ... Additional arguments passed to [shiny::varSelectInput()].
#' @importFrom shiny varSelectInput
#' @export
select_column_input <- function(ind, label = "Choose a column:",
                                default = NULL, ...) {
  req(ind, label)
  df_names <- get_int_dfs(ind)
  varSelectInput(paste0("column_", ind), label,
                 data = shinypal_env$intermediate_list[[df_names[[1]]]](),
                 selected = default, ...)
}

#' @title Button to show dataframe modal
#' @description
#'   Generates a [shiny::actionButton()] that, when clicked, generates a modal
#'   for the resulting dataset for the specified step. Make sure to set up a
#'   corresponding observer using [df_modal_observe()].
#' @param ind The index of the step.
#' @param text The text to display on the button.
#' @importFrom shiny actionButton
#' @export
df_modal_button <- function(ind, text = "View data") {
  # TODO: should/can the observer be generated here?
  actionButton(paste0("df_modal_", ind), text)
}

#' @title Text element with a button to copy the code to the clipboard
#' @description Render a reactive output variable as text within an application
#'   page. Uses [shiny::verbatimTextOutput()] which is usually paired with
#'   [shiny::renderPrint()] and provides fixed-width text in a `<pre>`. Make
#'   sure to set up a [shiny::observeEvent()] observer for when the
#'   [shiny::actionButton()] is clicked.
#' @param ind The index of the step.
#' @importFrom shiny div verbatimTextOutput actionButton icon
#' @export
verbatimTextOutput_copy <- function(ind) {
  # TODO: should the observer be generated here?
  div(
    verbatimTextOutput(paste0("code_", ind)),
    actionButton(paste0("copy_", ind), icon("copy")),
    class = "code_wrapper"
  )
}
