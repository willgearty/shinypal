# common UI components ####

#' @title Accordion panel that includes a remove button
#' @param ind The index of the step.
#' @param ... Additional arguments passed to [bslib::accordion_panel()].
#' @importFrom shiny actionButton
#' @importFrom bslib accordion_panel
#' @description
#'   A [bslib::accordion_panel()] pre-wired with a "Remove this step" button and
#'   the `data-rank-id` attribute shinypal's sortable workflow needs. Use it as
#'   the panel returned by a step's `fun_workflow`.
#' @returns A [bslib::accordion_panel()] tag with `value = "step_<ind>"` and a
#'   matching `data-rank-id` attribute.
#' @examples
#' accordion_panel_remove_button(1, "My step")
#' @family step UI
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
#' @description
#'   A [shiny::selectInput()] listing the intermediate datasets available to
#'   step `ind` (those produced by earlier steps), defaulting to the most
#'   recent. Keep its choices current with [df_select_observe()].
#' @returns A [shiny::selectInput()] tag with id `dataset_<ind>`.
#' @examples
#' \dontrun{
#' select_dataset_input(ind)
#' }
#' @seealso [df_select_observe()], which keeps this dropdown populated.
#' @family step UI
#' @export
select_dataset_input <- function(ind, label = "Choose a dataset:") {
  req(ind, label)
  df_names <- get_int_dfs(ind)
  # default to the most recent dataset (the one produced closest above this)
  selected_df <- if (length(df_names)) df_names[[length(df_names)]] else NULL
  selectInput(paste0("dataset_", ind), label, choices = df_names,
              selected = selected_df)
}

#' @title Select input to choose a column from a specified dataset
#' @description This should be paired with [select_dataset_input()].
#' @param ind The index of the step.
#' @param label The label for the select input.
#' @param default The default value for the select input.
#' @param ... Additional arguments passed to [shiny::varSelectInput()].
#' @importFrom shiny varSelectInput
#' @returns A [shiny::varSelectInput()] tag with id `column_<ind>`.
#' @examples
#' \dontrun{
#' select_column_input(ind)
#' }
#' @seealso [column_select_observe()], which keeps this selector in sync.
#' @family step UI
#' @export
select_column_input <- function(ind, label = "Choose a column:",
                                default = NULL, ...) {
  req(ind, label)
  df_names <- get_int_dfs(ind)
  shinypal_env <- check_setup()
  # guard against being added before any dataset exists
  data <- if (length(df_names) > 0) {
    # initialize the column choices from the most recent dataset
    shinypal_env$intermediate_list[[df_names[[length(df_names)]]]]()
  } else {
    data.frame()
  }
  varSelectInput(paste0("column_", ind), label,
                 data = data, selected = default, ...)
}

#' @title Button to show dataframe modal
#' @description
#'   Generates a [shiny::actionButton()] that, when clicked, generates a modal
#'   for the resulting dataset for the specified step. Make sure to set up a
#'   corresponding observer using [df_modal_observe()].
#' @param ind The index of the step.
#' @param text The text to display on the button.
#' @importFrom shiny actionButton
#' @returns A [shiny::actionButton()] with id `df_modal_<ind>`.
#' @examples
#' df_modal_button(1)
#' @seealso [df_modal_observe()], which opens the modal this button triggers.
#' @family step UI
#' @export
df_modal_button <- function(ind, text = "View data") {
  actionButton(paste0("df_modal_", ind), text)
}

#' @title Text element with a button to copy the code to the clipboard
#' @description Render a reactive output variable as text within an application
#'   page. Uses [shiny::verbatimTextOutput()] which is usually paired with
#'   [shiny::renderPrint()] and provides fixed-width text in a `<pre>`. Make
#'   sure to set up a corresponding observer using [clip_observe()].
#' @param ind The index of the step.
#' @importFrom shiny div verbatimTextOutput actionButton icon
#' @returns A [htmltools::div()] wrapping a [shiny::verbatimTextOutput()] (id
#'   `code_<ind>`) and a copy [shiny::actionButton()] (id `copy_<ind>`).
#' @examples
#' verbatimTextOutput_copy(1)
#' @seealso [clip_observe()], which copies the displayed code to the clipboard.
#' @family step UI
#' @export
verbatimTextOutput_copy <- function(ind) {
  div(
    verbatimTextOutput(paste0("code_", ind)),
    actionButton(paste0("copy_", ind), icon("copy")),
    class = "code_wrapper"
  )
}

#' @title Text input to give a step's dataset a custom name
#' @param ind The index of the step.
#' @param label The label for the text input.
#' @importFrom shiny textInput
#' @description
#'   A [shiny::textInput()] (id `varname_<ind>`) for naming the dataset a step
#'   produces. The name becomes the dataset's label in later selectors and its
#'   variable name in the generated script. Pair with [var_name_observe()], which validates
#'   the entry; [add_shinypal_data_step()] inserts both automatically when
#'   `rename = TRUE`.
#' @returns A [shiny::textInput()] tag with id `varname_<ind>`.
#' @examples
#' \dontrun{
#' varname_input(ind)
#' }
#' @seealso [var_name_observe()], which validates and stores the entry.
#' @family step UI
#' @export
varname_input <- function(ind, label = "Name this dataset (optional):") {
  req(ind)
  textInput(paste0("varname_", ind), label, value = "")
}
