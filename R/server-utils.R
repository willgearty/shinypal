# common server functions ####

#' @title Get the names of all intermediate data.frames for a given step
#' @param ind The index of the step.
#' @importFrom shiny reactiveValuesToList isolate
#' @description
#'   Returns the names of intermediate datasets produced by steps *earlier* than
#'   `ind` in the current workflow order, so a step can only ever consume
#'   upstream output. Used to populate the dataset selectors.
#' @returns A named character vector of intermediate datasets in workflow
#'   order: values are the stable internal ids (`occs_<ind>`) and names are the
#'   display labels (a custom name if set, otherwise the id). Empty if none are
#'   available upstream.
#' @examples
#' \dontrun{
#' # inside a reactive or observer in a module, after shinypal_setup()
#' get_int_dfs(ind)
#' }
#' @family intermediate data
#' @export
get_int_dfs <- function(ind) {
  shinypal_env <- check_setup()
  req(ind)
  # we want a small, intentional set of reactive dependencies here: the set of
  # stored names, the workflow order (code_chain), and the custom display names
  # (var_names, read below). each stored reactive is evaluated inside isolate()
  # so refreshing the dataset choices neither depends on every dataset's value
  # nor recomputes it.
  lst <- reactiveValuesToList(shinypal_env$intermediate_list)
  # restrict choices to datasets produced by steps *earlier* than this one in
  # the current workflow order, so a step can never consume a later step's (or
  # its own) output. this keeps the step dependency graph acyclic, preventing
  # reactive infinite loops and out-of-order generated code
  chain_names <- names(shinypal_env$code_chain())
  pos <- match(paste0("step_", ind), chain_names)
  # a step still being built isn't in the chain yet; treat it as appended last
  if (is.na(pos)) pos <- length(chain_names) + 1L
  earlier_inds <- sub("^step_", "", chain_names[seq_len(pos - 1L)])
  lst <- lst[sub(".*_(\\d+)$", "\\1", names(lst)) %in% earlier_inds]
  dfs <- lst |>
    Filter(f = Negate(is.null)) |>
    Filter(f = function(el) is.data.frame(isolate(el()))) |>
    names()
  # put the ids in workflow order
  dfs <- dfs[order(match(sub(".*_(\\d+)$", "\\1", dfs), earlier_inds))]
  # attach display labels: a custom name from var_names if set, otherwise the
  # id itself
  labels <- vapply(dfs, function(id) {
    nm <- shinypal_env$var_names[[id]]
    if (is.null(nm) || !nzchar(nm)) id else nm
  }, character(1))
  names(dfs) <- labels
  dfs
}

#' @title Show a modal with a reactable data.frame
#' @param ind The index of the step.
#' @param df_name The name of the data.frame to be displayed.
#' @importFrom shiny observeEvent showModal modalDialog
#' @importFrom DT DTOutput renderDataTable datatable
#' @importFrom shinycssloaders withSpinner
#' @description
#'   Installs an observer that opens a modal showing a [DT::datatable()] of the
#'   named intermediate dataset when the step's "view data" button is clicked.
#'   Pair with [df_modal_button()].
#' @returns Called for its side effects; invisibly returns the observer.
#' @examples
#' \dontrun{
#' df_modal_observe(ind, paste0("occs_", ind))
#' }
#' @seealso [df_modal_button()], the button that opens this modal.
#' @family step observers
#' @export
df_modal_observe <- function(ind, df_name) {
  shinypal_env <- check_setup()
  input <- shinypal_env$input
  output <- shinypal_env$output
  req(ind, df_name)
  observeEvent(input[[paste0("df_modal_", ind)]], {
    showModal(modalDialog(
      withSpinner(DTOutput(paste0("df_", ind))),
      size = "l", easyClose = TRUE
    ))
    output[[paste0("df_", ind)]] <- renderDataTable(datatable(
      isolate(shinypal_env$intermediate_list[[df_name]]()),
      style = "bootstrap5",
      options = list(
        autoWidth = TRUE,
        columnDefs = list(list(width = '25px', targets = 0),
                          list(width = '150px', targets = '_all')),
        scrollX = TRUE,
        scrollY = "70dvh",
        scrollCollapse = TRUE,
        pageLength = 25
      )
    ))
  }, ignoreInit = TRUE)
}

#' @title Keep a `selectInput` of intermediate data.frames up-to-date
#' @param ind The index of the step.
#' @importFrom shiny req observe isolate updateSelectInput
#' @importFrom rlang `%||%`
#' @description
#'   Installs an observer that keeps the step's dataset dropdown
#'   (`dataset_<ind>`) populated with the available upstream datasets,
#'   preserving the current selection where possible and otherwise defaulting to
#'   the most recent one.
#' @returns Called for its side effects; invisibly returns the observer.
#' @examples
#' \dontrun{
#' df_select_observe(ind)
#' }
#' @seealso [select_dataset_input()], the dropdown this observer updates.
#' @family step observers
#' @export
df_select_observe <- function(ind) {
  shinypal_env <- check_setup()
  input <- shinypal_env$input
  req(ind)
  # choices should always include all intermediate data.frames
  observe({
    choices <- get_int_dfs(ind) %||% character(0)
    # try to preserve the old selected dataset name
    old_df <- isolate(input[[paste0("dataset_", ind)]])
    if (!is.null(old_df) && old_df %in% choices) {
      selected <- old_df
    } else {
      # otherwise default to the most recent dataset (closest above this step)
      selected <- if (length(choices)) choices[[length(choices)]] else NULL
    }
    # update choices but maintain selected
    updateSelectInput(inputId = paste0("dataset_", ind), choices = choices,
                      selected = selected)
  })
}

#' @title Keep a `varSelectInput` of data.frame column names up-to-date
#' @param ind The index of the step.
#' @param inputId The id of the [shiny::varSelectInput()] object.
#' @importFrom shiny req observe isolate updateVarSelectInput
#' @importFrom rlang as_string
#' @description
#'   Installs an observer that keeps a [shiny::varSelectInput()] of column names
#'   in sync with the dataset currently chosen in the step's `dataset_<ind>`
#'   dropdown. Pair with [select_dataset_input()] and [select_column_input()].
#' @returns Called for its side effects; invisibly returns the observer.
#' @examples
#' \dontrun{
#' column_select_observe(ind, paste0("column_", ind))
#' }
#' @seealso [select_column_input()], the selector this observer updates.
#' @family step observers
#' @export
column_select_observe <- function(ind, inputId) {
  shinypal_env <- check_setup()
  input <- shinypal_env$input
  req(ind, inputId)
  observe({
    df_name <- input[[paste0("dataset_", ind)]]
    req(df_name)
    df <- get_int_data(df_name)()
    choices <- colnames(df)
    # try to preserve the old selected column name
    old_col <- isolate(input[[inputId]])
    if (!is.null(old_col) && as_string(old_col) %in% choices) {
      selected <- old_col
    } else {
      selected <- NULL
    }
    updateVarSelectInput(inputId = inputId, data = df, selected = selected)
  })
}

#' @title Add an observer to a copy button
#' @description
#'   Generates an event observer that watches for when a copy button is clicked.
#'   Upon clicking, the `code_expr` is evaluated and copied to the clipboard
#'   using [clipr::write_clip()].
#' @param ind The index of the step.
#' @param code_expr An expression (generated by [rlang::expr()] that, when
#'   evaluated, returns content to be copied (such as the output from
#'   [shinymeta::expandChain()].
#' @importFrom shiny observeEvent
#' @importFrom clipr write_clip
#' @returns Called for its side effects; invisibly returns the observer.
#' @examples
#' \dontrun{
#' clip_observe(ind, rlang::expr(get_chunk(ind)))
#' }
#' @seealso [verbatimTextOutput_copy()], whose copy button it wires.
#' @family step observers
#' @export
clip_observe <- function(ind, code_expr) {
  shinypal_env <- check_setup()
  input <- shinypal_env$input
  observeEvent(input[[paste0("copy_", ind)]], {
    write_clip(eval(code_expr), allow_non_interactive = TRUE)
  })
}

#' @title Observe a file input to be included in the download bundle
#' @param inputId The id of the [shiny::fileInput()] object whose uploaded file
#'   should be included in the downloadable bundle.
#' @importFrom shiny observeEvent isolate
#' @importFrom rlang inject set_names
#' @description
#'   Installs an observer that records the file uploaded through `inputId` so it
#'   is bundled into the downloadable report archive, keyed by the upload's
#'   original file name.
#' @returns Called for its side effects; invisibly returns the observer.
#' @examples
#' \dontrun{
#' file_observe("user_file")
#' }
#' @family step observers
#' @export
file_observe <- function(inputId) {
  shinypal_env <- check_setup()
  input <- shinypal_env$input
  observeEvent(input[[inputId]], {
    file_obj <- isolate(input[[inputId]])
    shinypal_env$include_files[[inputId]] <-
      set_names(list(file_obj$datapath), file_obj$name)
  }, ignoreInit = TRUE)
}

#' @title Validate and store a custom name for a step's dataset
#' @param ind The index of the step.
#' @importFrom shiny req observeEvent showNotification reactiveValuesToList
#' @description
#'   Installs an observer on the step's `varname_<ind>` text input. A valid,
#'   unique R variable name is recorded in shinypal's `var_names` registry,
#'   becoming the dataset's label in later selectors and its symbol in the
#'   generated script. Pair with [varname_input()].
#' @returns Called for its side effects; invisibly returns the observer.
#' @examples
#' \dontrun{
#' var_name_observe(ind)
#' }
#' @seealso [varname_input()], the input this observer watches.
#' @family step observers
#' @export
var_name_observe <- function(ind) {
  shinypal_env <- check_setup()
  input <- shinypal_env$input
  req(ind)
  id <- paste0("occs_", ind)
  observeEvent(input[[paste0("varname_", ind)]], {
    proposed <- input[[paste0("varname_", ind)]]
    # an empty entry means "no custom name": fall back to the internal id
    if (is.null(proposed) || !nzchar(trimws(proposed))) {
      shinypal_env$var_names[[id]] <- NULL
      return()
    }
    proposed <- trimws(proposed)
    # must be a syntactically valid R name so the renamed script still parses
    if (make.names(proposed) != proposed) {
      showNotification(
        paste0("\"", proposed, "\" isn't a valid R variable name; ",
               "the name was not applied."),
        type = "error"
      )
      return()
    }
    # must be unique among the other datasets' names (custom or default id) so
    # the generated script never declares two variables with the same name
    others <- setdiff(
      names(reactiveValuesToList(shinypal_env$intermediate_list)), id
    )
    taken <- vapply(others, function(other_id) {
      nm <- shinypal_env$var_names[[other_id]]
      if (is.null(nm) || !nzchar(nm)) other_id else nm
    }, character(1))
    if (proposed %in% taken) {
      showNotification(
        paste0("The name \"", proposed, "\" is already used by another step; ",
               "choose a unique name."),
        type = "error"
      )
      return()
    }
    shinypal_env$var_names[[id]] <- proposed
  }, ignoreInit = TRUE)
}

#' @title Get the expanded code chunk for a registered step
#' @description
#'   Returns the code chunk for `ind`, expanded with a shared context across
#'   all currently-registered steps. Use this inside a reactive consumer
#'   (e.g., `renderPrint()`, `observeEvent()`) to display or copy a step's
#'   code. The chunk is rebuilt whenever any dependency changes.
#' @param ind The index of the step.
#' @returns A code object suitable for printing or passing to
#'   [shinymeta::displayCodeModal()], or `NULL` if the step is not registered.
#' @examples
#' \dontrun{
#' # inside renderPrint() or observeEvent() in a module
#' get_chunk(ind)
#' }
#' @family generated code
#' @export
get_chunk <- function(ind) {
  shinypal_env <- check_setup()
  chunk <- shinypal_env$chunks()[[paste0("step_", ind)]]
  # re-throw any per-step error so the consuming reactive's req()/validate()
  # semantics handle it normally (silent errors stay silent, etc.)
  if (inherits(chunk, "condition")) stop(chunk)
  chunk
}

#' @title Check whether the workflow has incomplete or errored steps
#' @description
#'   Returns `TRUE` if any step in the current workflow cannot be expanded into
#'   the downloadable script due to failing `req()`/`validate()` checks.
#' @returns A length-one logical.
#' @examples
#' \dontrun{
#' if (workflow_has_errors()) {
#'   shiny::showNotification("Some steps are incomplete.")
#' }
#' }
#' @family generated code
#' @export
workflow_has_errors <- function() {
  shinypal_env <- check_setup()
  any(vapply(shinypal_env$chunks(), inherits, logical(1), "condition"))
}

#' @title Set an intermediate data object
#' @param obj A reactive data object to store.
#' @param name A name to store data object as.
#' @importFrom shiny req
#' @description
#'   Stores a reactive data object in shinypal's intermediate-data registry
#'   under `name`, making it available to later steps and to the assembled
#'   script. Usually called for you by [add_shinypal_data_step()].
#' @returns Called for its side effects; invisibly returns `NULL`.
#' @examples
#' \dontrun{
#' set_int_data(occs, paste0("occs_", ind))
#' }
#' @family intermediate data
#' @export
set_int_data <- function(obj, name) {
  shinypal_env <- check_setup()
  req(obj, name)
  shinypal_env$intermediate_list[[name]] <- obj
}

#' @title Get an intermediate data object
#' @param name The name of the data object to retrieve.
#' @importFrom shiny req
#' @description
#'   Retrieves a stored intermediate data reactive by name (the counterpart to
#'   [set_int_data()]). Call the returned reactive to obtain the data.
#' @returns The stored reactive; call it (e.g. `get_int_data(name)()`) to get
#'   the data. Propagates a `req()` failure if `name` is not registered.
#' @examples
#' \dontrun{
#' df <- get_int_data(input[[paste0("dataset_", ind)]])()
#' }
#' @family intermediate data
#' @export
get_int_data <- function(name) {
  shinypal_env <- check_setup()
  req(name)
  # make sure the item still exists in the list
  req(shinypal_env$intermediate_list[[name]])
  shinypal_env$intermediate_list[[name]]
}

#' @title Get the next workflow step index
#' @description
#'   Returns a unique, monotonically increasing index for a new workflow step.
#'   Each call increments a server-side counter. The counter is initialized by
#'   [shinypal_setup()] and is deliberately never reset while the session is
#'   running.
#' @returns A single positive integer to use as the new step's index.
#' @examples
#' \dontrun{
#' ind <- next_step_index()
#' }
#' @family workflow steps
#' @export
next_step_index <- function() {
  shinypal_env <- check_setup()
  shinypal_env$step_counter <- shinypal_env$step_counter + 1
  shinypal_env$step_counter
}

clear_workflow <- function() {
  shinypal_env <- check_setup()
  shinypal_env$report_list(tagList())
  shinypal_env$code_chain(list())
  shinypal_env$libraries_chain(list())
  shinypal_env$ec_subs(list())
  for (name in names(reactiveValuesToList(shinypal_env$intermediate_list))) {
    shinypal_env$intermediate_list[[name]] <- NULL
  }
  for (name in names(reactiveValuesToList(shinypal_env$include_files))) {
    shinypal_env$include_files[[name]] <- NULL
  }
  for (name in names(reactiveValuesToList(shinypal_env$var_names))) {
    shinypal_env$var_names[[name]] <- NULL
  }
}
