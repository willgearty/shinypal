#' @title Add a step to the report, workflow, and the code chain
#' @description
#'   A short description...
#' @param input The input object from the shiny app.
#' @param ind The index of the step to be added.
#' @param fun_workflow A function that generates the UI elements for the
#'   workflow.
#' @param fun_report A function that generates the UI elements for the report.
#' @param code_chain_list A list of quoted code bits that will be added to
#'   code_chain().
#' @param libs A character vector of R packages required for this step.
#' @param ec_subs An optional list of length 2, where the first element is a
#'   metaReactive object and the second element is a callback function. This is
#'   used to substitute expansion contexts in the code chain.
#' @importFrom shiny req isolate observeEvent updateNumericInput
#' @importFrom shiny reactiveValuesToList
#' @importFrom bslib accordion_panel_insert accordion_panel_open
#' @importFrom bslib accordion_panel_remove
#' @importFrom rlang inject !!
#' @export
add_shinypal_step <- function(input, ind, fun_workflow, fun_report,
                              code_chain_list, libs, ec_subs = NULL) {
  check_setup()
  # this is broken for some reason???
  # probably want to use check_required instead
  #req(input, ind, fun_workflow, fun_report, code_chain_list, libs)

  # add the UI elements to the workflow
  accordion_panel_insert("workflow_accordion", panel = fun_workflow(ind))
  accordion_panel_open("workflow_accordion", values = paste0("step_", ind))

  # add the UI elements to the report
  tmp_list <- shinypal_env$report_list()
  tmp_list[[paste0("step_", ind)]] <- fun_report(ind)
  shinypal_env$report_list(tmp_list)

  # add the code to the code chain
  tmp_list <- shinypal_env$code_chain()
  tmp_list[[paste0("step_", ind)]] <- code_chain_list
  shinypal_env$code_chain(tmp_list)

  # add the packages to the libraries chain
  tmp_list <- shinypal_env$libraries_chain()
  tmp_list[[paste0("step_", ind)]] <-
    lapply(libs, function(lib) inject(quote(quote(library(!!lib)))))
  shinypal_env$libraries_chain(tmp_list)

  # add the expansion context substitutions
  if (!is.null(ec_subs)) {
    # check for proper format
    if (length(ec_subs) != 2) {
      stop("ec_subs, if not NULL, must be a list of length 2, where the first ",
           "element is a reactive and the second element is a function.")
    }
    if (!inherits(ec_subs[[1]], "shinymeta_reactive")) {
      stop("Attempted to substitute an object that wasn't a metaReactive",
           call. = FALSE)
    }
    if (!is.function(ec_subs[[2]]) || length(formals(ec_subs[[2]])) != 0) {
      stop("Substitution callback should be a function that takes 0 args",
           call. = FALSE)
    }
    tmp_list <- shinypal_env$ec_subs()
    tmp_list[[paste0("step_", ind)]] <- ec_subs
    shinypal_env$ec_subs(tmp_list)
  }

  # handle removing this step from the report and workflow
  observeEvent(input[[paste0("remove_step_", ind)]], {
    accordion_panel_remove("workflow_accordion",
                           target = paste0("step_", ind))
    for (fun in c(shinypal_env$report_list,
                  shinypal_env$code_chain,
                  shinypal_env$libraries_chain,
                  shinypal_env$ec_subs)) {
      tmp_list <- fun()
      tmp_list[[paste0("step_", ind)]] <- NULL
      fun(tmp_list)
    }
    # remove any intermediate objects for this step
    for (name in grep(paste0("*_", ind),
                      names(reactiveValuesToList(
                        shinypal_env$intermediate_list
                      )),
                      value = TRUE)) {
      shinypal_env$intermediate_list[[name]] <- NULL
    }
    # remove included files
    for (name in grep(paste0("*_", ind),
                      names(reactiveValuesToList(shinypal_env$include_files)),
                      value = TRUE)) {
      shinypal_env$include_files[[name]] <- NULL
    }
    # if there are no steps left, clear the workflow as a final precaution
    if (length(shinypal_env$report_list()) == 0) {
      clear_workflow()
    }
  }, ignoreInit = TRUE)
  updateNumericInput(inputId = "accordion_version",
                     value =
                       isolate(input$accordion_version) + 1)
}
