#' @title Add a step to the report, workflow, and the code chain
#' @description
#'   Registers a single step with shinypal's reactive state. It inserts the
#'   step's panel into the workflow accordion, appends the step's block to
#'   the report, adds the step's quoted code to the code chain used to
#'   assemble the reproducible script, and records any packages the step
#'   needs. When `ec_subs` is supplied, it also registers an expansion-context
#'   substitution so the downloadable script can swap in alternate code (for
#'   example, fully-qualified calls).
#'
#'   Most data-producing steps should instead use [add_shinypal_data_step()],
#'   which wraps this together with the data storage, code output, and selector
#'   boilerplate. Call `add_shinypal_step()` directly for steps that don't fit
#'   that pattern.
#' @param input The shiny input object.
#' @param ind The index of the step.
#' @param fun_workflow A function that generates the UI elements for the
#'   workflow.
#' @param fun_report A function that generates the UI elements for the report.
#' @param code_chain_list A list of quoted code bits that will be added to
#'   code_chain().
#' @param libs A character vector of R packages required for this step.
#' @param ec_subs An optional list of length 2, where the first element is a
#'   metaReactive object and the second element is a callback function. This is
#'   used to substitute expansion contexts in the code chain.
#' @importFrom shiny req observeEvent showNotification
#' @importFrom shiny reactiveValuesToList
#' @importFrom htmltools div tags
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

  colors <- get_colors(ind)

  # add the UI elements to the workflow
  accordion_panel_insert("workflow_accordion", panel = tagList(
    fun_workflow(ind),
    tags$script(paste0(
      "$('[data-rank-id=step_", ind, "] .accordion-button')",
      ".css('background-color', '", colors$background, "')",
      ".css('color', '", colors$color, "');"
    ))
  ))
  accordion_panel_open("workflow_accordion", values = paste0("step_", ind))

  # add the UI elements to the report
  tmp_list <- shinypal_env$report_list()
  tmp_list[[paste0("step_", ind)]] <- div(
    fun_report(ind),
    style = paste0("border-left: 5px solid ", colors$background, "; ",
                   "border-radius: 0.375rem;")
  )
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
    # refuse to remove a step whose output another step still consumes
    my_dfs <- grep(paste0("_", ind, "$"),
                   names(reactiveValuesToList(shinypal_env$intermediate_list)),
                   value = TRUE)
    dependents <- Filter(function(step_name) {
      if (is.na(step_name)) return(FALSE)
      other_ind <- sub("^step_", "", step_name)
      if (identical(other_ind, as.character(ind))) return(FALSE)
      sel <- input[[paste0("dataset_", other_ind)]]
      !is.null(sel) && sel %in% my_dfs
    }, names(shinypal_env$code_chain()))
    if (length(dependents) > 0) {
      showNotification(
        paste0("Can't remove this step: its data is used by ",
               length(dependents),
               if (length(dependents) > 1) " later steps." else " later step.",
               " Repoint or remove those first."),
        type = "warning"
      )
      return()
    }
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
    for (name in grep(paste0("_", ind, "$"),
                      names(reactiveValuesToList(
                        shinypal_env$intermediate_list
                      )),
                      value = TRUE)) {
      shinypal_env$intermediate_list[[name]] <- NULL
    }
    # remove included files
    for (name in grep(paste0("_", ind, "$"),
                      names(reactiveValuesToList(shinypal_env$include_files)),
                      value = TRUE)) {
      shinypal_env$include_files[[name]] <- NULL
    }
    # if there are no steps left, clear the workflow as a final precaution
    if (length(shinypal_env$report_list()) == 0) {
      clear_workflow()
    }
  }, ignoreInit = TRUE)
}

#' @title Register a complete data-producing step
#' @description
#'   Convenience wrapper around the boilerplate shared by every step that
#'   produces an intermediate dataset: it stores the data reactive, wires
#'   the data-preview modal, registers the step via [add_shinypal_step()],
#'   and (optionally) keeps the dataset dropdown and column selectors in
#'   sync. A module only supplies the `data` reactive and its UI/report
#'   functions.
#' @param input The shiny input object.
#' @param output The shiny output object.
#' @param ind The index of the step.
#' @param data A [shinymeta::metaReactive2()] object produced by the step. It
#'   should use `varname = paste0("occs_", ind)` so its generated variable name
#'   matches the stored name.
#' @param fun_workflow A function that generates the UI elements for the
#'   workflow.
#' @param fun_report A function that generates the UI elements for the report.
#' @param libs A character vector of R packages required for this step.
#' @param code_guard An optional zero-argument function evaluated inside the
#'   code output before [get_chunk()]. Use it to surface step-specific
#'   `validate()` messages; plain `req()`s inside `data` already propagate
#'   through [get_chunk()], so simple steps can leave this `NULL`.
#' @param ec_subs An optional list of length 2, where the first element is a
#'   metaReactive object and the second element is a callback function. This is
#'   used to substitute expansion contexts in the code chain.
#' @param select_dataset Whether the step consumes an upstream dataset (and thus
#'   needs a [df_select_observe()] to keep its dataset dropdown current).
#' @param column_ids A character vector of column-selector input ids to keep in
#'   sync via [column_select_observe()] (one per [select_column_input()]).
#' @importFrom shiny renderPrint
#' @importFrom rlang expr inject !!
#' @export
add_shinypal_data_step <- function(input, output, ind, data,
                                   fun_workflow, fun_report,
                                   libs = character(0), code_guard = NULL,
                                   ec_subs = NULL, select_dataset = FALSE,
                                   column_ids = character(0)) {
  check_setup()
  # every data step stores its result under occs_<ind> and previews it the same
  # way
  name <- paste0("occs_", ind)
  set_int_data(data, name)

  # render the step's generated code; run the optional guard first so any
  # step-specific validate() messages show before get_chunk() is reached
  output[[paste0("code_", ind)]] <- renderPrint({
    if (!is.null(code_guard)) code_guard()
    get_chunk(ind)
  })

  clip_observe(input, output, ind, expr(get_chunk(ind)))
  df_modal_observe(input, output, ind, name)

  # register the step with the standard "materialise this step's data" preview
  add_shinypal_step(
    input, ind, fun_workflow, fun_report,
    list(inject(quote(invisible(get_int_data(paste0("occs_", !!ind))())))),
    libs, ec_subs
  )

  # keep the dataset dropdown and any column selectors up to date
  if (select_dataset) df_select_observe(input, ind)
  for (id in column_ids) column_select_observe(input, ind, id)
}

#' @title Register a complete plot-producing step
#' @description
#'   Convenience wrapper around the boilerplate shared by every step that
#'   renders a plot: it assigns the rendered plot to its output slot, renders
#'   the step's generated code, registers the step via [add_shinypal_step()],
#'   and (optionally) keeps the dataset dropdown and column selectors in sync. A
#'   module only supplies the `plot` render and its UI/report functions.
#'
#'   Unlike [add_shinypal_data_step()], a plot step does not store an
#'   intermediate dataset or show a data-preview modal; its result is a figure,
#'   not a selectable data.frame.
#' @param input The shiny input object.
#' @param output The shiny output object.
#' @param ind The index of the step.
#' @param plot A [shinymeta::metaRender2()] object that renders the plot.
#' @param fun_workflow A function that generates the UI elements for the
#'   workflow.
#' @param fun_report A function that generates the UI elements for the report.
#' @param libs A character vector of R packages required for this step.
#' @param code_guard An optional zero-argument function evaluated inside the
#'   code output before [get_chunk()]. Use it to surface step-specific
#'   `validate()` messages.
#' @param output_prefix The prefix for the plot's output slot, combined with
#'   `ind` to form the id (default `"plot_"`, giving `plot_<ind>`). Must match
#'   the [shiny::plotOutput()] id used in `fun_report` (e.g. `"map_"`).
#' @param select_dataset Whether the step consumes an upstream dataset (and thus
#'   needs a [df_select_observe()] to keep its dataset dropdown current).
#' @param column_ids A character vector of column-selector input ids to keep in
#'   sync via [column_select_observe()] (one per [select_column_input()]).
#' @importFrom shiny renderPrint
#' @importFrom rlang expr inject !!
#' @export
add_shinypal_plot_step <- function(input, output, ind, plot,
                                   fun_workflow, fun_report,
                                   libs = character(0), code_guard = NULL,
                                   output_prefix = "plot_",
                                   select_dataset = TRUE,
                                   column_ids = character(0)) {
  check_setup()
  # the rendered plot lives in output[[<output_prefix><ind>]]; the report's
  # plotOutput() and the code chain reference that same slot
  output[[paste0(output_prefix, ind)]] <- plot

  # render the step's generated code; run the optional guard first so any
  # step-specific validate() messages show before get_chunk() is reached
  output[[paste0("code_", ind)]] <- renderPrint({
    if (!is.null(code_guard)) code_guard()
    get_chunk(ind)
  })

  clip_observe(input, output, ind, expr(get_chunk(ind)))

  # register the step; the code chain renders this step's plot output
  add_shinypal_step(
    input, ind, fun_workflow, fun_report,
    list(inject(quote(output[[paste0(!!output_prefix, !!ind)]]()))),
    libs
  )

  # keep the dataset dropdown and any column selectors up to date
  if (select_dataset) df_select_observe(input, ind)
  for (id in column_ids) column_select_observe(input, ind, id)
}

# Start at purple instead of off-white
# https://cran.r-project.org/web/packages/khroma/vignettes/tol.html#rainbow
rainbow_palette <- khroma::color("smooth rainbow")(100, range = c(0.25, 1))

#' Generate a background color and font color for a step based on its index
#' @param ind The index of the step.
#' @returns A list containing the `color` and `background` CSS styles for the
#'   step.
#' @importFrom grDevices col2rgb
#' @export
get_colors <- function(ind) {
  # get last two digits of the index
  digits <- ind %% 100
  # skip 22 colors between each sequential step
  bkgd <- rainbow_palette[(digits * 23) %% 100]
  # determine font color based on luminance
  # https://stackoverflow.com/questions/1855884
  luminance <- c(.299, .587, .114) %*% col2rgb(bkgd) / 255
  col <- if (luminance < 0.5) "white" else "black"
  return(list(color = col, background = bkgd))
}
