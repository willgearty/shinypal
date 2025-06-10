#' @title Setup shinypal
#' @description
#'   A function to set up the shinypal environment. This function should be
#'   called at the top of the server function of your shiny app.
#' @details
#'   Each module should have a `ui-aux.R` file that defines the UI elements for
#'   that module in the "Workflow" accordion and a `server.R` file that defines
#'   the server-side logic for the module.
#' @param input The shiny input object.
#' @param output The shiny output object.
#' @param session The shiny session object.
#' @param modules A character vector of paths to independent modules.
#' @param download_filename The name of the file to download when the user
#'   clicks the download button.
#' @param download_template The path to the Quarto markdown template file that
#'   will be used to generate the report.
#' @importFrom shinymeta newExpansionContext expandChain
#' @importFrom shiny reactiveVal reactiveValues reactiveValuesToList
#' @importFrom shiny verbatimTextOutput renderUI renderPrint observeEvent
#' @importFrom shiny downloadHandler actionButton icon
#' @importFrom bslib accordion_panel_remove accordion_panel_close
#' @importFrom purrr list_flatten
#' @importFrom clipr write_clip
#' @importFrom rlang inject !!!
#' @importFrom htmltools div tagList
#' @export
shinypal_setup <- function(input, output, session, modules,
                           download_filename = "shinypal_script.zip",
                           download_template = "./modules/test_report.qmd") {
  # shared server objects ####
  # a shared expansion context for all expandChain() calls
  shinypal_env$shared_ec <- newExpansionContext()

  # a named list of quoted library() calls that go at the very top of the report
  # each element may also be a list of quoted calls that will be flattened
  # don't worry about duplicates, they will be filtered out
  shinypal_env$libraries_chain <- reactiveVal(list())

  # a named list of quoted code bits that will go after the libraries are loaded
  # each element may also be a list of quoted code bits that will be flattened
  shinypal_env$code_chain <- reactiveVal(list())

  # ordered, named list of elements in the report
  shinypal_env$report_list <- reactiveVal(tagList())

  # unordered, named list of intermediate variables that need to be global for
  # assembly of the markdown file
  shinypal_env$intermediate_list <- reactiveValues()

  # ordered, named list of expansionContext substitutions
  shinypal_env$ec_subs <- reactiveVal(list())

  # unordered, named list of files to include in the download bundle
  shinypal_env$include_files <- reactiveValues()

  options(spinner.type = 8)

  # render dynamic UI ####
  # add libraries to load at the beginning of the report
  output$libraries <- renderPrint({
    inject(expandChain(
      !!!shinypal_env$libraries_chain() |>
        unname() |>
        list_flatten() |>
        unique()
    ))
  })

  # render the report
  output$report <- renderUI({
    tagList(
      div(
        div(
          verbatimTextOutput("libraries"),
          actionButton("copy_libraries", icon("copy")),
          class = "code_wrapper libraries_wrapper"
        )
      ),
      shinypal_env$report_list()
    )
  })
  observeEvent(input$copy_libraries, {
    write_clip(
      inject(expandChain(
        !!!shinypal_env$libraries_chain() |>
          unname() |>
          list_flatten() |>
          unique()
      )),
      allow_non_interactive = TRUE
    )
  })

  # handle workflow reordering
  observeEvent(input$workflow_sortable, {
    new_order <- isolate(input$workflow_sortable)
    tmp_list <- shinypal_env$report_list()
    shinypal_env$report_list(tmp_list[new_order])
    tmp_list <- shinypal_env$code_chain()
    shinypal_env$code_chain(tmp_list[new_order])
  }, ignoreInit = TRUE)

  # report download ####
  # handle downloading a zip folder with the markdown script and rendered files
  output$download_script <- downloadHandler(
    filename = download_filename,
    content = function(file) {
      # make a new expansion context for the report with all the substitutions
      ec <- newExpansionContext()
      for(ec_sub in shinypal_env$ec_subs()) {
        inject(ec$substituteMetaReactive(!!!ec_sub))
      }
      buildRmdBundle(
        download_template,
        file,
        vars = list(
          # lists of quoted code bits, need to be injected into expandChain()
          libraries = inject(
            expandChain(
              !!!shinypal_env$libraries_chain() |>
                unname() |>
                list_flatten() |>
                unique()
            )),
          code = inject(
            expandChain(
              !!!shinypal_env$code_chain() |>
                unname() |>
                list_flatten(),
              .expansionContext = ec
            ))
        ),
        include_files = reactiveValuesToList(shinypal_env$include_files) |>
          unname() |>
          Filter(f = Negate(is.null)) |>
          list_flatten(),
        render_args = list(output_format = c("html_document", "pdf_document"))
      )
    }
  )

  # observers ####
  observeEvent(input$clear_steps, {
    sapply(names(shinypal_env$report_list()), function(el) {
      accordion_panel_remove("workflow_accordion", target = el)
    })
    clear_workflow()
  }, ignoreInit = TRUE)

  observeEvent(input$close_steps, {
    accordion_panel_close("workflow_accordion", values = TRUE)
  }, ignoreInit = TRUE)

  # load the dynamic bits for each module (ui-aux.R and server.R)
  # make sure local = TRUE so they all share a namespace with the main app
  sapply(modules, FUN = function(module) {
    ui_aux_file <- file.path(module, "ui-aux.R")
    if (file.exists(ui_aux_file)) source(ui_aux_file, local = TRUE)
    server_file <- file.path(module, "server.R")
    if (file.exists(server_file)) source(server_file, local = TRUE)
  })

  observe({
    # observe any changes
    inp <- reactiveValuesToList(input)
    # reset the expansion context and add substitutes
    shinypal_env$shared_ec <- newExpansionContext()
    for(ec_sub in shinypal_env$ec_subs()) {
      inject(shinypal_env$shared_ec$substituteMetaReactive(!!!ec_sub))
    }
  }, priority = 10000)

  shinypal_env$setup <- TRUE
}

# the following is copied from https://github.com/rstudio/shinymeta/blob/v0.2.1/R/report.R
# modifications allow it to work with shinylive
buildRmdBundle <- function(report_template, output_zip_path, vars = list(),
                           include_files = list(), render = TRUE,
                           render_args = list()) {

  force(report_template)
  force(vars)

  shinymeta:::with_progress_obj(function(progress) {
    progress$set(value = 0)
    progress$set(message = "Generating code")

    if (is.list(vars)) {
      vars <- lapply(vars, function(x) {
        if (is.language(x)) {
          paste(formatCode(x), collapse = "\n")
        } else {
          x
        }
      })
    }

    progress$set(value = 0.1)
    progress$set(message = "Expanding Rmd template")

    rmd_source <- shinymeta:::knit_expand_safe(report_template, vars = vars)
    rmd_filename <- shinymeta:::template_rename(report_template, "Rmd")

    build_bundle(rmd_source, rmd_filename, output_zip_path,
                 include_files = include_files, render = render,
                 render_args = render_args, progress = progress)
  })
}

build_bundle <- function(input_src, input_filename, output_zip_path,
                         include_files = list(), render = TRUE,
                         render_args = list(), progress) {
  force(input_src)
  force(input_filename)
  force(output_zip_path)
  force(include_files)
  force(render)
  force(render_args)

  # TODO: validate args
  progress$set(value = 0.2)
  progress$set(message =  "Adding items to zip archive")

  x <- shinymeta:::zip_archive()

  dest_filename_full <- fs::path(shinymeta:::archive_basedir(x), input_filename)

  # TODO: Verify UTF-8 encoding is preserved
  writeLines(input_src, dest_filename_full)

  shinymeta:::add_items(x, !!!include_files)

  progress$set(value = 0.3)

  if (render) {
    progress$set(message =  "Rendering report")
    # WG: fork = TRUE doesn't work with shinylive because of callr::r
    shinymeta:::render_with_args(dest_filename_full, render_args,
                                 fork = R.Version()$os != "emscripten")
  }

  progress$set(value = 0.9)
  progress$set(message =  "Compressing bundle")
  archive <- shinymeta:::build_archive(x, output_zip_path)
  progress$set(value = 1)
  archive
}
