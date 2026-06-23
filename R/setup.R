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
#' @importFrom shinymeta newExpansionContext expandChain formatCode
#' @importFrom shiny reactive reactiveVal reactiveValues reactiveValuesToList
#' @importFrom shiny verbatimTextOutput renderUI renderPrint observeEvent
#' @importFrom shiny downloadHandler actionButton icon showNotification
#' @importFrom bslib accordion_panel_remove accordion_panel_close
#' @importFrom purrr list_flatten
#' @importFrom clipr write_clip
#' @importFrom rlang inject !!!
#' @importFrom htmltools div tagList
#' @importFrom rmarkdown pandoc_available
#' @returns Called for its side effects and returns `NULL` invisibly. It
#'   initializes shinypal's shared reactive state, renders the live report and
#'   wires the download handler and workflow observers, then sources each
#'   module's `ui-aux.R` and `server.R` so their steps become available.
#' @examples
#' \dontrun{
#' server <- function(input, output, session) {
#'   modules <- list.dirs("./modules", recursive = FALSE)
#'   shinypal_setup(input, output, session, modules)
#' }
#' }
#' @export
shinypal_setup <- function(input, output, session, modules,
                           download_filename = "shinypal_script.zip",
                           download_template = "./modules/test_report.qmd") {
  # set up per-session state store
  session$userData$shinypal <- new.env(parent = emptyenv())
  shinypal_env <- session$userData$shinypal

  # stash input/output so the helpers can read them from the env instead of
  # taking them as arguments
  shinypal_env$input <- input
  shinypal_env$output <- output

  # shared server objects ####

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

  # monotonically increasing counter that hands out unique step indices via
  # next_step_index()
  shinypal_env$step_counter <- 0

  options(spinner.type = 8)

  # render dynamic UI ####
  # add libraries to load at the beginning of the report
  libraries_expr <- reactive({
    inject(expandChain(
      !!!shinypal_env$libraries_chain() |> unname() |> list_flatten() |> unique()
    ))
  })
  output$libraries <- renderPrint({ libraries_expr() })

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
    write_clip(libraries_expr(), allow_non_interactive = TRUE)
  })

  # handle workflow reordering
  observeEvent(input$workflow_sortable, {
    new_order <- isolate(input$workflow_sortable)
    # drop any NA-named entries and stale/unknown ids from the sortable capture
    reorder <- function(lst) {
      nm <- names(lst)
      if (is.null(nm)) return(lst)
      lst <- lst[!is.na(nm)]
      keep <- new_order[new_order %in% names(lst)]
      lst[c(keep, setdiff(names(lst), keep))]
    }
    shinypal_env$report_list(reorder(shinypal_env$report_list()))
    shinypal_env$code_chain(reorder(shinypal_env$code_chain()))
  }, ignoreInit = TRUE)

  # report download ####
  # handle downloading a zip folder with the markdown script and rendered files
  output$download_script <- downloadHandler(
    filename = download_filename,
    content = function(file) {
      # any unexpected failure (e.g., rendering) is reported to the user instead
      # of producing a broken or empty download
      tryCatch({
        chunks <- shinypal_env$chunks()
        failed <- vapply(chunks, inherits, logical(1), "condition")
        code <- vapply(chunks, function(chunk) {
          if (inherits(chunk, "condition")) {
            paste("# --- step omitted: incomplete or errored;",
                  "fix or remove it in the app, then re-download ---")
          } else {
            paste(formatCode(chunk), collapse = "\n")
          }
        }, character(1))

        buildRmdBundle(
          download_template,
          file,
          vars = list(
            libraries = libraries_expr(),
            code = paste(code, collapse = "\n\n")
          ),
          include_files = reactiveValuesToList(shinypal_env$include_files) |>
            unname() |>
            Filter(f = Negate(is.null)) |>
            list_flatten(),
          # need pandoc to render the rmarkdown file
          render = pandoc_available(),
          render_args = list(output_format = c("html_document", "pdf_document"))
        )

        # let the user know if any steps didn't make it into the script
        if (any(failed)) {
          showNotification(
            paste(sum(failed), "step(s) were incomplete or errored and were",
                  "left out of the script as comments. Fix or remove them in",
                  "the app for a complete script."),
            type = "warning", duration = 10
          )
        }
      }, error = function(e) {
        showNotification(
          paste("The script could not be generated:", conditionMessage(e)),
          type = "error", duration = NULL
        )
      })
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

  # a reactive that builds all interactive code chunks together, using one
  # expansion context whose lifetime is one render cycle of this reactive.
  # every chunk sees the same shared context, populated in code_chain() order,
  # whenever any underlying dependency changes, every chunk is rebuilt together.
  shinypal_env$chunks <- reactive({
    ec <- newExpansionContext()
    for (ec_sub in shinypal_env$ec_subs()) {
      inject(ec$substituteMetaReactive(!!!ec_sub))
    }
    chain <- shinypal_env$code_chain()

    result <- list()
    for (step_name in names(chain)) {
      step_code <- chain[[step_name]]
      # tryCatch lets per-step req()/validate() failures be surfaced
      # individually by the consuming output, rather than invalidating
      # the whole chunks reactive
      result[[step_name]] <- tryCatch(
        inject(expandChain(!!!step_code, .expansionContext = ec)),
        error = function(e) e
      )
    }
    result
  })

  shinypal_env$setup <- TRUE
}

#' @title Detect a shinylive (webR) session
#' @description
#'   Returns `TRUE` when the app is running in a shinylive/webR session (i.e.
#'   compiled to WebAssembly via Emscripten) and `FALSE` in a normal R session.
#'   Useful for gating behavior that can't run in the browser, such as loading
#'   packages with no WebAssembly build or bundling a downloadable zip.
#' @return A length-1 logical: `TRUE` under shinylive/webR, otherwise `FALSE`.
#' @examples
#' is_shinylive()
#' @export
is_shinylive <- function() { R.Version()$os == "emscripten" }

