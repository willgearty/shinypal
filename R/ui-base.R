#' @title UI for ShinyPal
#' @description
#'   A function to create the UI for the ShinyPal app. This function should be
#'   called in the UI function of your shiny app.
#' @details
#'   Each module should have a `ui-main.R` file that defines the UI elements for
#'   that module in the "Steps" accordion.
#' @param modules A character vector of paths to independent modules.
#' @importFrom shiny actionButton downloadButton numericInput uiOutput tagList
#' @importFrom bslib card card_header layout_column_wrap layout_sidebar
#' @importFrom bslib sidebar accordion
#' @importFrom sortable sortable_js sortable_options sortable_js_capture_input
#' @importFrom htmltools css div
#' @export
shinypal_ui <- function(modules) {
  tagList(
    tags$style(
      HTML(
        "
        pre.shiny-text-output {
          white-space: pre-wrap;
          margin-bottom: 0;
        }
        .code_wrapper:not(.libraries_wrapper) pre.shiny-text-output {
          border-top-left-radius: 0;
          border-bottom-left-radius: 0;
        }
        .code_wrapper {
          position: relative;
          button {
            position: absolute;
            top: 5px;
            right: 5px;
            opacity: 0%;
            transition: all ease 0.5s;
          }
        }
        .code_wrapper:hover {
          button {
            opacity: 100%;
          }
        }
        .code_wrapper:not(.libraries_wrapper) ~ div {
          margin-top: 0.5rem;
        }
        "
      )
    ),
    layout_sidebar(
      layout_column_wrap(
        card(
          id = "steps_card",
          card_header("Possible Workflow Steps"),
          accordion(
            !!!lapply(modules, function(module) {
              if (file.exists(file.path(module, "ui-main.R"))) {
                source(file.path(module, "ui-main.R"))$value
              } else {
                NULL
              }
            }),
            open = FALSE
          )
        ),
        card(
          card_header(tagList(
            "Workflow (click and drag headers to reorder)",
            div(
              actionButton("close_steps", label = "Collapse all steps",
                           class = "btn-sm"),
              actionButton("clear_steps", label = "Remove all steps",
                           class = "btn-sm"),
              class = "btn-group float-end"
            )
          )),
          accordion(id = "workflow_accordion"),
          div(numericInput("accordion_version", label = NULL, value = 1),
              style = css(display = "none")),
          sortable_js("workflow_accordion",
                      options = sortable_options(
                        onSort =
                          sortable_js_capture_input(
                            input_id = "workflow_sortable"
                          ),
                        handle = ".accordion-header"
                      ))
        ),
        style = css(grid_template_columns = "1fr 2fr")
      ),
      sidebar = sidebar(
        downloadButton("download_script", "Download script"),
        uiOutput("report"),
        width = "30%", position = "right"
      )
    )
  )
}
