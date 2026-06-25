#' @title UI for ShinyPal
#' @description
#'   A function to create the UI for the ShinyPal app. This function should be
#'   called in the UI function of your shiny app.
#' @details
#'   Each module should have a `ui-main.R` file that defines the UI elements for
#'   that module in the "Steps" accordion.
#' @param modules A character vector of paths to independent modules.
#' @importFrom shiny actionButton includeCSS includeScript uiOutput tagList
#' @importFrom bslib card card_header layout_column_wrap layout_sidebar
#' @importFrom bslib sidebar accordion
#' @importFrom sortable sortable_js sortable_options sortable_js_capture_input
#' @importFrom htmltools css div
#' @returns A [shiny::tagList()] holding the shinypal interface: a sidebar
#'   layout with the "Possible Workflow Steps" and "Workflow" cards on the left
#'   and the live report/download sidebar on the right. Drop it into your app's
#'   UI, for example inside a [bslib::page_navbar()] panel.
#' @examples
#' \dontrun{
#' ui <- function() {
#'   modules <- list.dirs("./modules", recursive = FALSE)
#'   bslib::page_navbar(title = "My app", bslib::nav_panel("Build", shinypal_ui(modules)))
#' }
#' }
#' @family app setup
#' @export
shinypal_ui <- function(modules) {
  tagList(
    includeCSS(system.file("www/styles.css", package = "shinypal")),
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
          ), class = "justify-content-between"),
          accordion(id = "workflow_accordion"),
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
        div(
          downloadButton("download_script", "Download script"),
          class = "shinypal-download" # sticky
        ),
        uiOutput("report"),
        width = "30%", position = "right",
        # use a custom resizable handle instead of the bslib built-in option
        resizable = FALSE, class = "shinypal-resizable"
      )
    ),
    includeScript(system.file("www/resize-handle.js", package = "shinypal"))
  )
}

downloadButton <- function(...) {
  tag <- shiny::downloadButton(...)
  # fix downloads with shinylive on Chromium browsers
  # https://github.com/posit-dev/r-shinylive/issues/74
  tag$attribs$download <- NULL
  # drop target = "_blank" so the download doesn't pop open a new browser
  # https://github.com/rstudio/shiny/issues/2020
  tag$attribs$target <- NULL
  tag
}
