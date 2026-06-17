# UI for ShinyPal

A function to create the UI for the ShinyPal app. This function should
be called in the UI function of your shiny app.

## Usage

``` r
shinypal_ui(modules)
```

## Arguments

- modules:

  A character vector of paths to independent modules.

## Value

A
[`shiny::tagList()`](https://rstudio.github.io/htmltools/reference/tagList.html)
holding the shinypal interface: a sidebar layout with the "Possible
Workflow Steps" and "Workflow" cards on the left and the live
report/download sidebar on the right. Drop it into your app's UI, for
example inside a
[`bslib::page_navbar()`](https://rstudio.github.io/bslib/reference/page_navbar.html)
panel.

## Details

Each module should have a `ui-main.R` file that defines the UI elements
for that module in the "Steps" accordion.

## Examples

``` r
if (FALSE) { # \dontrun{
ui <- function() {
  modules <- list.dirs("./modules", recursive = FALSE)
  bslib::page_navbar(title = "My app", bslib::nav_panel("Build", shinypal_ui(modules)))
}
} # }
```
