# Building an app with shinypal

shinypal turns a [Shiny](https://shiny.posit.co/) app into a
point-and-click workflow builder: the user assembles a pipeline step by
step, and shinypal shows (and exports) the equivalent reproducible R
code via [shinymeta](https://rstudio.github.io/shinymeta/). This
vignette walks through building your own.

A shinypal app has two parts:

1.  a thin **host app** that lays out the shinypal interface and
    initializes it, and
2.  a directory of **modules**, one per grouping of analysis steps, that
    you write.

``` r

library(shinypal)
```

## The host app

The host calls
[`shinypal_ui()`](http://williamgearty.com/shinypal/reference/shinypal_ui.md)
in its UI and
[`shinypal_setup()`](http://williamgearty.com/shinypal/reference/shinypal_setup.md)
at the top of its server function. Both take a vector of paths to your
module directories.

``` r

modules <- list.dirs("modules", recursive = FALSE)

ui <- bslib::page_navbar(
  title = "My workflow builder",
  bslib::nav_panel("Build", shinypal_ui(modules))
)

server <- function(input, output, session) {
  shinypal_setup(
    input, output, session,
    modules = modules,
    download_template = "modules/report.qmd"
  )
}

shiny::shinyApp(ui, server)
```

[`shinypal_ui()`](http://williamgearty.com/shinypal/reference/shinypal_ui.md)
renders two cards (the menu of *possible* steps on the left and the
draggable *workflow* on the right) plus a sidebar holding the live
report and a download button.
[`shinypal_setup()`](http://williamgearty.com/shinypal/reference/shinypal_setup.md)
initializes the shared reactive state, wires the report and download
handler, and sources each module’s `ui-aux.R` and `server.R`.

## Anatomy of a module

Each module is a folder containing at least three files:

| File | Sourced by | Purpose |
|----|----|----|
| `ui-main.R` | [`shinypal_ui()`](http://williamgearty.com/shinypal/reference/shinypal_ui.md) | the panel advertising the possible steps included in the module |
| `ui-aux.R` | [`shinypal_setup()`](http://williamgearty.com/shinypal/reference/shinypal_setup.md) | defines the `fun_workflow` and `fun_report` helpers for the steps |
| `server.R` | [`shinypal_setup()`](http://williamgearty.com/shinypal/reference/shinypal_setup.md) | defines the server logic for each step to the workflow |

`ui-aux.R` and `server.R` are sourced with `local = TRUE`, so they share
the host’s `input`/`output`/`session` and can see each other’s
definitions.

### `ui-main.R`

This file is *evaluated* and its value becomes a panel in the menu, so
it should return a single \[bslib::accordion_panel()\]. Give it an
action button the server will listen for.

``` r

bslib::accordion_panel(
  "Filter rows",
  "Keep only the rows that match a condition.",
  shiny::actionButton("add_filter", "Add this step")
)
```

### `ui-aux.R`

Defines two functions for each step with index `ind`: one that draws the
step’s panel in the workflow accordion, and one that draws its block in
the report. Use the shinypal UI helpers so the panel gets a remove
button, a dataset selector, and a copy-able code block for free.

``` r

filter_workflow <- function(ind) {
  accordion_panel_remove_button(
    ind, "Filter rows",
    select_dataset_input(ind),
    select_column_input(ind, "Column to filter on:"),
    shiny::numericInput(paste0("min_", ind), "Minimum value", value = 0),
    df_modal_button(ind)
  )
}

filter_report <- function(ind) {
  verbatimTextOutput_copy(ind)
}
```

### `server.R`

When the *Add* button is clicked, grab a fresh index with
[`next_step_index()`](http://williamgearty.com/shinypal/reference/next_step_index.md),
build a \[shinymeta::metaReactive2()\] that both *computes* the result
and *records* the code that produced it, then register everything with
[`add_shinypal_data_step()`](http://williamgearty.com/shinypal/reference/add_shinypal_data_step.md).

``` r

shiny::observeEvent(input$add_filter, {
  ind <- next_step_index()

  # metaReactive2 returns the data when called, and its code when expanded.
  # Name it occs_<ind> so shinypal can find it as an intermediate dataset.
  data <- shinymeta::metaReactive2(varname = paste0("occs_", ind), {
    df  <- get_int_data(input[[paste0("dataset_", ind)]])()
    col <- input[[paste0("column_", ind)]]
    shiny::req(col)
    shinymeta::metaExpr(
      dplyr::filter(df, !!col >= !!input[[paste0("min_", ind)]])
    )
  })

  add_shinypal_data_step(
    input, output, ind,
    data         = data,
    fun_workflow = filter_workflow,
    fun_report   = filter_report,
    libs         = "dplyr",
    select_dataset = TRUE,
    column_ids   = paste0("column_", ind)
  )
})
```

[`add_shinypal_data_step()`](http://williamgearty.com/shinypal/reference/add_shinypal_data_step.md)
stores the result under `occs_<ind>`, renders its generated code into
the report, wires the “view data” modal, and (because
`select_dataset = TRUE`) keeps the dataset dropdown in sync as other
steps come and go. A later step can then consume this step’s output by
calling
[`get_int_data()`](http://williamgearty.com/shinypal/reference/get_int_data.md).

## Plot steps

A step that produces a figure instead of a dataset uses
[`add_shinypal_plot_step()`](http://williamgearty.com/shinypal/reference/add_shinypal_plot_step.md)
with a \[shinymeta::metaRender2()\] render. Point `output_prefix` at the
`plotOutput()` id used in the report.

``` r

plot_render <- shinymeta::metaRender2(shiny::renderPlot, {
  df <- get_int_data(input[[paste0("dataset_", ind)]])()
  shinymeta::metaExpr(plot(df))
})

add_shinypal_plot_step(
  input, output, ind,
  plot          = plot_render,
  fun_workflow  = plot_workflow,
  fun_report    = function(ind) shiny::plotOutput(paste0("plot_", ind)),
  output_prefix = "plot_"
)
```

## The report template

`shinypal_setup(download_template = ...)` points at a Quarto/R Markdown
template with two knitr-expandable variables, `{{libraries}}` and
`{{code}}`, which shinypal fills with the assembled
[`library()`](https://rdrr.io/r/base/library.html) calls and step code.
Files a user uploads through a \[shiny::fileInput()\] can be bundled
into the download by registering them with
[`file_observe()`](http://williamgearty.com/shinypal/reference/file_observe.md).

## Deploying to the browser with shinylive

shinypal apps can be exported with
[shinylive](https://posit-dev.github.io/r-shinylive/) so they run
entirely in the browser via webR. Because some things behave differently
there (e.g., no system `zip`, and packages without a WebAssembly build
can’t load), use
[`is_shinylive()`](http://williamgearty.com/shinypal/reference/is_shinylive.md)
to gate that behavior:

``` r

# only attempt to load a package that has no WebAssembly build outside webR
if (!is_shinylive() && requireNamespace("paleobioDB", quietly = TRUE)) {
  library(paleobioDB)
}
```
