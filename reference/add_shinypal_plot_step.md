# Register a complete plot-producing step

Convenience wrapper around the boilerplate shared by every step that
renders a plot: it assigns the rendered plot to its output slot, renders
the step's generated code, registers the step via
[`add_shinypal_step()`](http://williamgearty.com/shinypal/reference/add_shinypal_step.md),
and (optionally) keeps the dataset dropdown and column selectors in
sync. A module only supplies the `plot` render and its UI/report
functions.

Unlike
[`add_shinypal_data_step()`](http://williamgearty.com/shinypal/reference/add_shinypal_data_step.md),
a plot step does not store an intermediate dataset or show a
data-preview modal; its result is a figure, not a selectable data.frame.

## Usage

``` r
add_shinypal_plot_step(
  ind,
  plot,
  fun_workflow,
  fun_report,
  libs = character(0),
  code_guard = NULL,
  output_prefix = "plot_",
  select_dataset = TRUE,
  column_ids = character(0)
)
```

## Arguments

- ind:

  The index of the step.

- plot:

  A
  [`shinymeta::metaRender2()`](https://rstudio.github.io/shinymeta/reference/metaRender.html)
  object that renders the plot.

- fun_workflow:

  A function that generates the UI elements for the workflow.

- fun_report:

  A function that generates the UI elements for the report.

- libs:

  A character vector of R packages required for this step.

- code_guard:

  An optional zero-argument function evaluated inside the code output
  before
  [`get_chunk()`](http://williamgearty.com/shinypal/reference/get_chunk.md).
  Use it to surface step-specific `validate()` messages.

- output_prefix:

  The prefix for the plot's output slot, combined with `ind` to form the
  id (default `"plot_"`, giving `plot_<ind>`). Must match the
  [`shiny::plotOutput()`](https://rdrr.io/pkg/shiny/man/plotOutput.html)
  id used in `fun_report` (e.g. `"map_"`).

- select_dataset:

  Whether the step consumes an upstream dataset (and thus needs a
  [`df_select_observe()`](http://williamgearty.com/shinypal/reference/df_select_observe.md)
  to keep its dataset dropdown current).

- column_ids:

  A character vector of column-selector input ids to keep in sync via
  [`column_select_observe()`](http://williamgearty.com/shinypal/reference/column_select_observe.md)
  (one per
  [`select_column_input()`](http://williamgearty.com/shinypal/reference/select_column_input.md)).

## Value

Called for its side effects and returns `NULL` invisibly. Assigns the
rendered plot to `output[[<output_prefix><ind>]]`, renders the step's
generated code, registers the step via
[`add_shinypal_step()`](http://williamgearty.com/shinypal/reference/add_shinypal_step.md),
and (when requested) installs the dataset/column selector observers.

## See also

Other workflow steps:
[`add_shinypal_data_step()`](http://williamgearty.com/shinypal/reference/add_shinypal_data_step.md),
[`add_shinypal_step()`](http://williamgearty.com/shinypal/reference/add_shinypal_step.md),
[`next_step_index()`](http://williamgearty.com/shinypal/reference/next_step_index.md),
[`step_varname()`](http://williamgearty.com/shinypal/reference/step_varname.md)

## Examples

``` r
if (FALSE) { # \dontrun{
# inside a module's server.R
ind <- next_step_index()
p <- shinymeta::metaRender2(shiny::renderPlot, {
  df <- get_int_data(input[[paste0("dataset_", ind)]])()
  shinymeta::metaExpr(plot(df))
})
add_shinypal_plot_step(
  ind, plot = p,
  fun_workflow = function(ind) accordion_panel_remove_button(ind, "Plot"),
  fun_report = function(ind) shiny::plotOutput(paste0("plot_", ind))
)
} # }
```
