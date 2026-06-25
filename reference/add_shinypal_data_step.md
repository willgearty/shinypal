# Register a complete data-producing step

Convenience wrapper around the boilerplate shared by every step that
produces an intermediate dataset: it stores the data reactive, wires the
data-preview modal, registers the step via
[`add_shinypal_step()`](http://williamgearty.com/shinypal/reference/add_shinypal_step.md),
and (optionally) keeps the dataset dropdown and column selectors in
sync. A module only supplies the `data` reactive and its UI/report
functions.

## Usage

``` r
add_shinypal_data_step(
  ind,
  data,
  fun_workflow,
  fun_report,
  libs = character(0),
  code_guard = NULL,
  ec_subs = NULL,
  select_dataset = FALSE,
  column_ids = character(0),
  rename = TRUE
)
```

## Arguments

- ind:

  The index of the step.

- data:

  A
  [`shinymeta::metaReactive2()`](https://rstudio.github.io/shinymeta/reference/metaReactive.html)
  object produced by the step. It should use
  `varname = paste0("occs_", ind)` so its generated variable name
  matches the stored name.

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
  Use it to surface step-specific `validate()` messages; plain `req()`s
  inside `data` already propagate through
  [`get_chunk()`](http://williamgearty.com/shinypal/reference/get_chunk.md),
  so simple steps can leave this `NULL`.

- ec_subs:

  An optional list of length 2, where the first element is a
  metaReactive object and the second element is a callback function.
  This is used to substitute expansion contexts in the code chain.

- select_dataset:

  Whether the step consumes an upstream dataset (and thus needs a
  [`df_select_observe()`](http://williamgearty.com/shinypal/reference/df_select_observe.md)
  to keep its dataset dropdown current).

- column_ids:

  A character vector of column-selector input ids to keep in sync via
  [`column_select_observe()`](http://williamgearty.com/shinypal/reference/column_select_observe.md)
  (one per
  [`select_column_input()`](http://williamgearty.com/shinypal/reference/select_column_input.md)).

- rename:

  Whether to render a
  [`varname_input()`](http://williamgearty.com/shinypal/reference/varname_input.md)
  text field in the step's panel (and install
  [`var_name_observe()`](http://williamgearty.com/shinypal/reference/var_name_observe.md))
  so the user can give this step's dataset a custom name, used as its
  label in later selectors and its variable name in the generated
  script. Defaults to `TRUE`.

## Value

Called for its side effects and returns `NULL` invisibly. Stores the
step's data reactive under `occs_<ind>`, wires its code output and
data-preview modal, registers the step via
[`add_shinypal_step()`](http://williamgearty.com/shinypal/reference/add_shinypal_step.md),
and (when requested) installs the dataset/column selector observers and,
when `rename = TRUE`, the dataset-rename field and its observer.

## See also

Other workflow steps:
[`add_shinypal_plot_step()`](http://williamgearty.com/shinypal/reference/add_shinypal_plot_step.md),
[`add_shinypal_step()`](http://williamgearty.com/shinypal/reference/add_shinypal_step.md),
[`next_step_index()`](http://williamgearty.com/shinypal/reference/next_step_index.md)

## Examples

``` r
if (FALSE) { # \dontrun{
# inside a module's server.R
ind <- next_step_index()
occs <- shinymeta::metaReactive2(varname = paste0("occs_", ind), {
  shinymeta::metaExpr(head(mtcars, input[[paste0("n_", ind)]]))
})
add_shinypal_data_step(
  ind, data = occs,
  fun_workflow = function(ind) accordion_panel_remove_button(ind, "Subset"),
  fun_report = function(ind) verbatimTextOutput_copy(ind)
)
} # }
```
