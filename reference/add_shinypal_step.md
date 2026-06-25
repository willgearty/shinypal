# Add a step to the report, workflow, and the code chain

Registers a single step with shinypal's reactive state. It inserts the
step's panel into the workflow accordion, appends the step's block to
the report, adds the step's quoted code to the code chain used to
assemble the reproducible script, and records any packages the step
needs. When `ec_subs` is supplied, it also registers an
expansion-context substitution so the downloadable script can swap in
alternate code (for example, fully-qualified calls).

Most data-producing steps should instead use
[`add_shinypal_data_step()`](http://williamgearty.com/shinypal/reference/add_shinypal_data_step.md),
which wraps this together with the data storage, code output, and
selector boilerplate. Call `add_shinypal_step()` directly for steps that
don't fit that pattern.

## Usage

``` r
add_shinypal_step(
  ind,
  fun_workflow,
  fun_report,
  code_chain_list,
  libs = character(0),
  ec_subs = NULL
)
```

## Arguments

- ind:

  The index of the step.

- fun_workflow:

  A function that generates the UI elements for the workflow.

- fun_report:

  A function that generates the UI elements for the report.

- code_chain_list:

  A list of quoted code bits that will be added to code_chain().

- libs:

  A character vector of R packages required for this step.

- ec_subs:

  An optional list of length 2, where the first element is a
  metaReactive object and the second element is a callback function.
  This is used to substitute expansion contexts in the code chain.

## Value

Called for its side effects and returns `NULL` invisibly. The step is
registered with shinypal's reactive state (its panel is inserted into
the workflow accordion, its block appended to the report, its quoted
code added to the code chain, and its packages recorded), and a
remove-button observer is installed for it.

## See also

Other workflow steps:
[`add_shinypal_data_step()`](http://williamgearty.com/shinypal/reference/add_shinypal_data_step.md),
[`add_shinypal_plot_step()`](http://williamgearty.com/shinypal/reference/add_shinypal_plot_step.md),
[`next_step_index()`](http://williamgearty.com/shinypal/reference/next_step_index.md)

## Examples

``` r
if (FALSE) { # \dontrun{
# inside a module's server.R, which shinypal_setup() sources with local = TRUE
ind <- next_step_index()
add_shinypal_step(
  ind,
  fun_workflow = function(ind) accordion_panel_remove_button(ind, "Head"),
  fun_report = function(ind) verbatimTextOutput_copy(ind),
  code_chain_list = list(quote(head(mtcars))),
  libs = "utils"
)
} # }
```
