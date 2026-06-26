# Build a data step's internal id and generated variable name

Returns the name shinypal uses both as a data step's stored-dataset id
and as the variable it is assigned in the generated script (set `prefix`
in
[`shinypal_setup()`](http://williamgearty.com/shinypal/reference/shinypal_setup.md)).
A module should pass this as the `varname` of its
[`shinymeta::metaReactive2()`](https://rstudio.github.io/shinymeta/reference/metaReactive.html)
so the emitted variable name matches the id shinypal stores and renames.

## Usage

``` r
step_varname(ind)
```

## Arguments

- ind:

  The index of the step.

## Value

A length-1 character string, the prefix followed by `ind` (e.g.,
`"data_1"`).

## See also

Other workflow steps:
[`add_shinypal_data_step()`](http://williamgearty.com/shinypal/reference/add_shinypal_data_step.md),
[`add_shinypal_plot_step()`](http://williamgearty.com/shinypal/reference/add_shinypal_plot_step.md),
[`add_shinypal_step()`](http://williamgearty.com/shinypal/reference/add_shinypal_step.md),
[`next_step_index()`](http://williamgearty.com/shinypal/reference/next_step_index.md)

## Examples

``` r
if (FALSE) { # \dontrun{
# inside a module's add-step observer, after shinypal_setup()
step_varname(ind)
} # }
```
