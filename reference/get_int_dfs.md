# Get the names of all intermediate data.frames for a given step

Returns the names of intermediate datasets produced by steps *earlier*
than `ind` in the current workflow order, so a step can only ever
consume upstream output. Used to populate the dataset selectors.

## Usage

``` r
get_int_dfs(ind)
```

## Arguments

- ind:

  The index of the step.

## Value

A named character vector of intermediate datasets in workflow order:
values are the stable internal ids
([`step_varname()`](http://williamgearty.com/shinypal/reference/step_varname.md),
e.g. `data_1`) and names are the display labels (a custom name if set,
otherwise the id). Empty if none are available upstream.

## See also

Other intermediate data:
[`get_int_data()`](http://williamgearty.com/shinypal/reference/get_int_data.md),
[`set_int_data()`](http://williamgearty.com/shinypal/reference/set_int_data.md)

## Examples

``` r
if (FALSE) { # \dontrun{
# inside a reactive or observer in a module, after shinypal_setup()
get_int_dfs(ind)
} # }
```
