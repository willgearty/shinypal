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

A character vector of intermediate dataset names in workflow order, or
an empty character vector if none are available upstream.

## Examples

``` r
if (FALSE) { # \dontrun{
# inside a reactive or observer in a module, after shinypal_setup()
get_int_dfs(ind)
} # }
```
