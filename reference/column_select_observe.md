# Keep a `varSelectInput` of data.frame column names up-to-date

Installs an observer that keeps a
[`shiny::varSelectInput()`](https://rdrr.io/pkg/shiny/man/varSelectInput.html)
of column names in sync with the dataset currently chosen in the step's
`dataset_<ind>` dropdown. Pair with
[`select_dataset_input()`](http://williamgearty.com/shinypal/reference/select_dataset_input.md)
and
[`select_column_input()`](http://williamgearty.com/shinypal/reference/select_column_input.md).

## Usage

``` r
column_select_observe(ind, inputId)
```

## Arguments

- ind:

  The index of the step.

- inputId:

  The id of the
  [`shiny::varSelectInput()`](https://rdrr.io/pkg/shiny/man/varSelectInput.html)
  object.

## Value

Called for its side effects; invisibly returns the observer.

## Examples

``` r
if (FALSE) { # \dontrun{
column_select_observe(ind, paste0("column_", ind))
} # }
```
