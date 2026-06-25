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

## See also

[`select_column_input()`](http://williamgearty.com/shinypal/reference/select_column_input.md),
the selector this observer updates.

Other step observers:
[`clip_observe()`](http://williamgearty.com/shinypal/reference/clip_observe.md),
[`df_modal_observe()`](http://williamgearty.com/shinypal/reference/df_modal_observe.md),
[`df_select_observe()`](http://williamgearty.com/shinypal/reference/df_select_observe.md),
[`file_observe()`](http://williamgearty.com/shinypal/reference/file_observe.md),
[`var_name_observe()`](http://williamgearty.com/shinypal/reference/var_name_observe.md)

## Examples

``` r
if (FALSE) { # \dontrun{
column_select_observe(ind, paste0("column_", ind))
} # }
```
