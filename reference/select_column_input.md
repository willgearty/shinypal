# Select input to choose a column from a specified dataset

This should be paired with
[`select_dataset_input()`](http://williamgearty.com/shinypal/reference/select_dataset_input.md).

## Usage

``` r
select_column_input(ind, label = "Choose a column:", default = NULL, ...)
```

## Arguments

- ind:

  The index of the step.

- label:

  The label for the select input.

- default:

  The default value for the select input.

- ...:

  Additional arguments passed to
  [`shiny::varSelectInput()`](https://rdrr.io/pkg/shiny/man/varSelectInput.html).

## Value

A
[`shiny::varSelectInput()`](https://rdrr.io/pkg/shiny/man/varSelectInput.html)
tag with id `column_<ind>`.

## See also

[`column_select_observe()`](http://williamgearty.com/shinypal/reference/column_select_observe.md),
which keeps this selector in sync.

Other step UI:
[`accordion_panel_remove_button()`](http://williamgearty.com/shinypal/reference/accordion_panel_remove_button.md),
[`df_modal_button()`](http://williamgearty.com/shinypal/reference/df_modal_button.md),
[`select_dataset_input()`](http://williamgearty.com/shinypal/reference/select_dataset_input.md),
[`varname_input()`](http://williamgearty.com/shinypal/reference/varname_input.md),
[`verbatimTextOutput_copy()`](http://williamgearty.com/shinypal/reference/verbatimTextOutput_copy.md)

## Examples

``` r
if (FALSE) { # \dontrun{
select_column_input(ind)
} # }
```
