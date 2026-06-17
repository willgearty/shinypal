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

## Examples

``` r
if (FALSE) { # \dontrun{
select_column_input(ind)
} # }
```
