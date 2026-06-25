# Select input to choose a shinypal intermediate dataset

A
[`shiny::selectInput()`](https://rdrr.io/pkg/shiny/man/selectInput.html)
listing the intermediate datasets available to step `ind` (those
produced by earlier steps), defaulting to the most recent. Keep its
choices current with
[`df_select_observe()`](http://williamgearty.com/shinypal/reference/df_select_observe.md).

## Usage

``` r
select_dataset_input(ind, label = "Choose a dataset:")
```

## Arguments

- ind:

  The index of the step.

- label:

  The label for the select input.

## Value

A
[`shiny::selectInput()`](https://rdrr.io/pkg/shiny/man/selectInput.html)
tag with id `dataset_<ind>`.

## See also

[`df_select_observe()`](http://williamgearty.com/shinypal/reference/df_select_observe.md),
which keeps this dropdown populated.

Other step UI:
[`accordion_panel_remove_button()`](http://williamgearty.com/shinypal/reference/accordion_panel_remove_button.md),
[`df_modal_button()`](http://williamgearty.com/shinypal/reference/df_modal_button.md),
[`select_column_input()`](http://williamgearty.com/shinypal/reference/select_column_input.md),
[`varname_input()`](http://williamgearty.com/shinypal/reference/varname_input.md),
[`verbatimTextOutput_copy()`](http://williamgearty.com/shinypal/reference/verbatimTextOutput_copy.md)

## Examples

``` r
if (FALSE) { # \dontrun{
select_dataset_input(ind)
} # }
```
