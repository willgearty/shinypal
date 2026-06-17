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

## Examples

``` r
if (FALSE) { # \dontrun{
select_dataset_input(ind)
} # }
```
