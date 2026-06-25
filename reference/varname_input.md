# Text input to give a step's dataset a custom name

A [`shiny::textInput()`](https://rdrr.io/pkg/shiny/man/textInput.html)
(id `varname_<ind>`) for naming the dataset a step produces. The name
becomes the dataset's label in later selectors and its variable name in
the generated script. Pair with
[`var_name_observe()`](http://williamgearty.com/shinypal/reference/var_name_observe.md),
which validates the entry;
[`add_shinypal_data_step()`](http://williamgearty.com/shinypal/reference/add_shinypal_data_step.md)
inserts both automatically when `rename = TRUE`.

## Usage

``` r
varname_input(ind, label = "Name this dataset (optional):")
```

## Arguments

- ind:

  The index of the step.

- label:

  The label for the text input.

## Value

A [`shiny::textInput()`](https://rdrr.io/pkg/shiny/man/textInput.html)
tag with id `varname_<ind>`.

## See also

[`var_name_observe()`](http://williamgearty.com/shinypal/reference/var_name_observe.md),
which validates and stores the entry.

Other step UI:
[`accordion_panel_remove_button()`](http://williamgearty.com/shinypal/reference/accordion_panel_remove_button.md),
[`df_modal_button()`](http://williamgearty.com/shinypal/reference/df_modal_button.md),
[`select_column_input()`](http://williamgearty.com/shinypal/reference/select_column_input.md),
[`select_dataset_input()`](http://williamgearty.com/shinypal/reference/select_dataset_input.md),
[`verbatimTextOutput_copy()`](http://williamgearty.com/shinypal/reference/verbatimTextOutput_copy.md)

## Examples

``` r
if (FALSE) { # \dontrun{
varname_input(ind)
} # }
```
