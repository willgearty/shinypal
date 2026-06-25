# Button to show dataframe modal

Generates a
[`shiny::actionButton()`](https://rdrr.io/pkg/shiny/man/actionButton.html)
that, when clicked, generates a modal for the resulting dataset for the
specified step. Make sure to set up a corresponding observer using
[`df_modal_observe()`](http://williamgearty.com/shinypal/reference/df_modal_observe.md).

## Usage

``` r
df_modal_button(ind, text = "View data")
```

## Arguments

- ind:

  The index of the step.

- text:

  The text to display on the button.

## Value

A
[`shiny::actionButton()`](https://rdrr.io/pkg/shiny/man/actionButton.html)
with id `df_modal_<ind>`.

## See also

[`df_modal_observe()`](http://williamgearty.com/shinypal/reference/df_modal_observe.md),
which opens the modal this button triggers.

Other step UI:
[`accordion_panel_remove_button()`](http://williamgearty.com/shinypal/reference/accordion_panel_remove_button.md),
[`select_column_input()`](http://williamgearty.com/shinypal/reference/select_column_input.md),
[`select_dataset_input()`](http://williamgearty.com/shinypal/reference/select_dataset_input.md),
[`varname_input()`](http://williamgearty.com/shinypal/reference/varname_input.md),
[`verbatimTextOutput_copy()`](http://williamgearty.com/shinypal/reference/verbatimTextOutput_copy.md)

## Examples

``` r
df_modal_button(1)
#> <button id="df_modal_1" type="button" class="btn btn-default action-button"><span class="action-label">View data</span></button>
```
