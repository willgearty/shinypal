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

## Examples

``` r
df_modal_button(1)
#> <button id="df_modal_1" type="button" class="btn btn-default action-button"><span class="action-label">View data</span></button>
```
