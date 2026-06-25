# Text element with a button to copy the code to the clipboard

Render a reactive output variable as text within an application page.
Uses
[`shiny::verbatimTextOutput()`](https://rdrr.io/pkg/shiny/man/textOutput.html)
which is usually paired with
[`shiny::renderPrint()`](https://rdrr.io/pkg/shiny/man/renderPrint.html)
and provides fixed-width text in a `<pre>`. Make sure to set up a
corresponding observer using
[`clip_observe()`](http://williamgearty.com/shinypal/reference/clip_observe.md).

## Usage

``` r
verbatimTextOutput_copy(ind)
```

## Arguments

- ind:

  The index of the step.

## Value

A
[`htmltools::div()`](https://rstudio.github.io/htmltools/reference/builder.html)
wrapping a
[`shiny::verbatimTextOutput()`](https://rdrr.io/pkg/shiny/man/textOutput.html)
(id `code_<ind>`) and a copy
[`shiny::actionButton()`](https://rdrr.io/pkg/shiny/man/actionButton.html)
(id `copy_<ind>`).

## See also

[`clip_observe()`](http://williamgearty.com/shinypal/reference/clip_observe.md),
which copies the displayed code to the clipboard.

Other step UI:
[`accordion_panel_remove_button()`](http://williamgearty.com/shinypal/reference/accordion_panel_remove_button.md),
[`df_modal_button()`](http://williamgearty.com/shinypal/reference/df_modal_button.md),
[`select_column_input()`](http://williamgearty.com/shinypal/reference/select_column_input.md),
[`select_dataset_input()`](http://williamgearty.com/shinypal/reference/select_dataset_input.md),
[`varname_input()`](http://williamgearty.com/shinypal/reference/varname_input.md)

## Examples

``` r
verbatimTextOutput_copy(1)
#> <div class="code_wrapper">
#>   <pre class="shiny-text-output noplaceholder" id="code_1"></pre>
#>   <button id="copy_1" type="button" class="btn btn-default action-button"><span class="action-label"><i class="far fa-copy" role="presentation" aria-label="copy icon"></i></span></button>
#> </div>
```
