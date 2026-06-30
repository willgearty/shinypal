# Accordion panel that includes a remove button

A
[`bslib::accordion_panel()`](https://rstudio.github.io/bslib/reference/accordion.html)
pre-wired with a "Remove this step" button and the `data-rank-id`
attribute shinypal's sortable workflow needs. Use it as the panel
returned by a step's `fun_workflow`.

## Usage

``` r
accordion_panel_remove_button(ind, ...)
```

## Arguments

- ind:

  The index of the step.

- ...:

  Additional arguments passed to
  [`bslib::accordion_panel()`](https://rstudio.github.io/bslib/reference/accordion.html).

## Value

A
[`bslib::accordion_panel()`](https://rstudio.github.io/bslib/reference/accordion.html)
tag with `value = "step_<ind>"` and a matching `data-rank-id` attribute.

## See also

Other step UI:
[`df_modal_button()`](http://williamgearty.com/shinypal/reference/df_modal_button.md),
[`select_column_input()`](http://williamgearty.com/shinypal/reference/select_column_input.md),
[`select_dataset_input()`](http://williamgearty.com/shinypal/reference/select_dataset_input.md),
[`varname_input()`](http://williamgearty.com/shinypal/reference/varname_input.md),
[`verbatimTextOutput_copy()`](http://williamgearty.com/shinypal/reference/verbatimTextOutput_copy.md)

## Examples

``` r
accordion_panel_remove_button(1, "My step")
#> <div class="accordion-item" data-value="step_1" data-rank-id="step_1">
#>   <div class="accordion-header">
#>     <button class="accordion-button collapsed" type="button" data-bs-toggle="collapse" data-bs-target="#bslib-accordion-panel-1450" aria-expanded="false" aria-controls="bslib-accordion-panel-1450">
#>       <div class="accordion-icon"></div>
#>       <div class="accordion-title">My step</div>
#>     </button>
#>   </div>
#>   <div id="bslib-accordion-panel-1450" class="accordion-collapse collapse">
#>     <div class="accordion-body">
#>       <button id="remove_step_1" type="button" class="btn btn-default action-button"><span class="action-label">Remove this step</span></button>
#>     </div>
#>   </div>
#> </div>
```
