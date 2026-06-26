# Show a modal with a reactable data.frame

Installs an observer that opens a modal showing a
[`DT::datatable()`](https://rdrr.io/pkg/DT/man/datatable.html) of the
named intermediate dataset when the step's "view data" button is
clicked. Pair with
[`df_modal_button()`](http://williamgearty.com/shinypal/reference/df_modal_button.md).

## Usage

``` r
df_modal_observe(ind, df_name)
```

## Arguments

- ind:

  The index of the step.

- df_name:

  The name of the data.frame to be displayed.

## Value

Called for its side effects; invisibly returns the observer.

## See also

[`df_modal_button()`](http://williamgearty.com/shinypal/reference/df_modal_button.md),
the button that opens this modal.

Other step observers:
[`clip_observe()`](http://williamgearty.com/shinypal/reference/clip_observe.md),
[`column_select_observe()`](http://williamgearty.com/shinypal/reference/column_select_observe.md),
[`df_select_observe()`](http://williamgearty.com/shinypal/reference/df_select_observe.md),
[`file_observe()`](http://williamgearty.com/shinypal/reference/file_observe.md),
[`var_name_observe()`](http://williamgearty.com/shinypal/reference/var_name_observe.md)

## Examples

``` r
if (FALSE) { # \dontrun{
df_modal_observe(ind, step_varname(ind))
} # }
```
