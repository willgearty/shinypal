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

## Examples

``` r
if (FALSE) { # \dontrun{
df_modal_observe(ind, paste0("occs_", ind))
} # }
```
