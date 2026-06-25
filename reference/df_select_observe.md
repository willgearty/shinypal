# Keep a `selectInput` of intermediate data.frames up-to-date

Installs an observer that keeps the step's dataset dropdown
(`dataset_<ind>`) populated with the available upstream datasets,
preserving the current selection where possible and otherwise defaulting
to the most recent one.

## Usage

``` r
df_select_observe(ind)
```

## Arguments

- ind:

  The index of the step.

## Value

Called for its side effects; invisibly returns the observer.

## See also

[`select_dataset_input()`](http://williamgearty.com/shinypal/reference/select_dataset_input.md),
the dropdown this observer updates.

Other step observers:
[`clip_observe()`](http://williamgearty.com/shinypal/reference/clip_observe.md),
[`column_select_observe()`](http://williamgearty.com/shinypal/reference/column_select_observe.md),
[`df_modal_observe()`](http://williamgearty.com/shinypal/reference/df_modal_observe.md),
[`file_observe()`](http://williamgearty.com/shinypal/reference/file_observe.md),
[`var_name_observe()`](http://williamgearty.com/shinypal/reference/var_name_observe.md)

## Examples

``` r
if (FALSE) { # \dontrun{
df_select_observe(ind)
} # }
```
