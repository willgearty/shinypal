# Validate and store a custom name for a step's dataset

Installs an observer on the step's `varname_<ind>` text input. A valid,
unique R variable name is recorded in shinypal's `var_names` registry,
becoming the dataset's label in later selectors and its symbol in the
generated script. Pair with
[`varname_input()`](http://williamgearty.com/shinypal/reference/varname_input.md).

## Usage

``` r
var_name_observe(ind)
```

## Arguments

- ind:

  The index of the step.

## Value

Called for its side effects; invisibly returns the observer.

## See also

[`varname_input()`](http://williamgearty.com/shinypal/reference/varname_input.md),
the input this observer watches.

Other step observers:
[`clip_observe()`](http://williamgearty.com/shinypal/reference/clip_observe.md),
[`column_select_observe()`](http://williamgearty.com/shinypal/reference/column_select_observe.md),
[`df_modal_observe()`](http://williamgearty.com/shinypal/reference/df_modal_observe.md),
[`df_select_observe()`](http://williamgearty.com/shinypal/reference/df_select_observe.md),
[`file_observe()`](http://williamgearty.com/shinypal/reference/file_observe.md)

## Examples

``` r
if (FALSE) { # \dontrun{
var_name_observe(ind)
} # }
```
