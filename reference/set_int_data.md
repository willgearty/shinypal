# Set an intermediate data object

Stores a reactive data object in shinypal's intermediate-data registry
under `name`, making it available to later steps and to the assembled
script. Usually called for you by
[`add_shinypal_data_step()`](http://williamgearty.com/shinypal/reference/add_shinypal_data_step.md).

## Usage

``` r
set_int_data(obj, name)
```

## Arguments

- obj:

  A reactive data object to store.

- name:

  A name to store data object as.

## Value

Called for its side effects; invisibly returns `NULL`.

## See also

Other intermediate data:
[`get_int_data()`](http://williamgearty.com/shinypal/reference/get_int_data.md),
[`get_int_dfs()`](http://williamgearty.com/shinypal/reference/get_int_dfs.md)

## Examples

``` r
if (FALSE) { # \dontrun{
set_int_data(occs, paste0("occs_", ind))
} # }
```
