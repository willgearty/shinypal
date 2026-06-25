# Get an intermediate data object

Retrieves a stored intermediate data reactive by name (the counterpart
to
[`set_int_data()`](http://williamgearty.com/shinypal/reference/set_int_data.md)).
Call the returned reactive to obtain the data.

## Usage

``` r
get_int_data(name)
```

## Arguments

- name:

  The name of the data object to retrieve.

## Value

The stored reactive; call it (e.g. `get_int_data(name)()`) to get the
data. Propagates a `req()` failure if `name` is not registered.

## See also

Other intermediate data:
[`get_int_dfs()`](http://williamgearty.com/shinypal/reference/get_int_dfs.md),
[`set_int_data()`](http://williamgearty.com/shinypal/reference/set_int_data.md)

## Examples

``` r
if (FALSE) { # \dontrun{
df <- get_int_data(input[[paste0("dataset_", ind)]])()
} # }
```
