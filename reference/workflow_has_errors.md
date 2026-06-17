# Check whether the workflow has incomplete or errored steps

Returns `TRUE` if any step in the current workflow cannot be expanded
into the downloadable script due to failing `req()`/`validate()` checks.

## Usage

``` r
workflow_has_errors()
```

## Value

A length-one logical.

## Examples

``` r
if (FALSE) { # \dontrun{
if (workflow_has_errors()) {
  shiny::showNotification("Some steps are incomplete.")
}
} # }
```
