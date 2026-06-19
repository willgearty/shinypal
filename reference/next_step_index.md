# Get the next workflow step index

Returns a unique, monotonically increasing index for a new workflow
step. Each call increments a server-side counter. The counter is
initialized by
[`shinypal_setup()`](http://williamgearty.com/shinypal/reference/shinypal_setup.md)
and is deliberately never reset while the session is running.

## Usage

``` r
next_step_index()
```

## Value

A single positive integer to use as the new step's index.

## Examples

``` r
if (FALSE) { # \dontrun{
ind <- next_step_index()
} # }
```
