# Get the expanded code chunk for a registered step

Returns the code chunk for `ind`, expanded with a shared context across
all currently-registered steps. Use this inside a reactive consumer
(e.g., `renderPrint()`, `observeEvent()`) to display or copy a step's
code. The chunk is rebuilt whenever any dependency changes.

## Usage

``` r
get_chunk(ind)
```

## Arguments

- ind:

  The index of the step.

## Value

A code object suitable for printing or passing to
[`shinymeta::displayCodeModal()`](https://rstudio.github.io/shinymeta/reference/displayCodeModal.html),
or `NULL` if the step is not registered.

## See also

Other generated code:
[`workflow_has_errors()`](http://williamgearty.com/shinypal/reference/workflow_has_errors.md)

## Examples

``` r
if (FALSE) { # \dontrun{
# inside renderPrint() or observeEvent() in a module
get_chunk(ind)
} # }
```
