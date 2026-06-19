# Detect a shinylive (webR) session

Returns `TRUE` when the app is running in a shinylive/webR session (i.e.
compiled to WebAssembly via Emscripten) and `FALSE` in a normal R
session. Useful for gating behavior that can't run in the browser, such
as loading packages with no WebAssembly build or bundling a downloadable
zip.

## Usage

``` r
is_shinylive()
```

## Value

A length-1 logical: `TRUE` under shinylive/webR, otherwise `FALSE`.

## Examples

``` r
is_shinylive()
#> [1] FALSE
```
