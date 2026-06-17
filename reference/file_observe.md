# Observe a file input to be included in the download bundle

Installs an observer that records the file uploaded through `inputId` so
it is bundled into the downloadable report archive, keyed by the
upload's original file name.

## Usage

``` r
file_observe(input, inputId)
```

## Arguments

- input:

  The shiny input object.

- inputId:

  The id of the
  [`shiny::fileInput()`](https://rdrr.io/pkg/shiny/man/fileInput.html)
  object whose uploaded file should be included in the downloadable
  bundle.

## Value

Called for its side effects; invisibly returns the observer.

## Examples

``` r
if (FALSE) { # \dontrun{
file_observe(input, "user_file")
} # }
```
