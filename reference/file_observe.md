# Observe a file input to be included in the download bundle

Installs an observer that records the file uploaded through `inputId` so
it is bundled into the downloadable report archive, keyed by the
upload's original file name.

## Usage

``` r
file_observe(inputId)
```

## Arguments

- inputId:

  The id of the
  [`shiny::fileInput()`](https://rdrr.io/pkg/shiny/man/fileInput.html)
  object whose uploaded file should be included in the downloadable
  bundle.

## Value

Called for its side effects; invisibly returns the observer.

## See also

Other step observers:
[`clip_observe()`](http://williamgearty.com/shinypal/reference/clip_observe.md),
[`column_select_observe()`](http://williamgearty.com/shinypal/reference/column_select_observe.md),
[`df_modal_observe()`](http://williamgearty.com/shinypal/reference/df_modal_observe.md),
[`df_select_observe()`](http://williamgearty.com/shinypal/reference/df_select_observe.md),
[`var_name_observe()`](http://williamgearty.com/shinypal/reference/var_name_observe.md)

## Examples

``` r
if (FALSE) { # \dontrun{
file_observe("user_file")
} # }
```
