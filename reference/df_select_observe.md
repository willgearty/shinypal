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

## Examples

``` r
if (FALSE) { # \dontrun{
df_select_observe(ind)
} # }
```
