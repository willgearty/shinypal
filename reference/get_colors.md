# Generate step colors from a step index

Maps a step index to a reproducible accent color (with a readable
foreground color) drawn from the khroma "smooth rainbow" palette, so
each workflow step gets a distinct, stable color for its accordion
header and report border.

## Usage

``` r
get_colors(ind)
```

## Arguments

- ind:

  The index of the step.

## Value

A list with two elements: `color`, the foreground color (`"black"` or
`"white"`, chosen for contrast against the background), and
`background`, the accent color as a hex string.

## See also

Other utilities:
[`is_shinylive()`](http://williamgearty.com/shinypal/reference/is_shinylive.md)

## Examples

``` r
get_colors(1)
#> $color
#> [1] "black"
#> 
#> $background
#> [1] "#519AB7"
#> 
get_colors(2)$background
#> [1] "#95BC60"
```
