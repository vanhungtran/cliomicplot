# LOESS Smoothing Type

Adds a LOESS (locally estimated scatterplot smoothing) curve with
optional confidence band.

## Usage

``` r
type_loess(se = TRUE, level = 0.95, span = 0.75, linewidth = 1, alpha = 0.15)
```

## Arguments

- se:

  Show confidence band (default TRUE)

- level:

  Confidence level (default 0.95)

- span:

  Smoothing span; smaller = more wiggly (default 0.75)

- linewidth:

  Line width (default 1)

- alpha:

  Confidence band transparency (default 0.15)

## Value

A cliplot_type object

## Examples

``` r
if (FALSE) { # \dontrun{
cliplot(mpg ~ wt, data = mtcars, type = "loess")
} # }
```
