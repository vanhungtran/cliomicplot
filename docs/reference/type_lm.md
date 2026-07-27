# Linear Model Fit Type

Adds a linear regression line with optional confidence band. Useful for
scatter plots with trend lines.

## Usage

``` r
type_lm(se = TRUE, level = 0.95, linewidth = 1, alpha = 0.15, formula = y ~ x)
```

## Arguments

- se:

  Show confidence band (default TRUE)

- level:

  Confidence level for the band (default 0.95)

- linewidth:

  Line width (default 1)

- alpha:

  Confidence band transparency (default 0.15)

- formula:

  Model formula override (default y ~ x)

## Value

A cliplot_type object

## Examples

``` r
if (FALSE) { # \dontrun{
cliplot(mpg ~ wt, data = mtcars, type = "lm")
cliplot(mpg ~ wt, data = mtcars, type = type_lm(level = 0.9))
} # }
```
