# Reference Lines Type

Adds horizontal, vertical, or diagonal reference lines to a plot.

## Usage

``` r
type_abline(
  intercept = 0,
  slope = 0,
  linewidth = 0.5,
  linetype = "dashed",
  color = "grey50"
)
```

## Arguments

- intercept:

  Intercept for abline (default 0). For hline: y value(s). For vline: x
  value(s).

- slope:

  Slope for diagonal lines (only for abline)

- linewidth:

  Line width

- linetype:

  Line type ("solid", "dashed", "dotted", etc.)

- color:

  Line color

## Value

A cliplot_type object
