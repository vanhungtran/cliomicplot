# Parallel Coordinates Plot Type

Creates parallel coordinate plots for multivariate data exploration.
Each variable is shown as a vertical axis, and each observation is a
line connecting its values across all axes. Colored by a grouping
variable.

## Usage

``` r
type_parallel(alpha = 0.5, linewidth = 0.4, box_alpha = 0.3)
```

## Arguments

- alpha:

  Line transparency (default 0.5)

- linewidth:

  Line width (default 0.4)

- box_alpha:

  Box plot alpha on axes (default 0.3)

## Value

A cliplot_type object
