# Ridgeline Plot Type

Creates ridgeline (joy) plots for visualizing distribution changes
across ordered categories. Particularly effective for showing how a
continuous variable varies across groups.

## Usage

``` r
type_ridge(
  scale = 1.5,
  alpha = 0.6,
  bandwidth = NULL,
  gradient = FALSE,
  quantile_lines = FALSE,
  quantiles = c(0.25, 0.5, 0.75),
  show_points = FALSE,
  point_size = 0.5,
  point_alpha = 0.15
)
```

## Arguments

- scale:

  Overall scaling factor for ridge heights (default 1.5)

- alpha:

  Fill transparency for ridges (default 0.6)

- bandwidth:

  Bandwidth for density estimation; NULL = auto

- gradient:

  Add vertical gradient fill within each ridge

- quantile_lines:

  Add quantile lines within ridges

- quantiles:

  Vector of quantiles to draw lines for (default c(0.25, 0.5, 0.75))

- show_points:

  Show underlying data points (default FALSE)

- point_size:

  Size of underlying data points

- point_alpha:

  Transparency of underlying points

## Value

A cliplot_type object

## Examples

``` r
if (FALSE) { # \dontrun{
# Classic ridgeline plot
cliplot(Temp ~ factor(Month), data = airquality, type = "ridge")

# With gradient fill and quantile lines
cliplot(Sepal.Length ~ Species, data = iris,
        type = type_ridge(gradient = TRUE, quantile_lines = TRUE))
} # }
```
