# 2D Density / Contour Plot Type

Creates a 2D density plot showing the joint distribution of two
continuous variables. Includes filled contours and optional scatter
overlay. An elegant alternative to overplotted scatterplots.

## Usage

``` r
type_density2d(
  bins = 10,
  alpha = 0.6,
  show_points = FALSE,
  point_size = 1,
  point_alpha = 0.3
)
```

## Arguments

- bins:

  Number of contour bins (default 10)

- alpha:

  Fill transparency

- show_points:

  Overlay scatter points (default FALSE)

- point_size:

  Point size for overlay

- point_alpha:

  Point transparency for overlay

## Value

A cliplot_type object
