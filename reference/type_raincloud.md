# Raincloud Plot Type

Creates elegant raincloud plots combining a split-half violin, boxplot,
and jittered raw data points. Popular in neuroscience and psychology for
transparent data visualization.

## Usage

``` r
type_raincloud(
  alpha = 0.7,
  point_size = 1.5,
  point_alpha = 0.4,
  box_width = 0.2,
  adjust = 1,
  side = "right"
)
```

## Arguments

- alpha:

  Raincloud transparency (default 0.7)

- point_size:

  Size of jittered points (default 1.5)

- point_alpha:

  Transparency of points (default 0.4)

- box_width:

  Width of boxplot relative to violin (default 0.2)

- adjust:

  Bandwidth adjustment for density (default 1)

- side:

  Which side to draw the raincloud: "right", "left", or "both"

## Value

A cliplot_type object
