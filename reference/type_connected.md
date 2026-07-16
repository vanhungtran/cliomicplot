# Connected Scatterplot Type

Creates connected scatterplots showing the path of paired observations
over time or sequence. Each point is connected to the next in order,
revealing trajectories and phase changes.

## Usage

``` r
type_connected(
  point_size = 3,
  linewidth = 0.6,
  point_alpha = 0.9,
  line_alpha = 0.6,
  show_arrows = FALSE
)
```

## Arguments

- point_size:

  Point size (default 3)

- linewidth:

  Connecting line width (default 0.6)

- point_alpha:

  Point transparency

- line_alpha:

  Line transparency

- show_arrows:

  Show direction arrows (default FALSE)

## Value

A cliplot_type object
