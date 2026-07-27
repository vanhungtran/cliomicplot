# Streamgraph / Streamchart Type

Creates beautiful streamgraphs (flowing stacked area charts) for
visualizing changes in composition over time. Uses ggstream for smooth,
organic curves.

## Usage

``` r
type_streamgraph(
  alpha = 0.8,
  bw = 0.75,
  sorting = "mirror",
  interpolate = "basis"
)
```

## Arguments

- alpha:

  Fill transparency (default 0.8)

- bw:

  Bandwidth for smoothing (default 0.75)

- sorting:

  How to sort layers: "none", "onset", "insideout", "mirror"

- interpolate:

  Interpolation type: "linear", "basis", "cardinal"

## Value

A cliplot_type object
