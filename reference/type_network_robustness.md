# Network Robustness Degradation Curves Plot Type

Generates publication-ready network stability and robustness curves
comparing the degradation trajectory of different biological networks or
cohorts.

## Usage

``` r
type_network_robustness(
  show_se = TRUE,
  line_width = 1,
  point_size = 1.8,
  colors = c("#E41A1C", "#377EB8", "#4DAF4A", "#984EA3", "#FF7F00"),
  xlab = "Proportion of Nodes Removed",
  ylab = "Natural Connectivity"
)
```

## Arguments

- show_se:

  Logical. If TRUE, renders standard error ribbons for random attack
  simulations (default TRUE).

- line_width:

  Line width for degradation curves (default 1.0).

- point_size:

  Point size for sampled removal fractions (default 1.8).

- colors:

  Vector of colors for condition groups. Default uses publication-ready
  palette.

- xlab, ylab:

  Axis labels.

## Value

A `cliplot_type` object for use with
[`cliplot`](https://vanhungtran.github.io/cliomicplot/reference/cliplot.md).
