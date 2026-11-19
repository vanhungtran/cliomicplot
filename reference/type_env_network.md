# Circular Modular Network Coupled with Environmental Correlation Heatmaps

Creates publication-ready multi-domain integration networks coupling a
central modular microbial/omics co-occurrence network with peripheral
triangular correlation heatmaps of environmental, clinical, or
host-associated indicators (e.g. soil chemistry across seasons, clinical
biomarkers across cohorts, or multi-tissue traits). Curved inter-domain
links connect environmental indicators from the triangular diagonal to
central network modules, weighted by statistical significance and
colored by correlation direction.

## Usage

``` r
type_env_network(
  env_cors = NULL,
  links = NULL,
  module_colors = NULL,
  corner_palettes = NULL,
  link_colors = c(Positive = "#E64B35", Negative = "#4DBBD5"),
  show_asterisks = TRUE,
  curvature = 0.14,
  circle_radius = 2.45,
  title = NULL
)
```

## Arguments

- env_cors:

  Named list of 1 to 4 correlation matrices for corner triangular
  heatmaps. Names define corner labels (e.g.
  `list("Env 1" = cor1, "Env 2" = cor2)`).

- links:

  Optional data frame of links connecting environmental variables to
  network modules. Required columns: `env`, `module`, `r`, and `p` (or
  `log10p`). If NULL, significant links are computed or sampled
  automatically from input data.

- module_colors:

  Optional named vector of colors for network modules.

- corner_palettes:

  Optional list of color vectors for the corner heatmaps.

- link_colors:

  Named vector of colors for link signs (default
  `c("Positive" = "#E64B35", "Negative" = "#4DBBD5")`).

- show_asterisks:

  Logical. If TRUE, overlays significance asterisks (\*, \*\*, \*\*\*)
  in correlation tiles. Default TRUE.

- curvature:

  Numeric curvature of inter-domain connecting lines (default 0.14).

- circle_radius:

  Numeric radius of the central boundary circle (default 2.45).

- title:

  Optional plot title.

## Value

A `cliplot_type` object for use with
[`cliplot`](https://vanhungtran.github.io/cliomicplot/reference/cliplot.md).
