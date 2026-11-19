# Direct Plotting Function for Modular Network with Environmental Factor Heatmaps

Direct Plotting Function for Modular Network with Environmental Factor
Heatmaps

## Usage

``` r
cliplot_env_network(
  nodes = NULL,
  edges = NULL,
  env_cors = NULL,
  links = NULL,
  module_colors = NULL,
  corner_palettes = NULL,
  title = NULL,
  ...
)
```

## Arguments

- nodes:

  Optional data frame of central network nodes (columns: `id`, `module`,
  optional `degree`, `x`, `y`).

- edges:

  Optional data frame of central network edges (columns: `from`, `to`).

- env_cors:

  Named list of 1 to 4 correlation matrices for corner triangular
  heatmaps.

- links:

  Data frame of environmental-to-module links.

- module_colors:

  Named color vector for modules.

- corner_palettes:

  List of color gradients for corner heatmaps.

- title:

  Plot title.

- ...:

  Additional arguments.

## Value

A `ggplot` object.
