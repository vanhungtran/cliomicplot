# Grammar Layer: Inter-Domain Coupling Links

Renders the curved connecting links between peripheral environmental
indicators and central network module perimeters, weighted by
correlation sign and statistical significance.

## Usage

``` r
geom_env_links(
  data = NULL,
  link_width = NULL,
  link_width_range = c(0.4, 1.4),
  link_alpha = 0.7,
  link_colors = NULL,
  module_colors = NULL
)
```

## Arguments

- data:

  Optional `cli_env_network_data` object.

- link_width:

  Optional constant line width. Overrides significance weighting.

- link_width_range:

  Line width range for significance-weighted links (default
  `c(0.4, 1.4)`).

- link_alpha:

  Transparency of links (default 0.70).

- link_colors:

  Named vector of colors for link directions.

- module_colors:

  Named vector of colors for network modules.

## Value

A list of `ggplot2` layers.
