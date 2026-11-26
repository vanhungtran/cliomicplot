# Grammatical Data Preparation for Central Network Coupled with Environmental Heatmaps

Generates the complete spatial layout, node coordinates, intra-module
edge segments, convex hulls, peripheral triangular heatmap coordinates,
and curved inter-domain link trajectories. The returned
`cli_env_network_data` S3 object serves as the data foundation for the
modular grammar layers
([`geom_env_boundary`](https://vanhungtran.github.io/cliomicplot/reference/geom_env_boundary.md),
[`geom_env_central_network`](https://vanhungtran.github.io/cliomicplot/reference/geom_env_central_network.md),
[`geom_env_corner_heatmaps`](https://vanhungtran.github.io/cliomicplot/reference/geom_env_corner_heatmaps.md),
and
[`geom_env_links`](https://vanhungtran.github.io/cliomicplot/reference/geom_env_links.md)).

## Usage

``` r
cli_env_network_data(
  comm_data = NULL,
  env_data = NULL,
  nodes = NULL,
  edges = NULL,
  env_cors = NULL,
  links = NULL,
  modules = NULL,
  module_labels = NULL,
  module_colors = NULL,
  corner_palettes = NULL,
  link_colors = c(Positive = "#E64B35", Negative = "#4DBBD5"),
  label_type = c("auto", "stars", "coef", "coef_stars", "none"),
  show_asterisks = TRUE,
  show_values = FALSE,
  value_digits = 2,
  value_size = NULL,
  compact = TRUE,
  show_module_labels = TRUE,
  module_label_size = 2.4,
  module_label_color = "grey15",
  module_label_fill = "white",
  module_label_bg = TRUE,
  module_label_alpha = 1,
  module_label_pos = c("radial", "centroid"),
  show_module_legend = TRUE,
  legend_position = "top",
  show_domain_labels = TRUE,
  env_label_size = 1.95,
  env_label_color = "grey20",
  env_label_face = "plain",
  link_width_range = c(0.4, 1.4),
  link_width = NULL,
  link_alpha = 0.7,
  show_link_values = FALSE,
  link_value_size = 2,
  link_value_digits = 2,
  circle_radius = 2.8,
  circle_width = 0.4,
  circle_color = "grey65",
  circle_linetype = "dashed",
  outer_ring_size = 0.6,
  outer_ring_color = "grey60",
  outer_ring_alpha = 0.6,
  intra_edge_width = 0.18,
  intra_edge_color = "grey82",
  intra_edge_alpha = 0.35,
  diag_point_size = 2,
  diag_point_fill = "#F781BF",
  diag_point_color = "#C51B7D",
  node_size_range = c(1.1, 3.8),
  hull_alpha = 0.18,
  curvature = 0.14,
  preset = "default",
  scifig = FALSE
)
```

## Arguments

- comm_data:

  Optional numeric matrix or data frame of community abundance data
  (samples in rows, features in columns). If supplied and `links` is
  NULL, empirical linkages are automatically calculated using
  [`cli_calc_env_links`](https://vanhungtran.github.io/cliomicplot/reference/cli_calc_env_links.md).

- env_data:

  Numeric matrix, data frame, or named list of 1 to 4 correlation
  matrices or indicator variables for corner triangular heatmaps. Alias
  for `env_cors`.

- nodes:

  Optional data frame of central network nodes.

- edges:

  Optional data frame of central network edges.

- env_cors:

  Alias for `env_data`.

- links:

  Optional data frame of pre-computed environmental-to-module linkages.

- modules:

  Optional named vector assigning features to module names.

- module_labels:

  Optional character vector or named vector of descriptive module
  labels.

- module_colors:

  Optional named vector of colors for modules.

- corner_palettes:

  Optional list of 1 to 4 color gradient palettes for the corner
  heatmaps.

- link_colors:

  Named vector of colors for link directions (default
  `c("Positive" = "#E64B35", "Negative" = "#4DBBD5")`).

- label_type:

  Format for heatmap tile values: `"auto"`, `"stars"`, `"coef"`,
  `"coef_stars"`, or `"none"`.

- show_asterisks:

  Logical. If TRUE, display significance asterisks. Default TRUE.

- show_values:

  Logical. If TRUE, display numeric correlation coefficients. Default
  FALSE.

- value_digits:

  Integer. Number of decimal places for correlation digits (default 2).

- value_size:

  Numeric. Font size for tile correlation text (default NULL for
  auto-scaling).

- compact:

  Logical. If TRUE (default), omits leading zero for compact tile text.

- show_module_labels:

  Logical. If TRUE, displays module label badges (default TRUE).

- module_label_size:

  Font size for module label text (default 2.4).

- module_label_color:

  Text color for module labels (default `"grey15"`).

- module_label_fill:

  Background fill color for module labels (default `"white"`).

- module_label_bg:

  Logical. If TRUE, renders bounding badge boxes behind labels (default
  TRUE).

- module_label_alpha:

  Transparency for module label background (default 1.0).

- module_label_pos:

  Placement strategy for module labels: `"radial"` (default) or
  `"centroid"`.

- show_module_legend:

  Logical. If TRUE, shows module color legend (default TRUE).

- legend_position:

  Position of plot legend: `"top"` (default), `"bottom"`, `"right"`, or
  `"left"`.

- show_domain_labels:

  Logical. If TRUE, displays corner domain headers (default TRUE).

- env_label_size:

  Font size for environmental variable labels (default 1.95).

- env_label_color:

  Color for environmental variable labels (default `"grey20"`).

- env_label_face:

  Font face for environmental variable labels (default `"plain"`).

- link_width_range:

  Line width range for significance-weighted links (default
  `c(0.4, 1.4)`).

- link_width:

  Optional constant line width overriding significance weighting.

- link_alpha:

  Transparency for curved inter-domain links (default 0.70).

- show_link_values:

  Logical. If TRUE, shows correlation coefficients on links (default
  FALSE).

- link_value_size:

  Font size for link correlation digits (default 2.0).

- link_value_digits:

  Number of decimal places for link correlation digits (default 2).

- circle_radius:

  Radius of central boundary circle (default 2.80).

- circle_width:

  Line width of boundary circle (default 0.4).

- circle_color:

  Color of boundary circle (default `"grey65"`).

- circle_linetype:

  Linetype of boundary circle (default `"dashed"`).

- outer_ring_size:

  Size of dots along outer boundary ring (default 0.6).

- outer_ring_color:

  Color of dots along outer boundary ring (default `"grey60"`).

- outer_ring_alpha:

  Transparency of dots along outer boundary ring (default 0.6).

- intra_edge_width:

  Line width of intra-module edges (default 0.18).

- intra_edge_color:

  Color of intra-module edges (default `"grey82"`).

- intra_edge_alpha:

  Transparency of intra-module edges (default 0.35).

- diag_point_size:

  Size of diagonal indicator connector points (default 2.0).

- diag_point_fill:

  Fill color of diagonal indicator points (default `"#F781BF"`).

- diag_point_color:

  Border color of diagonal indicator points (default `"#C51B7D"`).

- node_size_range:

  Numeric vector of length 2 for node sizes (default `c(1.1, 3.8)`).

- hull_alpha:

  Shading transparency for module cluster hulls (default 0.18).

- curvature:

  Curvature of inter-domain links (default 0.14).

- preset:

  Publisher style preset: `"default"`, `"nature"`, `"clinical"`,
  `"seasonal"`, `"cell"`, `"dark"`, `"lancet"`, `"minimal"`,
  `"multi_tissue"`, `"circadian"`, or `"bipolar"`.

- scifig:

  Logical. If TRUE, enforces AI4SciFig publication gates (default
  FALSE).

## Value

An S3 object of class `c("cli_env_network_data", "list")` containing
layout tables.
