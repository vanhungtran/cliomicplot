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
  link_colors = c(Positive = "#E64B35", Negative = "#4DBBD5"),
  show_asterisks = TRUE,
  show_values = FALSE,
  show_cor = NULL,
  label_type = c("auto", "stars", "coef", "coef_stars", "none"),
  value_digits = 2,
  value_size = NULL,
  compact = TRUE,
  show_module_labels = TRUE,
  module_labels = NULL,
  module_label_size = 2.8,
  module_label_color = "grey15",
  module_label_fill = "white",
  module_label_bg = TRUE,
  module_label_alpha = 1,
  module_label_pos = c("radial", "centroid"),
  show_module_legend = TRUE,
  legend_position = "top",
  show_domain_labels = FALSE,
  env_label_size = 1.8,
  env_label_color = "grey20",
  env_label_face = "plain",
  link_width_range = c(0.4, 1.4),
  link_width = NULL,
  link_alpha = 0.7,
  show_link_values = FALSE,
  link_value_size = 2,
  link_value_digits = 2,
  circle_radius = 2.45,
  circle_width = 0.4,
  circle_color = "grey65",
  circle_linetype = "dashed",
  outer_ring_size = 0.6,
  outer_ring_color = "grey60",
  outer_ring_alpha = 0.6,
  intra_edge_width = 0.18,
  intra_edge_color = "grey82",
  intra_edge_alpha = 0.35,
  diag_point_size = 1.8,
  diag_point_fill = "#F781BF",
  diag_point_color = "#C51B7D",
  node_size_range = c(1.1, 3.8),
  hull_alpha = 0.18,
  curvature = 0.14,
  preset = c("default", "nature", "cell", "science", "lancet"),
  scifig = FALSE,
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

- show_asterisks:

  Logical. If TRUE, shows significance asterisks. Default TRUE.

- show_values:

  Logical. If TRUE, displays numerical correlation values in tiles.
  Default FALSE.

- show_cor:

  Logical alias for `show_values`.

- label_type:

  Character string specifying tile label format: `"auto"`, `"coef"`,
  `"coef_stars"`, `"stars"`, or `"none"`.

- value_digits:

  Decimal places for correlation coefficients (default 2).

- value_size:

  Font size for tile text (default auto-scaled).

- compact:

  Logical. If TRUE (default), omits leading zero for compact fitting.

- module_label_fill:

  Background fill color for module labels (default `"white"`).

- env_label_size:

  Font size for environmental variable labels (default 1.8).

- env_label_color:

  Text color for environmental variable labels (default `"grey20"`).

- env_label_face:

  Font face for environmental variable labels (default `"plain"`).

- link_width_range:

  Line width range for inter-domain links (default `c(0.4, 1.4)`).

- link_width:

  Optional constant line width for inter-domain links. Overrides
  `link_width_range`.

- link_alpha:

  Transparency for inter-domain curved links (default 0.70).

- show_link_values:

  Logical. If TRUE, shows correlation coefficients on inter-domain links
  (default FALSE).

- link_value_size:

  Font size for link correlation digits (default 2.0).

- link_value_digits:

  Decimal places for link correlation digits (default 2).

- circle_radius:

  Radius of central boundary circle (default 2.45).

- circle_width:

  Line width of the boundary circle (default 0.4).

- circle_color:

  Color of the boundary circle (default `"grey65"`).

- circle_linetype:

  Linetype of the boundary circle (default `"dashed"`).

- outer_ring_size:

  Size of outer boundary ring dots (default 0.6).

- outer_ring_color:

  Color of outer boundary ring dots (default `"grey60"`).

- outer_ring_alpha:

  Transparency of outer boundary ring dots (default 0.6).

- intra_edge_width:

  Width of intra-module edges (default 0.18).

- intra_edge_color:

  Color of intra-module edges (default `"grey82"`).

- intra_edge_alpha:

  Transparency of intra-module edges (default 0.35).

- diag_point_size:

  Size of diagonal connector points on heatmaps (default 1.8).

- diag_point_fill:

  Fill color of diagonal connector points (default `"#F781BF"`).

- diag_point_color:

  Border color of diagonal connector points (default `"#C51B7D"`).

- node_size_range:

  Point size range for central network nodes (default `c(1.1, 3.8)`).

- hull_alpha:

  Transparency for module cluster hulls (default 0.18).

- curvature:

  Curvature of inter-domain curved links (default 0.14).

- title:

  Plot title.

- ...:

  Additional arguments.

## Value

A `ggplot` object.
