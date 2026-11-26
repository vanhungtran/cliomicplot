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
  comm_data = NULL,
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
  preset = c("default", "nature", "clinical", "seasonal", "cell", "dark", "lancet",
    "minimal", "multi_tissue", "pharmacology", "circadian", "diurnal", "bipolar",
    "response"),
  style = NULL,
  scifig = FALSE,
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

- show_values:

  Logical. If TRUE, displays numerical correlation values in correlation
  tiles. Default FALSE.

- show_cor:

  Logical alias for `show_values`. If non-NULL, overrides `show_values`.

- label_type:

  Character string specifying tile annotation format: `"auto"` (default,
  determined by `show_values` and `show_asterisks`), `"coef"` (e.g.
  `".42"`), `"coef_stars"` (e.g. `".42***"`), `"stars"` (e.g. `"***"`),
  or `"none"`.

- value_digits:

  Integer number of decimal places for correlation coefficients (default
  2).

- value_size:

  Optional numeric font size for tile text. If NULL (default),
  automatically calculated based on tile step dimensions so numbers fit
  inside without clipping.

- compact:

  Logical. If TRUE (default), strips leading zeros (e.g. `".42"` instead
  of `"0.42"`) for compact, high-density tile fitting.

- show_module_labels:

  Logical. If TRUE (default), displays text labels for
  modules/components inside the central circle.

- module_labels:

  Optional named character vector or list mapping module IDs (e.g.
  `"Mod_1"`) to custom display labels (e.g.
  `c("Mod_1" = "M1: S. aureus core")`).

- module_label_size:

  Numeric font size for module labels inside the circle (default 2.4).

- module_label_color:

  Color of module label text and borders (default `"grey15"`).

- module_label_fill:

  Background fill color for module labels (default `"white"`).

- module_label_bg:

  Logical. If TRUE (default), renders labels inside rounded opaque
  badges.

- module_label_alpha:

  Numeric fill transparency for module label badges (default 1.0, fully
  opaque).

- module_label_pos:

  Placement of module labels: `"radial"` (default, placed radially
  outward toward the circle boundary to avoid overlapping dense core
  nodes) or `"centroid"` (placed directly at module centroid).

- show_module_legend:

  Logical. If TRUE, shows the module color legend. Default TRUE.

- legend_position:

  Position of the legend: `"top"` (default), `"bottom"`, `"right"`,
  `"left"`, or `"none"`.

- show_domain_labels:

  Logical. If TRUE, displays domain headers for the corner heatmaps.
  Default TRUE.

- env_label_size:

  Font size for environmental variable labels outside corner heatmaps
  (default 1.95).

- env_label_color:

  Text color for environmental variable labels (default `"grey20"`).

- env_label_face:

  Font face for environmental variable labels (default `"plain"`).

- link_width_range:

  Numeric vector of length 2 specifying line width range for
  inter-domain curved links scaled by significance (default
  `c(0.4, 1.4)`).

- link_width:

  Optional numeric constant line width for inter-domain links. If
  non-NULL, overrides `link_width_range`.

- link_alpha:

  Transparency for inter-domain curved links (default 0.70).

- show_link_values:

  Logical. If TRUE, displays numerical correlation coefficients on the
  curved inter-domain links (default FALSE).

- link_value_size:

  Font size for correlation digits on inter-domain links (default 2.0).

- link_value_digits:

  Integer number of decimal places for link correlation digits (default
  2).

- circle_radius:

  Numeric radius of the central boundary circle (default 2.80).

- circle_width:

  Line width of the central boundary circle (default 0.4).

- circle_color:

  Line color of the central boundary circle (default `"grey65"`).

- circle_linetype:

  Linetype of the central boundary circle (default `"dashed"`).

- outer_ring_size:

  Size of dots along the outer boundary circle (default 0.6).

- outer_ring_color:

  Color of dots along the outer boundary circle (default `"grey60"`).

- outer_ring_alpha:

  Transparency of dots along the outer boundary circle (default 0.6).

- intra_edge_width:

  Line width of intra-module network edges (default 0.18).

- intra_edge_color:

  Color of intra-module network edges (default `"grey82"`).

- intra_edge_alpha:

  Transparency of intra-module network edges (default 0.35).

- diag_point_size:

  Size of diagonal indicator connector points on heatmaps (default 2.0).

- diag_point_fill:

  Fill color of diagonal indicator points (default `"#F781BF"`).

- diag_point_color:

  Border color of diagonal indicator points (default `"#C51B7D"`).

- node_size_range:

  Numeric vector of length 2 specifying size range for network nodes
  (default `c(1.1, 3.8)`).

- hull_alpha:

  Shading transparency for module cluster hulls (default 0.18).

- curvature:

  Numeric curvature of inter-domain connecting lines (default 0.14).

- preset:

  Character string specifying publisher style preset: `"default"`,
  `"nature"`, `"clinical"`, `"seasonal"`, `"cell"`, `"dark"`,
  `"lancet"`, `"minimal"`, `"multi_tissue"` (or `"pharmacology"`),
  `"circadian"` (or `"diurnal"`), or `"bipolar"` (or `"response"`).

- style:

  Character string alias for `preset`. If supplied, overrides `preset`.

- scifig:

  Logical. If TRUE, enforces AI4SciFig publication gates (font floor \>=
  5 pt, line floor \>= 0.25 pt, zero sign suppression, and CVD compliant
  palettes).

- title:

  Optional plot title.

## Value

A `cliplot_type` object for use with
[`cliplot`](https://vanhungtran.github.io/cliomicplot/reference/cliplot.md).
