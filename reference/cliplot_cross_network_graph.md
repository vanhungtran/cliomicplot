# Direct Cross-Network Architecture Plotting Function

Direct Cross-Network Architecture Plotting Function

## Usage

``` r
cliplot_cross_network_graph(
  nodes = NULL,
  edges = NULL,
  cross_edges = NULL,
  level = c("node", "module"),
  color_by = NULL,
  groups = NULL,
  group_colors = NULL,
  layout = c("triangle", "horizontal", "vertical", "grid", "concentric"),
  module_colors = NULL,
  cross_colors = NULL,
  show_module_labels = NULL,
  module_labels = NULL,
  module_label_size = 2.6,
  module_label_color = "grey15",
  module_label_fill = "white",
  module_label_bg = TRUE,
  meta_label_size = 3.2,
  meta_label_color = "grey15",
  module_size_range = c(3.5, 8.5),
  circle_radius = 1.05,
  preset = c("default", "nature", "cell", "science", "lancet", "dark", "horizontal_flow",
    "bipolar", "concentric", "minimal"),
  style = NULL,
  show_hulls = TRUE,
  hull_alpha = 0.18,
  show_meta = TRUE,
  node_size_range = c(1.2, 4.2),
  edge_color = "grey85",
  edge_alpha = 0.45,
  edge_width = 0.2,
  cross_width = 0.6,
  cross_alpha = 0.75,
  scifig = FALSE,
  title = NULL,
  ...
)
```

## Arguments

- nodes:

  Optional data frame of nodes containing columns: `id`, `group`,
  `module`, and optional `degree`, `size`, `x`, `y`. If NULL,
  demonstration data are generated.

- edges:

  Data frame of intra-group edges containing columns: `from`, `to`,
  `group`.

- cross_edges:

  Data frame of cross-group edges containing columns: `from`, `to`,
  `group1`, `group2`. If NULL, conserved features with identical `id`
  across groups are linked automatically.

- level:

  Visualization level: `"node"` (default) or `"module"`.

- color_by:

  Character string specifying node color mapping: `"group"` or
  `"module"`. Defaults to `"group"` when `level = "module"` and
  `"module"` when `level = "node"`.

- groups:

  Group order vector.

- group_colors:

  Optional named vector of colors for experimental groups.

- layout:

  Group placement: `"triangle"`, `"horizontal"`, `"vertical"`, or
  `"grid"`.

- module_colors:

  Named color vector for modules.

- cross_colors:

  Named color vector for cross-group links.

- show_module_labels:

  Logical. If TRUE, displays labels for module components in the
  circles. Defaults to TRUE when `level = "module"` and FALSE when
  `level = "node"`.

- module_labels:

  Optional named vector of custom labels for modules.

- module_label_size:

  Numeric font size for module labels (default 2.6).

- module_label_color:

  Color of module labels (default `"grey15"`).

- module_label_fill:

  Background fill color for module labels (default `"white"`).

- module_label_bg:

  Logical. If TRUE (default), renders labels in a semi-transparent white
  pill badge.

- meta_label_size:

  Font size for group metadata text (default 3.2).

- meta_label_color:

  Text color for group metadata text (default `"grey15"`).

- module_size_range:

  Size range for module circles (default `c(3.5, 8.5)`).

- circle_radius:

  Radius for module circle ring layout (default 1.05).

- title:

  Optional plot title.

- ...:

  Additional graphic arguments.

## Value

A `ggplot` object.
