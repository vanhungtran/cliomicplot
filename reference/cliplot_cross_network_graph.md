# Direct Cross-Network Architecture Plotting Function

Direct Cross-Network Architecture Plotting Function

## Usage

``` r
cliplot_cross_network_graph(
  nodes = NULL,
  edges = NULL,
  cross_edges = NULL,
  level = c("node", "module"),
  groups = NULL,
  layout = c("triangle", "horizontal", "vertical", "grid"),
  module_colors = NULL,
  cross_colors = NULL,
  show_module_labels = NULL,
  module_labels = NULL,
  module_label_size = 2.6,
  module_label_color = "grey15",
  module_label_bg = TRUE,
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

- groups:

  Group order vector.

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

- module_label_bg:

  Logical. If TRUE (default), renders labels in a semi-transparent white
  pill badge.

- title:

  Optional plot title.

- ...:

  Additional graphic arguments.

## Value

A `ggplot` object.
