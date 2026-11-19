# Multi-Group Cross-Network Architecture Graph

Creates publication-ready multi-group network comparison graphs to
visualize co-occurrence networks, module conservation, and cross-network
shared links across multiple experimental groups or conditions (e.g. KO
vs WT vs OE, Healthy vs Lesional vs Non-Lesional, or Multi-Cohort
comparisons). Supports two representations:

- `level = "node"`: Full network topology with modular clusters, shaded
  transparent module hulls, intra-group edges, and inter-group
  cross-links connecting shared or conserved features.

- `level = "module"`: Module-level circular ring architecture where each
  group is represented as a ring of module nodes sized by module member
  count, with inter-group links showing module-level preservation.

## Usage

``` r
type_cross_network_graph(
  level = c("node", "module"),
  groups = NULL,
  layout = c("triangle", "horizontal", "vertical", "grid"),
  module_colors = NULL,
  cross_colors = NULL,
  show_hulls = TRUE,
  hull_alpha = 0.18,
  show_meta = TRUE,
  node_size_range = c(1.2, 4.2),
  edge_color = "grey85",
  edge_alpha = 0.45,
  edge_width = 0.2,
  cross_width = 0.6,
  cross_alpha = 0.75,
  show_module_labels = NULL,
  module_labels = NULL,
  module_label_size = 2.6,
  module_label_color = "grey15",
  module_label_bg = TRUE
)
```

## Arguments

- level:

  Character string specifying visualization level: `"node"` (default)
  for full node-edge topology, or `"module"` for circular module ring
  preservation.

- groups:

  Character vector specifying group names in desired layout order.

- layout:

  Character string specifying group layout: `"triangle"` (default for 3
  groups), `"horizontal"`, `"vertical"`, or `"grid"`.

- module_colors:

  Optional named vector of colors for modules.

- cross_colors:

  Optional named vector of colors for inter-group cross links (e.g.
  `c("KO - WT" = "#E7298A")`).

- show_hulls:

  Logical. If TRUE, draws semi-transparent shaded hulls around modules
  (node level). Default TRUE.

- hull_alpha:

  Numeric transparency for module hulls (default 0.18).

- show_meta:

  Logical. If TRUE, prints group metadata (Node and Edge counts).
  Default TRUE.

- node_size_range:

  Numeric vector of length 2 specifying point size range (default
  `c(1.2, 4.2)`).

- edge_color:

  Color for intra-group edges (default `"grey85"`).

- edge_alpha:

  Transparency for intra-group edges (default `0.45`).

- edge_width:

  Width for intra-group edges (default `0.2`).

- cross_width:

  Width for cross-group links (default `0.6`).

- cross_alpha:

  Transparency for cross-group links (default `0.75`).

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

## Value

A `cliplot_type` object for use with
[`cliplot`](https://vanhungtran.github.io/cliomicplot/reference/cliplot.md).
