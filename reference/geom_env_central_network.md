# Grammar Layer: Central Modular Network Nodes, Edges, and Hulls

Renders the central modular co-occurrence network, including
intra-module edge connections, soft cluster convex hulls, node glyphs
(shape 16 fill-only), and radially positioned module badges.

## Usage

``` r
geom_env_central_network(
  data = NULL,
  hull_alpha = 0.18,
  intra_edge_width = 0.18,
  intra_edge_color = "grey82",
  intra_edge_alpha = 0.35,
  node_size_range = c(1.1, 3.8),
  show_module_labels = TRUE,
  module_label_bg = TRUE,
  module_label_fill = "white",
  module_label_color = "grey15",
  module_label_size = 2.4,
  module_label_alpha = 1
)

geom_central_network(
  data = NULL,
  hull_alpha = 0.18,
  intra_edge_width = 0.18,
  intra_edge_color = "grey82",
  intra_edge_alpha = 0.35,
  node_size_range = c(1.1, 3.8),
  show_module_labels = TRUE,
  module_label_bg = TRUE,
  module_label_fill = "white",
  module_label_color = "grey15",
  module_label_size = 2.4,
  module_label_alpha = 1
)
```

## Arguments

- data:

  Optional `cli_env_network_data` object.

- hull_alpha:

  Shading transparency for module cluster hulls (default 0.18).

- intra_edge_width:

  Width of intra-module edges (default 0.18).

- intra_edge_color:

  Color of intra-module edges (default `"grey82"`).

- intra_edge_alpha:

  Transparency of intra-module edges (default 0.35).

- node_size_range:

  Size range for central network nodes (default `c(1.1, 3.8)`).

- show_module_labels:

  Logical. If TRUE, displays module label badges (default TRUE).

- module_label_bg:

  Logical. If TRUE, renders badge boxes behind labels (default TRUE).

- module_label_fill:

  Background fill color for module labels (default `"white"`).

- module_label_color:

  Text color for module labels (default `"grey15"`).

- module_label_size:

  Font size for module labels (default 2.4).

- module_label_alpha:

  Transparency for module label background (default 1.0).

## Value

A list of `ggplot2` layers.
