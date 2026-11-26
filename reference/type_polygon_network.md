# Modular Polygon Ring Network Plot Type

Creates a publication-grade modular polygon ring network layout where
community modules are placed as circular node rings along the vertices
of a macro-polygon (Wen et al. iMeta 2025). Supports both constant ring
radii (`"PolygonClusterG"`) and adaptive ring radii proportional to
module member count (`"PolygonRrClusterG"`).

## Usage

``` r
type_polygon_network(
  adaptive_radius = TRUE,
  macro_radius = 3.8,
  base_ring_radius = 1.15,
  show_module_hulls = TRUE,
  hull_alpha = 0.12,
  show_module_badges = TRUE,
  intra_edge_color = "#CBD5E1",
  intra_edge_alpha = 0.45,
  intra_edge_width = 0.25,
  inter_edge_color = "#E64B35",
  inter_edge_alpha = 0.65,
  inter_edge_width = 0.45,
  inter_edge_curved = TRUE,
  module_palette = NULL,
  node_size_range = c(1.8, 5),
  label_top = 2,
  label_size = 2.6,
  nodes = NULL,
  edges = NULL
)
```

## Arguments

- adaptive_radius:

  Logical. If TRUE (default, `"PolygonRrClusterG"`), module ring radii
  scale with \\\sqrt{N_k / \bar{N}}\\ so larger modules occupy larger
  rings. If FALSE (`"PolygonClusterG"`), all module rings have uniform
  radius.

- macro_radius:

  Numeric radius for the macro-polygon placing module centers (default
  3.8).

- base_ring_radius:

  Numeric base radius for module circular rings (default 1.15).

- show_module_hulls:

  Logical. If TRUE (default), displays shaded circular hulls behind each
  module ring.

- hull_alpha:

  Numeric transparency for module ring background hulls (default 0.12).

- show_module_badges:

  Logical. If TRUE (default), renders a central pill badge in each ring
  showing module ID and node count.

- intra_edge_color:

  Color for intra-module edges (default `"#CBD5E1"`, light slate).

- intra_edge_alpha:

  Numeric transparency for intra-module edges (default 0.45).

- intra_edge_width:

  Numeric line width for intra-module edges (default 0.25).

- inter_edge_color:

  Color or palette for cross-module edges (default `"#E64B35"`).

- inter_edge_alpha:

  Numeric transparency for cross-module edges (default 0.65).

- inter_edge_width:

  Numeric line width for cross-module edges (default 0.45).

- inter_edge_curved:

  Logical. If TRUE (default), cross-module edges are drawn as smooth
  curved arcs.

- module_palette:

  Optional named vector of colors for modules.

- node_size_range:

  Numeric vector of length 2 for node sizes mapped to intra-module
  degree (default `c(1.8, 5.0)`).

- label_top:

  Integer. Number of top hub taxa per module to label (default 2).

- label_size:

  Numeric font size for node labels (default 2.6).

- nodes:

  Optional data frame of nodes with columns `node` and `module`.

- edges:

  Optional data frame of edges with columns `from` and `to`.

## Value

A `cliplot_type` object for use with
[`cliplot`](https://vanhungtran.github.io/cliomicplot/reference/cliplot.md).

## Examples

``` r
if (FALSE) { # \dontrun{
cliplot(type = type_polygon_network(nodes = nodes_df, edges = edges_df, adaptive_radius = TRUE))
} # }
```
