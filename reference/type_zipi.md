# Zi-Pi Keystone Ecological Role Plot Type

Creates a publication-grade Zi-Pi topological role plot for microbial
co-occurrence networks and multi-omics communities (Wen et al. iMeta
2025; Guimera & Amaral Nature 2005). Partitions the \\P_i\\ vs \\Z_i\\
Cartesian plane into four ecological roles:

- **Peripherals** (\\Z_i \le 2.5, P_i \le 0.62\\): Specialists with few
  inter-module links.

- **Connectors** (\\Z_i \le 2.5, P_i \> 0.62\\): Generalists linking
  distinct modules.

- **Module hubs** (\\Z_i \> 2.5, P_i \le 0.62\\): Highly connected
  specialists within their module.

- **Network hubs** (\\Z_i \> 2.5, P_i \> 0.62\\): Super-generalists
  critical to network stability.

## Usage

``` r
type_zipi(
  z_threshold = 2.5,
  p_threshold = 0.62,
  show_quadrants = TRUE,
  quadrant_alpha = 0.06,
  quadrant_labels = TRUE,
  label_hubs = TRUE,
  label_top = 8,
  label_size = 3,
  point_size = 2.8,
  point_alpha = 0.85,
  show_rug = TRUE,
  show_summary = TRUE,
  role_colors = NULL,
  adj_mat = NULL,
  modules = NULL
)
```

## Arguments

- z_threshold:

  Numeric cutoff for module hubs on \\Z_i\\ (default 2.5).

- p_threshold:

  Numeric cutoff for connectors on \\P_i\\ (default 0.62).

- show_quadrants:

  Logical. If TRUE (default), displays soft shaded background tiles for
  each of the 4 topological quadrants with watermark role labels.

- quadrant_alpha:

  Numeric transparency for background quadrant tiles (default 0.05).

- quadrant_labels:

  Logical. If TRUE (default), renders small text badges naming each
  quadrant.

- label_hubs:

  Logical. If TRUE (default), labels keystone nodes (Network hubs,
  Module hubs, Connectors).

- label_top:

  Integer. Maximum number of keystone taxa to label (default 8).

- label_size:

  Numeric size for node text labels (default 3.0).

- point_size:

  Numeric base point size, or column name for continuous sizing (default
  2.8).

- point_alpha:

  Numeric transparency for points (default 0.85).

- show_rug:

  Logical. If TRUE (default), adds marginal rugs along axes to indicate
  point densities.

- show_summary:

  Logical. If TRUE (default), adds an automated summary banner with
  percentage tallies.

- role_colors:

  Optional named vector of colors for roles.

- adj_mat:

  Optional adjacency matrix. If supplied, \\Z_i\\ and \\P_i\\ are
  computed automatically.

- modules:

  Optional module assignments when `adj_mat` is provided.

## Value

A `cliplot_type` object for use with
[`cliplot`](https://vanhungtran.github.io/cliomicplot/reference/cliplot.md).

## Examples

``` r
if (FALSE) { # \dontrun{
# From pre-calculated Zi and Pi:
cliplot(Zi ~ Pi | Role, data = zipi_df, type = type_zipi())

# From adjacency matrix:
cliplot(type = type_zipi(adj_mat = net_adj, modules = net_mods))
} # }
```
