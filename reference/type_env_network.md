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
  module_label_bg = TRUE,
  module_label_alpha = 1,
  module_label_pos = c("radial", "centroid"),
  curvature = 0.14,
  circle_radius = 2.45,
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

  Numeric font size for module labels inside the circle (default 2.8).

- module_label_color:

  Color of module label text and borders (default `"grey15"`).

- module_label_bg:

  Logical. If TRUE (default), renders labels inside rounded opaque white
  badges for high contrast against network nodes and links.

- module_label_alpha:

  Numeric fill transparency for module label badges (default 1.0, fully
  opaque).

- module_label_pos:

  Placement of module labels: `"radial"` (default, placed radially
  outward toward the circle boundary to avoid overlapping dense core
  nodes) or `"centroid"` (placed directly at module centroid).

- curvature:

  Numeric curvature of inter-domain connecting lines (default 0.14).

- circle_radius:

  Numeric radius of the central boundary circle (default 2.45).

- title:

  Optional plot title.

## Value

A `cliplot_type` object for use with
[`cliplot`](https://vanhungtran.github.io/cliomicplot/reference/cliplot.md).
