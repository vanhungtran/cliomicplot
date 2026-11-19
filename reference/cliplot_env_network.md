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
  show_asterisks = TRUE,
  show_values = FALSE,
  show_cor = NULL,
  label_type = c("auto", "stars", "coef", "coef_stars", "none"),
  value_digits = 2,
  value_size = NULL,
  compact = TRUE,
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

- title:

  Plot title.

- ...:

  Additional arguments.

## Value

A `ggplot` object.
