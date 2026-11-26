# Grammar Layer: Peripheral Triangular Correlation Heatmaps

Renders the 1 to 4 peripheral triangular correlation heatmaps, including
correlation tiles, off-diagonal significance asterisks/digits, diagonal
connector badges, outer 78° angled labels, and domain category titles.

## Usage

``` r
geom_env_corner_heatmaps(
  data = NULL,
  diag_point_size = 2,
  diag_point_fill = "#F781BF",
  env_label_size = 1.95,
  env_label_color = "grey20",
  env_label_face = "plain",
  show_domain_labels = TRUE
)

geom_env_heatmaps(
  data = NULL,
  diag_point_size = 2,
  diag_point_fill = "#F781BF",
  env_label_size = 1.95,
  env_label_color = "grey20",
  env_label_face = "plain",
  show_domain_labels = TRUE
)
```

## Arguments

- data:

  Optional `cli_env_network_data` object.

- diag_point_size:

  Size of diagonal indicator connector points (default 2.0).

- diag_point_fill:

  Fill color of diagonal indicator points (default `"#F781BF"`).

- env_label_size:

  Font size for environmental variable labels (default 1.95).

- env_label_color:

  Color for environmental variable labels (default `"grey20"`).

- env_label_face:

  Font face for environmental variable labels (default `"plain"`).

- show_domain_labels:

  Logical. If TRUE, renders domain category headers (default TRUE).

## Value

A list of `ggplot2` layers.
