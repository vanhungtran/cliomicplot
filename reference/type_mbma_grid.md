# MBMA Trial Design Grid Heatmap Type

Creates simulation matrix heatmaps for clinical trial design exploration
(Figure 3A in Serrano et al. 2025). Displays predicted response rates
across combinations of baseline covariates (e.g. baseline severity
scores), assessment timepoints (e.g. Weeks 8, 12, 16), and trial design
policies (e.g. Monotherapy vs. Concomitant TCS). Each cell displays the
central percentage in bold with its corresponding `± 95% CI` margin
underneath.

## Usage

``` r
type_mbma_grid(
  ci_margin = NULL,
  show_labels = TRUE,
  label_digits = 1,
  palette = c("blues", "teal", "viridis", "sunset"),
  tile_border = "#FFFFFF",
  tile_border_lwd = 0.75,
  text_size = 3.4,
  ci_size = 2.4,
  ...
)

type_trial_design_grid(
  ci_margin = NULL,
  show_labels = TRUE,
  label_digits = 1,
  palette = c("blues", "teal", "viridis", "sunset"),
  tile_border = "#FFFFFF",
  tile_border_lwd = 0.75,
  text_size = 3.4,
  ci_size = 2.4,
  ...
)
```

## Arguments

- ci_margin:

  Column name for confidence interval half-width (margin), or `NULL` if
  already formatted or not shown.

- show_labels:

  Logical. Whether to print two-line text annotations inside cells
  (default `TRUE`).

- label_digits:

  Number of decimal digits for percentage values (default `1`).

- palette:

  Color gradient palette name: `"blues"` (default), `"teal"`,
  `"viridis"`, or `"sunset"`.

- tile_border:

  Color for tile dividing gridlines (default `"#FFFFFF"`).

- tile_border_lwd:

  Linewidth for tile borders (default `0.75`).

- text_size:

  Numeric text size for cell estimates (default `3.4`).

- ci_size:

  Numeric text size for CI margin text (default `2.4`).

- ...:

  Additional arguments.

## Value

A `cliplot_type` object for use with
[`cliplot`](https://vanhungtran.github.io/cliomicplot/reference/cliplot.md).
