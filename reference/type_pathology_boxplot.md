# Pathology Boxplot Type (Nature Aging / SciFig Style)

Creates publication-grade pathology boxplots conforming to Nature Aging
and SciFig standards (as seen in Yadav et al. 2026). Features
semi-transparent boxes with status-matched borders, bold black
horizontal medians, diamond mean markers, dotted trajectory lines
connecting paired means across conditions, dashed zero reference
baseline, and optional subsampling for large-cohort biological
replicates.

## Usage

``` r
type_pathology_boxplot(
  add_jitter = TRUE,
  jitter_width = 0.1,
  jitter_alpha = 0.35,
  jitter_size = 0.85,
  jitter_subsample = NULL,
  add_violin = FALSE,
  violin_alpha = 0.2,
  outlier_shape = NA,
  outlier_size = 1.5,
  width = 0.32,
  alpha = 0.22,
  linewidth = 0.45,
  position_dodge = 0.42,
  show_mean = TRUE,
  show_mean_line = TRUE,
  mean_shape = 23,
  mean_fill = "#FFFFFF",
  mean_color = "black",
  mean_size = 2,
  mean_stroke = 0.45,
  mean_line_lty = "dotted",
  mean_line_col = "grey45",
  mean_line_lwd = 0.38,
  bold_median = TRUE,
  bold_median_col = "#111111",
  bold_median_lwd = 0.85,
  zero_line = TRUE,
  zero_line_y = 0,
  zero_line_col = "grey75",
  zero_line_lty = "dashed"
)
```

## Arguments

- add_jitter:

  Add jittered points (default TRUE).

- jitter_width:

  Width of jitter (default 0.15 for default, 0.10 for pathology, 0.12
  for baranger).

- jitter_alpha:

  Transparency of jittered points (default 0.38 for default, 0.35 for
  pathology, 0.65 for baranger).

- jitter_size:

  Size of jittered points (default 1.6 for default, 0.85 for pathology,
  1.8 for baranger).

- jitter_subsample:

  Optional integer. When specified, subsamples up to
  \`jitter_subsample\` observations per category/group for jittered
  point display while computing boxplots and statistics on all data
  (ideal for large cohorts such as GTEx with N ~ 20k).

- add_violin:

  Overlay violin plot (default FALSE for classic/pathology; default TRUE
  for Baranger style).

- violin_alpha:

  Violin transparency (default 0.20 for classic, 0.30 for baranger).

- outlier_shape:

  Shape for outlier points. Set to \`NA\` to hide outliers (publication
  style). Default \`NA\`.

- outlier_size:

  Size of outlier points (default 1.5).

- width:

  Box width (default 0.55 for default, 0.32 for pathology, 0.22 for
  baranger).

- alpha:

  Box fill transparency (default 0.28 for default, 0.22 for pathology,
  0.75 for baranger).

- linewidth:

  Box outline width (default 0.65 for default, 0.45 for pathology).

- position_dodge:

  Dodge width for grouped boxes. Default 0.8 for default/baranger, 0.42
  for pathology style.

- show_mean:

  Logical. Show mean marker diamond (default \`TRUE\` when \`style =
  "pathology"\`, \`FALSE\` otherwise).

- show_mean_line:

  Logical. Show dotted connecting line between paired means within each
  x category (default \`TRUE\` when \`style = "pathology"\`, \`FALSE\`
  otherwise).

- mean_shape:

  Shape for mean marker (default \`23\`, diamond).

- mean_fill:

  Fill color for mean marker (default \`"#FFFFFF"\`, white).

- mean_color:

  Border color for mean marker (default \`"black"\`).

- mean_size:

  Size of mean marker (default \`2.0\`).

- mean_stroke:

  Border thickness for mean marker (default \`0.45\`).

- mean_line_lty:

  Linetype for paired mean connecting line (default \`"dotted"\`).

- mean_line_col:

  Color for paired mean connecting line (default \`"grey45"\`).

- mean_line_lwd:

  Linewidth for paired mean connecting line (default \`0.38\`).

- bold_median:

  Logical. Draw a bold black horizontal line across each box for the
  median (default \`TRUE\` when \`style = "pathology"\`, \`FALSE\`
  otherwise).

- bold_median_col:

  Color for bold median line (default \`"#111111"\`).

- bold_median_lwd:

  Linewidth for bold median line (default \`0.85\`).

- zero_line:

  Logical. Draw a horizontal reference line at y = 0 (default \`TRUE\`
  when \`style = "pathology"\`, \`FALSE\` otherwise).

- zero_line_y:

  Intercept for zero reference line (default \`0\`).

- zero_line_col:

  Color for zero reference line (default \`"grey75"\`).

- zero_line_lty:

  Linetype for zero reference line (default \`"dashed"\`).

## Value

A cliplot_type object for use with
[`cliplot`](https://vanhungtran.github.io/cliomicplot/reference/cliplot.md).

## Examples

``` r
if (FALSE) { # \dontrun{
cliplot(delta ~ pathology | status, data = df_gtex,
        type = type_pathology_boxplot(),
        palette = "pathology", theme = "pathology")
} # }
```
