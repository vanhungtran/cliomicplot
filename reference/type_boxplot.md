# Boxplot Type

Creates publication-ready boxplots with optional jittered points, violin
overlays, statistical annotations, and specialized styling. Supports
classic cliplot (\`"default"\`), Nature Aging pathology style
(\`"pathology"\`), and David Baranger (2018) scatter-box-violin
composites (\`"baranger"\`, \`"scatter_box_violin"\`, \`"scatter_box"\`,
\`"box_violin"\`).

## Usage

``` r
type_boxplot(
  style = c("default", "pathology", "baranger", "scatter_box_violin", "scatter_box",
    "box_violin"),
  add_jitter = NULL,
  jitter_width = NULL,
  jitter_alpha = NULL,
  jitter_size = NULL,
  jitter_subsample = NULL,
  point_shape = NULL,
  point_border = "black",
  point_stroke = 0.35,
  add_violin = NULL,
  violin_alpha = NULL,
  violin_width = NULL,
  violin_color = NULL,
  violin_linewidth = 0.55,
  trim = TRUE,
  scale = "width",
  notch = NULL,
  notchwidth = 0.5,
  outlier_shape = NA,
  outlier_size = 1.5,
  width = NULL,
  alpha = NULL,
  linewidth = NULL,
  box_color = NULL,
  position_dodge = NULL,
  show_mean = NULL,
  show_mean_line = NULL,
  mean_shape = 23,
  mean_fill = "#FFFFFF",
  mean_color = "black",
  mean_size = 2,
  mean_stroke = 0.45,
  mean_line_lty = "dotted",
  mean_line_col = "grey45",
  mean_line_lwd = 0.38,
  bold_median = NULL,
  bold_median_col = "#111111",
  bold_median_lwd = 0.85,
  zero_line = NULL,
  zero_line_y = 0,
  zero_line_col = "grey75",
  zero_line_lty = "dashed"
)
```

## Arguments

- style:

  Boxplot aesthetic style: \`"default"\` (classic cliplot),
  \`"pathology"\` (Nature Aging / SciFig style with colored borders,
  bold median, white diamond mean, dotted mean trajectory lines, and
  dashed zero baseline), \`"baranger"\` / \`"scatter_box_violin"\`
  (David Baranger style with wide violin, narrow notched boxplot, and
  overlaid jittered points with black border), \`"scatter_box"\`
  (notched boxplot with bordered jittered points), or \`"box_violin"\`
  (wide violin with narrow notched boxplot, no points).

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

- point_shape:

  Point shape for raw data scatter points. \`21\` renders filled circles
  with black borders (default in Baranger style); \`16\` renders solid
  circular points (default in classic/pathology styles).

- point_border:

  Border outline color for points when \`point_shape\` is 21–25 (default
  \`"black"\`).

- point_stroke:

  Border stroke thickness for points (default \`0.35\`).

- add_violin:

  Overlay violin plot (default FALSE for classic/pathology; default TRUE
  for Baranger style).

- violin_alpha:

  Violin transparency (default 0.20 for classic, 0.30 for baranger).

- violin_width:

  Width of violin layer (default \`0.80\` for baranger).

- violin_color:

  Outline color of violin layer (\`"black"\` for baranger, \`NA\` for
  classic).

- violin_linewidth:

  Line width of violin outline (default \`0.55\`).

- trim:

  Logical. Trim violin tails to observed data range (default TRUE).

- scale:

  Density scaling method for violin: \`"width"\` (default), \`"area"\`,
  or \`"count"\`.

- notch:

  Logical. Draw notched boxplots showing the 95 around the median
  (default TRUE in Baranger styles, FALSE otherwise).

- notchwidth:

  Notch depth relative to box width (default \`0.5\`).

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

- box_color:

  Custom outline color for boxplots (default \`"black"\` for baranger,
  \`"#30343B"\` for default, or group-matched for pathology).

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

## References

Baranger, D. (2018). Showing your data: Scatter, box, & violin plots.
<https://davidbaranger.com/2018/03/05/showing-your-data-scatter-box-violin-plots/>

## Examples

``` r
if (FALSE) { # \dontrun{
# Basic publication-style boxplot (transparent fill, no outliers)
cliplot(len ~ dose, data = ToothGrowth, type = "boxplot")

# David Baranger style scatter-box-violin composite
cliplot(Sepal.Length ~ Species, data = iris,
        type = type_boxplot(style = "baranger"))

# Nature Aging / SciFig pathology style boxplot
cliplot(delta ~ pathology | status, data = df_gtex,
        type = type_pathology_boxplot(),
        palette = "pathology", theme = "pathology")
} # }
```
