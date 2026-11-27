# Scatter-Box-Violin Plot Type (David Baranger Style)

Creates publication-ready distribution plots layering three
complementary representations of data as advocated by David Baranger
(2018): 1. A background semi-transparent violin plot showing the
probability density and shape of the distribution. 2. A narrow
foreground notched boxplot displaying summary statistics (median, 95 3.
An overlay of individual jittered raw data points (with black outline
and group fill) ensuring complete data transparency. Supports both
single-factor and dodged multi-factor groups.

## Usage

``` r
type_scatter_box_violin(
  notch = TRUE,
  notchwidth = 0.5,
  point_shape = 21,
  point_border = "black",
  point_stroke = 0.35,
  add_violin = TRUE,
  violin_alpha = 0.3,
  violin_width = 0.8,
  violin_color = "black",
  violin_linewidth = 0.55,
  trim = TRUE,
  scale = "width",
  add_jitter = TRUE,
  jitter_width = 0.12,
  jitter_size = 1.8,
  jitter_alpha = 0.65,
  jitter_subsample = NULL,
  width = 0.22,
  alpha = 0.75,
  linewidth = 0.65,
  box_color = "black",
  position_dodge = 0.8,
  outlier_shape = NA,
  outlier_size = 1.5,
  ...
)

type_baranger(
  notch = TRUE,
  notchwidth = 0.5,
  point_shape = 21,
  point_border = "black",
  point_stroke = 0.35,
  add_violin = TRUE,
  violin_alpha = 0.3,
  violin_width = 0.8,
  violin_color = "black",
  violin_linewidth = 0.55,
  trim = TRUE,
  scale = "width",
  add_jitter = TRUE,
  jitter_width = 0.12,
  jitter_size = 1.8,
  jitter_alpha = 0.65,
  jitter_subsample = NULL,
  width = 0.22,
  alpha = 0.75,
  linewidth = 0.65,
  box_color = "black",
  position_dodge = 0.8,
  outlier_shape = NA,
  outlier_size = 1.5,
  ...
)

type_sbv(
  notch = TRUE,
  notchwidth = 0.5,
  point_shape = 21,
  point_border = "black",
  point_stroke = 0.35,
  add_violin = TRUE,
  violin_alpha = 0.3,
  violin_width = 0.8,
  violin_color = "black",
  violin_linewidth = 0.55,
  trim = TRUE,
  scale = "width",
  add_jitter = TRUE,
  jitter_width = 0.12,
  jitter_size = 1.8,
  jitter_alpha = 0.65,
  jitter_subsample = NULL,
  width = 0.22,
  alpha = 0.75,
  linewidth = 0.65,
  box_color = "black",
  position_dodge = 0.8,
  outlier_shape = NA,
  outlier_size = 1.5,
  ...
)

type_scatter_box(
  notch = TRUE,
  point_shape = 21,
  point_border = "black",
  point_stroke = 0.35,
  add_jitter = TRUE,
  jitter_width = 0.15,
  jitter_size = 1.8,
  jitter_alpha = 0.65,
  width = 0.45,
  alpha = 0.75,
  linewidth = 0.65,
  box_color = "black",
  position_dodge = 0.8,
  ...
)

type_box_violin(
  notch = TRUE,
  add_violin = TRUE,
  violin_alpha = 0.3,
  violin_width = 0.8,
  violin_color = "black",
  violin_linewidth = 0.55,
  trim = TRUE,
  scale = "width",
  width = 0.22,
  alpha = 0.75,
  linewidth = 0.65,
  box_color = "black",
  position_dodge = 0.8,
  ...
)
```

## Arguments

- notch:

  Logical. Draw notched boxplots showing the 95 around the median
  (default TRUE in Baranger styles, FALSE otherwise).

- notchwidth:

  Notch depth relative to box width (default \`0.5\`).

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

- add_jitter:

  Add jittered points (default TRUE).

- jitter_width:

  Width of jitter (default 0.15 for default, 0.10 for pathology, 0.12
  for baranger).

- jitter_size:

  Size of jittered points (default 1.6 for default, 0.85 for pathology,
  1.8 for baranger).

- jitter_alpha:

  Transparency of jittered points (default 0.38 for default, 0.35 for
  pathology, 0.65 for baranger).

- jitter_subsample:

  Optional integer. When specified, subsamples up to
  \`jitter_subsample\` observations per category/group for jittered
  point display while computing boxplots and statistics on all data
  (ideal for large cohorts such as GTEx with N ~ 20k).

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

- outlier_shape:

  Shape for outlier points. Set to \`NA\` to hide outliers (publication
  style). Default \`NA\`.

- outlier_size:

  Size of outlier points (default 1.5).

## Value

A cliplot_type object for use with
[`cliplot`](https://vanhungtran.github.io/cliomicplot/reference/cliplot.md).

## References

Baranger, D. (2018). Showing your data: Scatter, box, & violin plots.
<https://davidbaranger.com/2018/03/05/showing-your-data-scatter-box-violin-plots/>

## Examples

``` r
if (FALSE) { # \dontrun{
# Classic Baranger scatter-box-violin plot on single factor
cliplot(Sepal.Length ~ Species, data = iris,
        type = type_scatter_box_violin())

# Grouped / dodged Baranger plot with 2-way layout
cliplot(len ~ dose | supp, data = ToothGrowth,
        type = type_scatter_box_violin(notch = FALSE),
        palette = "nature")
} # }
```
