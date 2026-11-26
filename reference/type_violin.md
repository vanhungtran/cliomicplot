# Violin Plot Type

Creates publication-ready violin plots with rich interior anatomy
options (boxplots, quartile lines, mean-SE glyphs), optional single-cell
jittered raw data points, split-half condition comparisons, and custom
density scaling.

## Usage

``` r
type_violin(
  inner = c("boxplot", "quartiles", "mean_se", "median_iqr", "none"),
  quantiles = c(0.25, 0.5, 0.75),
  quantile_color = "grey25",
  quantile_linewidth = 0.45,
  quantile_linetype = "solid",
  split = FALSE,
  add_boxplot = TRUE,
  add_jitter = TRUE,
  jitter_width = 0.15,
  jitter_alpha = 0.45,
  jitter_size = 1.5,
  violin_alpha = 0.78,
  violin_color = "#30343B",
  linewidth = 0.3,
  box_width = 0.18,
  box_fill = "white",
  box_alpha = 0.82,
  trim = TRUE,
  scale = "width",
  orientation = c("vertical", "horizontal"),
  show_n = FALSE
)
```

## Arguments

- inner:

  Interior summary structure: `"boxplot"` (default, overlay
  mini-boxplot), `"quartiles"` (draw 25%, median, and 75% density
  quantile lines), `"mean_se"` (mean diamond glyph with standard error
  errorbars), `"median_iqr"` (median circle with interquartile range
  crossbar), or `"none"`.

- quantiles:

  Numeric vector of quantile lines to draw when `inner = "quartiles"`
  (default `c(0.25, 0.5, 0.75)`).

- quantile_color:

  Color for quantile lines (default `"grey25"`).

- quantile_linewidth:

  Line width for quantile lines (default 0.45).

- quantile_linetype:

  Line type for quantile lines (default `"solid"`).

- split:

  Logical. When `TRUE` and a 2-level grouping variable is present,
  renders split violins comparing both conditions back-to-back (default
  `FALSE`).

- add_boxplot:

  Backward-compatible flag to overlay boxplot (default `TRUE`). If set
  to `FALSE`, overrides `inner = "none"`.

- add_jitter:

  Overlay jittered raw data points (default `TRUE`).

- jitter_width:

  Width of horizontal jitter (default 0.15).

- jitter_alpha:

  Transparency of jittered points (default 0.45).

- jitter_size:

  Size of jittered points (default 1.5).

- violin_alpha:

  Violin fill transparency (default 0.78).

- violin_color:

  Outline color of violins (default `"#30343B"`).

- linewidth:

  Outline stroke width (default 0.3).

- box_width:

  Width of boxplot overlay (default 0.18).

- box_fill:

  Fill color of boxplot overlay (default `"white"`).

- box_alpha:

  Transparency of boxplot overlay (default 0.82).

- trim:

  Trim violin tails to observed data range (default `TRUE`).

- scale:

  Density scaling method: `"width"` (default, all categories have equal
  maximum width), `"area"`, or `"count"`.

- orientation:

  Plot orientation: `"vertical"` (default) or `"horizontal"`.

- show_n:

  Logical. Annotate sample sizes (\\n = \dots\\) below categories
  (default `FALSE`).

## Value

A cliplot_type object for use with
[`cliplot`](https://vanhungtran.github.io/cliomicplot/reference/cliplot.md).

## Examples

``` r
if (FALSE) { # \dontrun{
# Standard publication violin with inner boxplot and jitter
cliplot(len ~ dose, data = ToothGrowth, type = "violin")

# Violin with interior quartile lines
cliplot(len ~ dose, data = ToothGrowth,
        type = type_violin(inner = "quartiles"))

# Mean diamond glyph with standard error bars
cliplot(len ~ dose | supp, data = ToothGrowth,
        type = type_violin(inner = "mean_se", add_jitter = TRUE))

# Split violin comparing two conditions back-to-back
cliplot(len ~ dose | supp, data = ToothGrowth,
        type = type_violin(split = TRUE, inner = "quartiles"))
} # }
```
