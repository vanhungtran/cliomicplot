# Split Violin Plot Type

Creates publication-ready split violin plots comparing two conditions
(such as Healthy vs Diseased, Control vs Treatment, or Responder vs
Non-Responder) side-by-side as left and right halves sharing a common
categorical axis.

## Usage

``` r
type_split_violin(
  inner = c("quartiles", "boxplot", "mean_se", "none"),
  quantiles = c(0.25, 0.5, 0.75),
  quantile_color = "grey25",
  quantile_linewidth = 0.45,
  quantile_linetype = "solid",
  add_jitter = FALSE,
  jitter_width = 0.08,
  jitter_alpha = 0.45,
  jitter_size = 1.3,
  violin_alpha = 0.8,
  violin_color = "#20242A",
  linewidth = 0.35,
  box_width = 0.15,
  box_fill = c("group", "white"),
  box_alpha = 0.9,
  trim = TRUE,
  scale = "width",
  orientation = c("vertical", "horizontal")
)
```

## Arguments

- inner:

  Interior summary structure: `"quartiles"` (default, draws 25%, median,
  and 75% quantile lines), `"boxplot"` (miniature boxplot on each half),
  `"mean_se"` (mean diamond glyph with standard error bar), or `"none"`.

- quantiles:

  Numeric vector of quantile probabilities (default
  `c(0.25, 0.5, 0.75)`).

- quantile_color:

  Color for quantile lines (default `"grey25"`).

- quantile_linewidth:

  Line width for quantile lines (default 0.45).

- quantile_linetype:

  Line type for quantile lines (default `"solid"`).

- add_jitter:

  Add jittered points on each split half (default `FALSE`).

- jitter_width:

  Horizontal jitter width (default 0.08).

- jitter_alpha:

  Point transparency (default 0.45).

- jitter_size:

  Point size (default 1.3).

- violin_alpha:

  Transparency of split violins (default 0.8).

- violin_color:

  Border color of violins (default `"#20242A"`).

- linewidth:

  Outline stroke width (default 0.35).

- box_width:

  Width of inner boxplot when `inner = "boxplot"` (default 0.15).

- box_fill:

  Fill style for inner boxplot: `"group"` (default, filled by group
  color) or `"white"`.

- box_alpha:

  Transparency of inner boxplot (default 0.9).

- trim:

  Trim violin tails to observed data range (default `TRUE`).

- scale:

  Violin scaling method: `"width"` (default, all categories have equal
  maximum width), `"area"`, or `"count"`.

- orientation:

  Plot orientation: `"vertical"` (default) or `"horizontal"`.

## Value

A cliplot_type object for use with
[`cliplot`](https://vanhungtran.github.io/cliomicplot/reference/cliplot.md).

## Examples

``` r
if (FALSE) { # \dontrun{
# Compare supplement effects across doses
cliplot(len ~ dose | supp, data = ToothGrowth,
        type = type_split_violin(),
        title = "Odontoblast Length by Dose & Supplement")

# With inner boxplots highlighted by group
cliplot(len ~ dose | supp, data = ToothGrowth,
        type = type_split_violin(inner = "boxplot"))
} # }
```
