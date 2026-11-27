# Attribution Plot Type (Shapley Values, Stepwise Ablation, and Dual Contrasts)

Creates publication-grade attribution plots inspired by
leakage-controlled machine learning and multi-omics drug target
prioritization benchmarks. Supports three core styles:

- `"shapley"`: Horizontal or vertical bars displaying exact Shapley
  value attributions or feature-level contributions across mechanisms or
  feature tiers, with optional grouping across learners.

- `"stepwise"`: Stepwise mechanism removal / ablation curves tracing
  metric deterioration (e.g. AUROC or Average Precision) with 95%
  confidence intervals and step callouts.

- `"contrast"`: Dual-context attribution contrast comparing marginal
  impact when removed from a fully specified model vs. when added to an
  unaugmented baseline.

## Usage

``` r
type_attribution(
  style = c("shapley", "stepwise", "contrast"),
  reference_line = NULL,
  annotate = TRUE,
  label_format = "%.3f",
  error_low = NULL,
  error_high = NULL,
  color_pos = "#B64342",
  color_neg = "#0F4D92",
  bar_width = 0.65,
  point_size = 3.5,
  text_size = 3,
  learner_colors = c(hgb = "#0F4D92", HGB = "#0F4D92", lr = "#B64342", LR = "#B64342", rf
    = "#42949E", RF = "#42949E"),
  ...
)

type_shapley(...)

type_stepwise(...)
```

## Arguments

- style:

  One of `"shapley"` (default), `"stepwise"`, or `"contrast"`.

- reference_line:

  Numeric. Reference value for baseline (default 0 for Shapley/contrast;
  0.5 for stepwise AUROC).

- annotate:

  Logical. Whether to print numeric values on bars or points (default
  TRUE).

- label_format:

  Character format string for annotations (default "\\.3f").

- error_low:

  Column name or vector for lower error bounds in stepwise/contrast
  plots.

- error_high:

  Column name or vector for upper error bounds in stepwise/contrast
  plots.

- color_pos:

  Color for positive contributions (default `"#B64342"`).

- color_neg:

  Color for negative contributions (default `"#0F4D92"`).

- bar_width:

  Numeric bar width (default 0.65).

- point_size:

  Numeric point size for stepwise/contrast (default 3.5).

- text_size:

  Numeric text size for value annotations (default 3.0).

- learner_colors:

  Named vector of colors for multi-learner contrasts (default: HGB =
  `"#0F4D92"`, LR = `"#B64342"`, RF = `"#42949E"`).

- ...:

  Additional arguments.

## Value

A `cliplot_type` object for use with
[`cliplot`](https://vanhungtran.github.io/cliomicplot/reference/cliplot.md).

## Examples

``` r
if (FALSE) { # \dontrun{
# Shapley values across leakage mechanisms
df_shap = data.frame(
  mechanism = c("Label", "Prior Trials", "Attention", "Zero-fill", "Ungrouped CV"),
  shapley   = c(0.032, 0.020, 0.028, -0.008, 0.003),
  learner   = rep("HGB", 5)
)
cliplot(shapley ~ mechanism, data = df_shap, type = type_attribution(style = "shapley"))
} # }
```
