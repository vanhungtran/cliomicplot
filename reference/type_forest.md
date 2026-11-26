# Forest Plot Type

Creates publication-ready forest plots for displaying hazard ratios,
odds ratios, and confidence intervals from survival analysis, logistic
regression, observational causal models, or meta-analysis. Enhanced with
journal reporting standards: alternating row shading, summary pooled
diamonds, unclipped numeric annotations, and zero-magnitude sign
suppression.

## Usage

``` r
type_forest(
  estimate = NULL,
  ci_low = NULL,
  ci_high = NULL,
  variable = NULL,
  ref_line = TRUE,
  ref_val = 1,
  log_scale = TRUE,
  sort = TRUE,
  stripe = TRUE,
  show_values = FALSE,
  diamond = FALSE,
  point_size = 3,
  ci_width = 0.9,
  sig_color = "#E64B35",
  ns_color = "#3C5488"
)
```

## Arguments

- estimate:

  Column name or vector of point estimates (HR, OR, etc.).

- ci_low:

  Column name or vector of lower CI bounds.

- ci_high:

  Column name or vector of upper CI bounds.

- variable:

  Column name or vector of variable labels.

- ref_line:

  Show reference line (default TRUE).

- ref_val:

  Value for reference line (default 1 for HR/OR; set 0 for beta).

- log_scale:

  Use log10 scale for estimates (default TRUE for HR/OR).

- sort:

  Sort by estimate magnitude (default TRUE).

- stripe:

  Add subtle alternating background row shading to prevent eye drift
  across wide canvases (default TRUE; Archetype 9).

- show_values:

  Annotate exact numeric estimates and 95% CIs on the plot (default
  FALSE).

- diamond:

  Highlight summary / meta-analysis pooled rows as diamonds (default
  FALSE, or auto-detected from `is_summary` column).

- point_size:

  Point size for estimates (default 3).

- ci_width:

  CI line width (default 0.9).

- sig_color:

  Color for significant results (default "#E64B35").

- ns_color:

  Color for non-significant results (default "#3C5488").

## Value

A cliplot_type object for use with
[`cliplot`](https://vanhungtran.github.io/cliomicplot/reference/cliplot.md).

## Examples

``` r
if (FALSE) { # \dontrun{
forest_data = data.frame(
  Variable = c("Age >60", "Male", "Stage III", "Stage IV", "Mutation+"),
  HR = c(1.8, 1.3, 2.5, 4.2, 1.9),
  CI_low = c(1.2, 0.9, 1.6, 2.8, 1.3),
  CI_high = c(2.7, 1.9, 3.9, 6.3, 2.8),
  P = c(0.001, 0.15, 0.0005, 0.0001, 0.003)
)

cliplot(HR ~ Variable, data = forest_data, type = "forest")
} # }
```
