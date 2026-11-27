# Multi-Tier Model Benchmark Bar Charts

Creates publication-quality tiered model benchmark bar charts with
grouped-jackknife / bootstrap confidence intervals, print-safe borders,
and permutation null distribution reference bands. Designed for rigorous
multi-omics drug target prioritization and machine learning benchmarks
(multiomics_paper / Liu et al.).

## Usage

``` r
type_benchmark_bars(
  null_band = NULL,
  value_labels = TRUE,
  error_low = NULL,
  error_high = NULL,
  bar_width = 0.75,
  cap_size = 0.25,
  border_color = "black",
  border_width = 0.5,
  label_format = "%.2f",
  text_size = 3,
  group_sep_lines = NULL,
  log_scale = FALSE,
  ...
)

type_benchmark(...)
```

## Arguments

- null_band:

  Numeric vector of length 2 giving the lower and upper bounds of the
  permutation null distribution (e.g., `c(0.505, 0.549)`), drawn as a
  neutral reference band behind the bars.

- value_labels:

  Logical. Whether to print exact performance values above the error
  whiskers (default TRUE).

- error_low:

  Column name or vector for lower confidence interval bounds.

- error_high:

  Column name or vector for upper confidence interval bounds.

- bar_width:

  Bar width (default 0.75).

- cap_size:

  Error bar whisker cap size (default 0.25).

- border_color:

  Bar border color (default `"black"`).

- border_width:

  Bar border line width (default 0.5).

- label_format:

  Character format string for annotations (default "\\.2f").

- text_size:

  Numeric text size for annotations (default 3.0).

- group_sep_lines:

  Vector of x-axis positions or category names where vertical dotted
  boundary lines should be drawn between governance tiers (default
  NULL).

- log_scale:

  Logical. Whether y-axis should be log10 transformed (useful for AP
  lift; default FALSE).

- ...:

  Additional arguments.

## Value

A `cliplot_type` object for use with
[`cliplot`](https://vanhungtran.github.io/cliomicplot/reference/cliplot.md).

## Examples

``` r
if (FALSE) { # \dontrun{
df_bench = data.frame(
  model     = c("Biology", "Druggability", "B+D", "Attention", "Clinical", "Label"),
  auroc     = c(0.77, 0.77, 0.86, 0.91, 0.89, 0.98),
  auroc_lo  = c(0.69, 0.68, 0.78, 0.84, 0.82, 0.94),
  auroc_hi  = c(0.85, 0.84, 0.92, 0.97, 0.96, 1.00),
  learner   = rep("HGB", 6)
)
cliplot(auroc ~ model, data = df_bench,
        type = type_benchmark_bars(null_band = c(0.505, 0.549)))
} # }
```
