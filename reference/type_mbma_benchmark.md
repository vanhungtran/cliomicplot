# MBMA Model-versus-Observed Benchmark Validation Type

Creates benchmark validation plots comparing clinical trial
distributions against Model-Based Meta-Analysis projections (Figure 3C
in Serrano et al. 2025). Overlays trial-level boxplots and
sample-size-scaled observations onto horizontal model prediction bands
(mean line + shaded 95% CI).

## Usage

``` r
type_mbma_benchmark(
  model_mean = NULL,
  model_ci_low = NULL,
  model_ci_high = NULL,
  point_size_var = NULL,
  box_width = 0.45,
  jitter_width = 0.14,
  band_col = "#2C3E50",
  band_fill = "#BDC3C7",
  band_alpha = 0.35,
  ...
)
```

## Arguments

- model_mean:

  Numeric scalar. Expected response rate from the model.

- model_ci_low:

  Numeric scalar. Lower bound of the model 95% CI.

- model_ci_high:

  Numeric scalar. Upper bound of the model 95% CI.

- point_size_var:

  Column name for point sizing (default `"n"`).

- box_width:

  Numeric boxplot width (default `0.45`).

- jitter_width:

  Numeric jitter width (default `0.14`).

- band_col:

  Color of model projected mean line (default `"#2C3E50"`).

- band_fill:

  Fill color of model projected CI band (default `"#BDC3C7"`).

- band_alpha:

  Transparency of model projected CI band (default `0.35`).

- ...:

  Additional arguments.

## Value

A `cliplot_type` object for use with
[`cliplot`](https://vanhungtran.github.io/cliomicplot/reference/cliplot.md).
