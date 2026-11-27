# MBMA Longitudinal Trajectory Type

Creates publication-ready Model-Based Meta-Analysis (MBMA) trajectory
plots. Displays non-linear longitudinal placebo response kinetics,
confidence/credible interval ribbons, stratified covariate projections
(e.g. baseline disease severity tertiles or concomitant therapy status),
and overlaid clinical trial arm observations sized proportionally to
sample size.

## Usage

``` r
type_mbma_trajectory(
  style = c("longitudinal", "continuous", "divergence"),
  show_points = TRUE,
  point_size_var = NULL,
  point_size_range = c(1.5, 6),
  show_ribbon = TRUE,
  ribbon_alpha = 0.18,
  line_width = 1.1,
  plateau_week = NULL,
  plateau_text = "94% plateau",
  colors = NULL,
  ...
)
```

## Arguments

- style:

  Plot aesthetic style: `"longitudinal"` (time/weeks on x-axis),
  `"continuous"` (baseline covariate on x-axis), or `"divergence"`
  (paired baseline contrast trajectories).

- show_points:

  Logical. Whether to overlay observed trial arm points (default
  `TRUE`).

- point_size_var:

  Column name for point sizing (e.g. sample size `"n"` or
  `"sample_size"`). Default `NULL`.

- point_size_range:

  Numeric vector of length 2 specifying min and max point sizes (default
  `c(1.5, 6.0)`).

- show_ribbon:

  Logical. Whether to draw shaded 95% CI/CrI ribbon (default `TRUE`).

- ribbon_alpha:

  Transparency of confidence ribbon (default `0.18`).

- line_width:

  Linewidth of model trajectory curve (default `1.1`).

- plateau_week:

  Optional numeric. Draws a vertical dotted reference line and callout
  at the specified week where placebo response plateaus (e.g. `12`).

- plateau_text:

  Annotation label for the plateau line (default `"94% plateau"`).

- colors:

  Named character vector or palette name for stratification groups.

- ...:

  Additional arguments.

## Value

A `cliplot_type` object for use with
[`cliplot`](https://vanhungtran.github.io/cliomicplot/reference/cliplot.md).
