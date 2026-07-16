# Forest Plot Type

Creates publication-ready forest plots for displaying hazard ratios,
odds ratios, and confidence intervals from survival analysis, logistic
regression, or meta-analysis.

## Usage

``` r
type_forest(
  estimate = NULL,
  ci_low = NULL,
  ci_high = NULL,
  variable = NULL,
  ref_line = TRUE,
  sort = TRUE,
  point_size = 3,
  ci_width = 1,
  sig_color = "#E64B35",
  ns_color = "#3C5488"
)
```

## Arguments

- estimate:

  Column name or vector of point estimates (HR, OR, etc.)

- ci_low:

  Column name or vector of lower CI bounds

- ci_high:

  Column name or vector of upper CI bounds

- variable:

  Column name or vector of variable labels

- ref_line:

  Reference line at x = 1 (default TRUE)

- sort:

  Sort by estimate (default TRUE)

- point_size:

  Point size for estimates (default 3)

- ci_width:

  CI line width (default 1)

- sig_color:

  Color for significant results

- ns_color:

  Color for non-significant results

## Value

A cliplot_type object for use with
[`cliplot`](https://vanhungtran.github.io/cliomicplot/reference/cliplot.md).

## Examples

``` r
if (FALSE) { # \dontrun{
# From a data frame with columns: Variable, HR, CI_low, CI_high, P
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
