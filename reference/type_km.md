# Kaplan-Meier Survival Curve Type

Creates publication-ready Kaplan-Meier survival curves with risk tables
and log-rank test p-values. Uses survminer under the hood.

## Usage

``` r
type_km(
  time = NULL,
  event = NULL,
  risk_table = TRUE,
  pval = TRUE,
  conf_int = FALSE,
  median_line = FALSE,
  palette = NULL,
  linewidth = 0.8,
  xlab = "Time",
  ylab = "Survival Probability"
)
```

## Arguments

- time:

  Column name or vector of survival times

- event:

  Column name or vector of event indicators (1 = event, 0 = censored)

- risk_table:

  Show risk table below the plot (default TRUE)

- pval:

  Show log-rank p-value (default TRUE)

- conf_int:

  Show confidence intervals (default FALSE)

- median_line:

  Show median survival line (default FALSE)

- palette:

  Color palette for groups

- linewidth:

  Line width for survival curves (default 0.8)

- xlab:

  Label for x-axis (default "Time")

- ylab:

  Label for y-axis (default "Survival Probability")

## Value

A cliplot_type object for use with
[`cliplot`](https://vanhungtran.github.io/cliomicplot/reference/cliplot.md).

## Examples

``` r
if (FALSE) { # \dontrun{
library(survival)
data(lung)

# Basic KM plot
cliplot(Surv(time, status) ~ sex, data = lung, type = "km")

# With risk table and CI
cliplot(Surv(time, status) ~ sex, data = lung,
        type = type_km(risk_table = TRUE, conf_int = TRUE))
} # }
```
