# Kaplan-Meier Survival Curve & Cumulative Incidence Type

Creates publication-ready Kaplan-Meier survival curves or cumulative
incidence plots with optional risk tables, landmark milestone callouts,
confidence intervals, median survival lines, and log-rank p-values.
Driven by a native ggplot2 + survival engine with zero external
dependencies, and compatible with survminer when requested.

## Usage

``` r
type_km(
  time = NULL,
  event = NULL,
  risk_table = TRUE,
  pval = TRUE,
  conf_int = FALSE,
  median_line = FALSE,
  cumulative_incidence = FALSE,
  milestones = NULL,
  censor = TRUE,
  censor_size = 1.8,
  palette = NULL,
  linewidth = 0.8,
  xlab = "Time",
  ylab = NULL,
  engine = c("auto", "native", "survminer")
)
```

## Arguments

- time:

  Column name or vector of survival times.

- event:

  Column name or vector of event indicators (1 = event, 0 = censored).

- risk_table:

  Show risk table below the plot (default TRUE).

- pval:

  Show log-rank p-value on the plot (default TRUE).

- conf_int:

  Show confidence interval ribbon (default FALSE).

- median_line:

  Show median survival drop lines (default FALSE).

- cumulative_incidence:

  Plot cumulative incidence ((1 - S(t)) \* 100 percent) instead of
  survival probability (default FALSE; Archetype 5).

- milestones:

  Numeric vector of landmark milestone times (e.g., `c(6, 12, 24, 60)`)
  where exact percentages and point markers are annotated directly on
  the curve (Archetype 5).

- censor:

  Show tick markers at censored observations (default TRUE).

- censor_size:

  Size of censoring cross glyphs (default 1.8).

- palette:

  Color palette for groups (default "jama").

- linewidth:

  Line width for survival step curves (default 0.8).

- xlab:

  Label for x-axis (default "Time").

- ylab:

  Label for y-axis (default "Survival Probability" or "Cumulative
  Incidence (%)").

- engine:

  Engine to use: `"auto"` (default, uses native engine with survminer
  fallback if requested), `"native"`, or `"survminer"`.

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

# Landmark milestone callouts at 180, 365, 730 days with CI
cliplot(Surv(time, status) ~ sex, data = lung,
        type = type_km(conf_int = TRUE, milestones = c(180, 365, 730)))

# Cumulative incidence mode
cliplot(Surv(time, status) ~ sex, data = lung,
        type = type_km(cumulative_incidence = TRUE, milestones = c(180, 365)))
} # }
```
