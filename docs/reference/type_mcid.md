# Minimal Clinically Important Difference (MCID) Dumbbell Plot Type

Creates publication-ready MCID dumbbell plots (Clinical Archetype 1) to
distinguish genuine clinical relevance from mere statistical
significance. Connects observed causal or clinical effect sizes to
validated instrument MCID thresholds (e.g., DLQI = 4.0, EASI = 6.6,
SCORAD = 8.7), scaling point glyphs by sample size (N treated) and
ordering endpoints by effect-to-MCID ratio.

## Usage

``` r
type_mcid(
  mcid = NULL,
  estimate = NULL,
  n_treated = NULL,
  category = NULL,
  sort_by_ratio = TRUE,
  segment_width = 1.2,
  mcid_tick_size = 3,
  point_size_range = c(3, 7.5),
  palette = NULL,
  xlab = "Absolute Clinical Estimate (|estimate|)",
  ylab = NULL
)
```

## Arguments

- mcid:

  Numeric threshold or column name containing MCID reference values.

- estimate:

  Column name or vector of observed effect estimates.

- n_treated:

  Optional column name or vector of sample sizes to scale point sizes.

- category:

  Optional column name for clinical status classification (e.g., "SoC
  exceeding MCID", "SoC below MCID", "Concomitant exceeding MCID").

- sort_by_ratio:

  Sort endpoints by effect-to-MCID ratio (default TRUE).

- segment_width:

  Dumbbell connecting line width (default 1.2).

- mcid_tick_size:

  Size of MCID reference vertical tick (default 3.0).

- point_size_range:

  Range for sample-size-scaled points (default `c(3, 7.5)`).

- palette:

  Custom color palette for categories.

- xlab:

  Label for x-axis (default "Absolute Effect Estimate (\|estimate\|)").

- ylab:

  Label for y-axis (default NULL).

## Value

A cliplot_type object for use with
[`cliplot`](https://vanhungtran.github.io/cliomicplot/reference/cliplot.md).

## Examples

``` r
if (FALSE) { # \dontrun{
mcid_df = data.frame(
  Endpoint = c("EASI (Dupilumab)", "DLQI (Dupilumab)", "SCORAD (TCS)", "DLQI (Antihistamines)"),
  Estimate = c(9.2, 5.8, 4.1, 1.2),
  MCID     = c(6.6, 4.0, 8.7, 4.0),
  N        = c(420, 395, 210, 150),
  Category = c("SoC Exceeding", "SoC Exceeding", "SoC Below", "Concomitant Below")
)

cliplot(Estimate ~ Endpoint, data = mcid_df, type = type_mcid(mcid = "MCID", n_treated = "N"))
} # }
```
