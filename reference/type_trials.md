# Clinical Trials Pipeline Stacked Bar Chart

Creates a horizontal stacked bar chart showing the pipeline of clinical
trials, projects, or any portfolio broken down by category segments
within each row. Segments are color-coded and labelled; total counts
appear on the right.

## Usage

``` r
type_trials(
  segment_colors = NULL,
  segment_text_colors = NULL,
  bar_height = 0.72,
  segment_border = "#EEE9D4",
  segment_border_width = 0.5,
  label_threshold = 3,
  show_totals = TRUE,
  total_nudge = 0.15
)
```

## Arguments

- segment_colors:

  Named character vector mapping segment labels to fill colours.
  Defaults to the active qualitative palette.

- segment_text_colors:

  Named character vector mapping segment labels to label colours for
  text drawn inside segments. Defaults to dark-on-light heuristics.

- bar_height:

  Height of each bar as a fraction of the row spacing (default 0.72).

- segment_border:

  Colour for the border drawn around each segment rectangle (default
  `"#EEE9D4"`).

- segment_border_width:

  Line width for segment borders (default 0.5).

- label_threshold:

  Minimum segment width for which an internal label is drawn (default
  3).

- show_totals:

  Logical. Whether to show a total-count label to the right of each row
  (default `TRUE`).

- total_nudge:

  Fraction of the maximum total used as padding for the x-axis limit
  when totals are shown (default 0.15).

## Value

A `cliplot_type` object for use with
[`cliplot`](https://vanhungtran.github.io/cliomicplot/reference/cliplot.md).

## Formula convention

Call as
`cliplot(value ~ category, data = df, by = segment, type = "trials")` —
the value variable lands on the stacked x-axis, categories form the
rows, and the `by` variable defines the colour segments within each bar.

## Examples

``` r
if (FALSE) { # \dontrun{
trials <- data.frame(
  company = rep(c("AstraZeneca", "Roche", "Pfizer"), each = 5),
  phase   = rep(c("I", "I/II", "II", "II/III", "III"), 3),
  n       = c(4, 3, 24, 0, 32, 10, 8, 21, 0, 19, 9, 2, 10, 0, 6)
)

cliplot(n ~ company, data = trials, by = phase, type = "trials",
        title = "Clinical Trial Pipeline by Phase")
} # }
```
