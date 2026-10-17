# Slope Plot Type

Creates publication-ready slope plots (slopegraphs) showing how paired
values change between two or more conditions. Ideal for before/after
treatment comparisons and paired time-course data. Inspired by cnsplots'
`slopeplot()`.

## Usage

``` r
type_slope(
  pair = NULL,
  color_by = NULL,
  linewidth = 0.65,
  point_size = 2.6,
  line_alpha = 0.65,
  point_alpha = 0.9,
  reorder_x = TRUE
)
```

## Arguments

- pair:

  Column name (or vector) identifying paired observations. Lines connect
  the same subject across conditions.

- color_by:

  Optional column name used to color the paired lines (e.g. responder
  status). Defaults to the `by` grouping variable.

- linewidth:

  Width of the connecting lines (default 0.65).

- point_size:

  Size of the points (default 2.6).

- line_alpha:

  Transparency of the lines (default 0.65).

- point_alpha:

  Transparency of the points (default 0.9).

- reorder_x:

  Reorder the x-axis categories in order of appearance (default TRUE).

## Value

A `cliplot_type` object for use with
[`cliplot`](https://vanhungtran.github.io/cliomicplot/reference/cliplot.md).

## Details

Use the formula interface
`cliplot(value ~ condition, data, type = "slope")` with
`type_slope(pair = "subject")` to draw one line per paired subject
across conditions. The optional `color_by` column highlights groups
(e.g. responders vs non-responders).

## Examples

``` r
if (FALSE) { # \dontrun{
cliplot(value ~ timepoint, data = paired_df,
        type = type_slope(pair = "patient", color_by = "responder"))
} # }
```
