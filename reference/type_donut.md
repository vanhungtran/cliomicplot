# Donut Chart Type

Creates publication-ready donut (ring) charts for visualizing
proportions. A pie chart with a central hole that can carry a summary
label. Inspired by cnsplots' `donutplot()`.

## Usage

``` r
type_donut(
  show_percent = TRUE,
  label_size = 3.5,
  center_label = NULL,
  center_value = NULL,
  border_color = "white",
  border_width = 1,
  hole_lo = 0,
  hole_hi = 2.6
)
```

## Arguments

- show_percent:

  Label each slice with its percentage (default TRUE).

- label_size:

  Size of the percentage labels (default 3.5).

- center_label:

  Optional text drawn in the center hole (default NULL).

- center_value:

  Optional bold value drawn under the center label (default NULL).

- border_color:

  Color of the slice borders (default "white").

- border_width:

  Width of the slice borders (default 1).

- hole_lo:

  Lower bound of the radial scale (default 0). The visual hole spans
  from `hole_lo` to the inner edge of the ring.

- hole_hi:

  Upper bound of the ring (default 2.6).

## Value

A `cliplot_type` object for use with
[`cliplot`](https://vanhungtran.github.io/cliomicplot/reference/cliplot.md).

## Details

Pass a categorical variable (`cliplot(~ group, data, type = "donut")`)
to count proportions, or a data frame with a category column and an
optional value column.

## Examples

``` r
if (FALSE) { # \dontrun{
cliplot(~ Species, data = iris, type = "donut")
cliplot(category ~ value, data = composition_df, type = "donut")
} # }
```
