# UpSet Plot Type

Creates publication-ready UpSet plots for visualizing set intersections.
An alternative to Venn diagrams that scales to many sets: the top panel
shows intersection sizes as bars, the bottom panel shows a membership
dot matrix, and (optionally) the left panel shows set sizes. Inspired by
cnsplots' `upsetplot()`.

## Usage

``` r
type_upset(
  sets = NULL,
  sort_by = c("size", "degree"),
  max_rank = NULL,
  min_size = 0,
  bar_color = "#3C5488",
  point_color = "#3C5488",
  set_color = "#E64B35",
  show_set_sizes = TRUE,
  show_size_labels = TRUE,
  bar_width = 0.68
)
```

## Arguments

- sets:

  Optional named list of sets (vectors of elements). When `NULL`, sets
  are resolved from the data: a named list passed as `x`, or a data
  frame of binary membership columns.

- sort_by:

  Sort intersections by `"size"` (default) or `"degree"` (number of sets
  in the intersection).

- max_rank:

  Maximum number of intersections to show (default NULL = all observed
  intersections).

- min_size:

  Only show intersections with at least this many elements (default 0).

- bar_color:

  Color of the intersection-size bars (default `"#3C5488"`).

- point_color:

  Color of the membership dots (default `"#3C5488"`).

- set_color:

  Color of the set-size bars (default `"#E64B35"`).

- show_set_sizes:

  Show the set-size panel on the left (default TRUE).

- show_size_labels:

  Label intersection sizes above bars (default TRUE).

- bar_width:

  Width of the intersection bars (default 0.68).

## Value

A `cliplot_type` object for use with
[`cliplot`](https://vanhungtran.github.io/cliomicplot/reference/cliplot.md).
The composed figure requires patchwork; when patchwork is not installed
a single-panel bar chart of intersection sizes is returned.

## Details

Pass a named list of sets as the first argument, or a data frame of
binary membership columns (rows = items, columns = sets). Only
intersections that actually occur in the data are shown, matching UpSetR
behaviour.

## Examples

``` r
if (FALSE) { # \dontrun{
# Named list of sets
sets = list(Treatment_A = c("TP53", "BRCA1", "MYC"),
            Treatment_B = c("TP53", "KRAS", "APC"),
            Control     = c("TP53", "MYC", "RB1"))
cliplot(sets, type = "upset")

# Binary membership data frame
cliplot(deg_binary, type = "upset", theme = "nature")
} # }
```
