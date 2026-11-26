# Ecological Network Edge-Sign Dynamics Plot Type

Visualizes the balance between ecological cooperation (positive
co-occurrence links) and competitive exclusion (negative correlation
links) across experimental groups, clinical disease states, or
spatiotemporal gradients (Wen et al. iMeta 2025).

## Usage

``` r
type_edge_dynamics(
  mode = c("stack", "ratio", "counts"),
  pos_color = "#4DBBD5",
  neg_color = "#E64B35",
  bar_width = 0.65,
  show_labels = TRUE,
  label_size = 3,
  threshold_ratio = NULL,
  edge_data = NULL
)
```

## Arguments

- mode:

  Character string specifying representation: `"stack"` (stacked
  proportion 100% bar chart), `"ratio"` (negative-to-positive ratio
  trendline/points), or `"counts"` (grouped absolute edge count bars).
  Default is `"stack"`.

- pos_color:

  Color for positive co-occurrence edges (default `"#4DBBD5"`, cyan
  blue).

- neg_color:

  Color for negative co-occurrence edges (default `"#E64B35"`, coral
  red).

- bar_width:

  Numeric width of bars (default 0.65).

- show_labels:

  Logical. If TRUE (default), displays percentage text inside stacked
  bars.

- label_size:

  Numeric font size for percentage labels (default 3.0).

- threshold_ratio:

  Numeric reference ratio line (default 1.0 or NULL).

- edge_data:

  Optional data frame with columns `group`, `sign` (or `cor`/`weight`),
  and optional `count`.

## Value

A `cliplot_type` object for use with
[`cliplot`](https://vanhungtran.github.io/cliomicplot/reference/cliplot.md).

## Examples

``` r
if (FALSE) { # \dontrun{
cliplot(type = type_edge_dynamics(mode = "stack"))
} # }
```
