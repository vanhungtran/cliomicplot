# Venn Diagram Type

Creates publication-ready Venn diagrams for 2-3 sets, showing unique and
shared element counts. Built entirely with ggplot2 so the result
integrates with the journal themes and palettes of cliomicplot. Inspired
by cnsplots' `vennplot()`.

## Usage

``` r
type_venn(
  sets = NULL,
  colors = NULL,
  fill_alpha = 0.42,
  linewidth = 0.8,
  label_size = 3.4,
  set_label_size = 3.8,
  show_counts = TRUE
)
```

## Arguments

- sets:

  Optional named list of 1-3 sets. When `NULL`, sets are resolved from
  the data (a named list passed as `x`).

- colors:

  Optional vector of colors for the sets. Defaults to the active
  cliomicplot palette.

- fill_alpha:

  Fill transparency of the circles (default 0.42).

- linewidth:

  Outline width of the circles (default 0.8).

- label_size:

  Size of the region-count labels (default 3.4).

- set_label_size:

  Size of the set-name labels (default 3.8).

- show_counts:

  Show region counts (default TRUE).

## Value

A `cliplot_type` object for use with
[`cliplot`](https://vanhungtran.github.io/cliomicplot/reference/cliplot.md).

## Details

Supports 1-3 sets. For more than 3 sets use
[`type_upset`](https://vanhungtran.github.io/cliomicplot/reference/type_upset.md).

## Examples

``` r
if (FALSE) { # \dontrun{
sets = list(Up = c("TP53", "BRCA1", "MYC"),
            Down = c("TP53", "KRAS", "APC"),
            Mut = c("TP53", "EGFR", "PTEN"))
cliplot(sets, type = "venn", theme = "nature")
} # }
```
