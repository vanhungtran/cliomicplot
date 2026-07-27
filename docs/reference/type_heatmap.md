# Heatmap Type

Creates publication-ready heatmaps for visualizing omics data matrices.
Supports row/column clustering, annotation tracks, and customizable
color scales.

## Usage

``` r
type_heatmap(
  scale = "row",
  cluster_rows = TRUE,
  cluster_cols = TRUE,
  show_rownames = NULL,
  show_colnames = TRUE,
  color_low = "#2166AC",
  color_mid = "#F7F7F7",
  color_high = "#B2182B",
  color_midpoint = 0,
  annotation_col = NULL,
  annotation_row = NULL,
  annotation_colors = NULL,
  border_color = NA,
  fontsize = 10
)
```

## Arguments

- scale:

  Scale rows/columns: "none", "row", "column" (default "row")

- cluster_rows:

  Cluster rows (default TRUE)

- cluster_cols:

  Cluster columns (default TRUE)

- show_rownames:

  Show row names (default FALSE, auto-enabled for \<50 rows)

- show_colnames:

  Show column names (default TRUE)

- color_low:

  Low end of color gradient (default "#2166AC")

- color_mid:

  Midpoint color (default "#F7F7F7")

- color_high:

  High end of color gradient (default "#B2182B")

- color_midpoint:

  Midpoint value (default 0)

- annotation_col:

  Optional data frame of column annotations

- annotation_row:

  Optional data frame of row annotations

- annotation_colors:

  List of colors for annotations

- border_color:

  Border color for cells (default NA, no border)

- fontsize:

  Base font size (default 10)

## Value

A cliplot_type object for use with
[`cliplot`](https://vanhungtran.github.io/cliomicplot/reference/cliplot.md).

## Examples

``` r
if (FALSE) { # \dontrun{
# Basic heatmap from matrix
mat = matrix(rnorm(200), ncol = 10)
rownames(mat) = paste0("Gene", 1:20)
colnames(mat) = paste0("Sample", 1:10)
cliplot(mat, type = "heatmap")

# With annotations
ann_col = data.frame(Group = rep(c("Control","Treatment"), each = 5))
rownames(ann_col) = colnames(mat)
cliplot(mat, type = type_heatmap(annotation_col = ann_col))
} # }
```
