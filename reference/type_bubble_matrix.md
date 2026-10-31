# Differential Abundance Bubble Matrix Dot Plot

Creates publication-ready bubble matrix dot plots for differential
abundance, gene expression, or biomarker screening across clinical
subgroups, anatomical sites, or study visits. Encodes statistical
significance (e.g. \\-\log\_{10}(q)\\) as point size and directional
effect size (e.g. \\\log_2(\text{FC})\\) as a diverging color gradient.

## Usage

``` r
type_bubble_matrix(
  size_col = NULL,
  fill_col = NULL,
  min_size = 1.5,
  max_size = 6.5,
  midpoint = 0,
  colors = c("#4575B4", "grey92", "#D73027"),
  limits = c(-2, 2),
  size_name = expression(-log[10] ~ italic(q)),
  fill_name = expression(log[2] ~ "Fold Change"),
  reorder_by = NULL,
  grid_color = "#EBEBEB",
  xlab = NULL,
  ylab = NULL
)
```

## Arguments

- size_col:

  Column name for significance or point size (e.g. "padj", "qvalue", or
  "-log10q").

- fill_col:

  Column name for effect size or direction (e.g. "log2FC" or
  "estimate").

- min_size:

  Minimum point radius in mm (default 1.5).

- max_size:

  Maximum point radius in mm (default 6.5).

- midpoint:

  Value representing no directional change (default 0).

- colors:

  Diverging color gradient vector of length 3 (low, mid, high). Default
  `c("#4575B4", "grey92", "#D73027")`.

- limits:

  Numeric vector of length 2 specifying color scale limits. Default
  `c(-2, 2)`.

- size_name:

  Legend title for point size (default
  `expression(-log[10]~italic(q))`).

- fill_name:

  Legend title for color gradient (default
  `expression(log[2]~"Fold Change")`).

- reorder_by:

  Optional column or vector to reorder features along the y-axis.

- grid_color:

  Color for background grid lines (default `"#EBEBEB"`).

- xlab, ylab:

  Axis labels.

## Value

A `cliplot_type` object for use with
[`cliplot`](https://vanhungtran.github.io/cliomicplot/reference/cliplot.md).

## Examples

``` r
if (FALSE) { # \dontrun{
# Simulated differential abundance results
set.seed(42)
da_df <- expand.grid(
  Species = c("S. epidermidis", "S. aureus", "C. acnes", "M. restricta", "P. melaninogenica"),
  Comparison = c("Forehead (T1)", "Cheek (T1)", "Antecubital (T1)", "Antecubital (T3)")
)
da_df$log2FC <- rnorm(nrow(da_df), mean = 0, sd = 1.2)
da_df$qval <- runif(nrow(da_df), 0.001, 0.2)

cliplot(Species ~ Comparison, data = da_df,
        type = type_bubble_matrix(size_col = "qval", fill_col = "log2FC"))
} # }
```
