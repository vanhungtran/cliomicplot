# Volcano Plot Type

Creates publication-ready volcano plots for visualizing differential
expression or other omics results. Points are colored by significance
and log2 fold-change thresholds.

## Usage

``` r
type_volcano(
  pval_cutoff = 0.05,
  fc_cutoff = 1,
  point_size = 2.2,
  point_alpha = 0.72,
  label_genes = NULL,
  max_overlaps = 15,
  up_color = "#D73027",
  down_color = "#2B6CB0",
  ns_color = "#A8B0BA",
  cutoff_region_alpha = 0.045,
  show_threshold_labels = TRUE
)
```

## Arguments

- pval_cutoff:

  P-value cutoff for significance (default 0.05)

- fc_cutoff:

  Absolute log2 fold-change cutoff (default 1, i.e. 2-fold)

- point_size:

  Point size (default 2.2)

- point_alpha:

  Point transparency (default 0.72)

- label_genes:

  Character vector of gene names to label, or "significant" to label all
  significant genes, or NULL for no labels.

- max_overlaps:

  Maximum overlapping labels for ggrepel (default 15)

- up_color:

  Color for upregulated points (default "red")

- down_color:

  Color for downregulated points (default "blue")

- ns_color:

  Color for non-significant points (default "grey70")

- cutoff_region_alpha:

  Alpha for lightly shaded significant regions.

- show_threshold_labels:

  Add text labels for cutoff lines.

## Value

A cliplot_type object for use with
[`cliplot`](https://vanhungtran.github.io/cliomicplot/reference/cliplot.md).

## Details

Formula orientation matters. cliplot uses the standard `y ~ x`
convention, and a volcano plot puts log2 fold-change on the x-axis and
significance on the y-axis. Supply the formula as
`-log10(PValue) ~ logFC` (i.e. `y ~ x` with the significance term on the
left). Row names of `data` are used as gene labels. Use an FDR-adjusted
p-value column (e.g. `padj`) for the y term when available, so the
significance threshold reflects multiple-testing control.

## Examples

``` r
if (FALSE) { # \dontrun{
# Basic volcano plot (y ~ x: significance ~ fold-change)
cliplot(-log10(padj) ~ logFC, data = deg_results, type = "volcano")

# With gene labels
cliplot(-log10(padj) ~ logFC, data = deg_results,
        type = type_volcano(label_genes = "significant"))

# Custom cutoffs
cliplot(-log10(padj) ~ logFC, data = deg_results,
        type = type_volcano(pval_cutoff = 0.01, fc_cutoff = 1.5))
} # }
```
