# MA Plot Type

Creates publication-ready MA (Mean-Average) plots for visualizing
differential expression results. Shows log2 fold-change vs. mean
expression with significance highlighting.

## Usage

``` r
type_ma(
  pval_cutoff = 0.05,
  point_size = 1.8,
  point_alpha = 0.65,
  sig_color = "#D73027",
  down_color = "#2B6CB0",
  ns_color = "#A8B0BA",
  add_loess = TRUE,
  loess_color = "#263238"
)
```

## Arguments

- pval_cutoff:

  P-value cutoff for coloring significant genes (default 0.05)

- point_size:

  Point size (default 1.8)

- point_alpha:

  Point transparency (default 0.65)

- sig_color:

  Color for significant points (default "#E64B35")

- down_color:

  Color for significant negative fold changes.

- ns_color:

  Color for non-significant points (default "grey60")

- add_loess:

  Add LOESS smoothing line (default TRUE)

- loess_color:

  Color for LOESS line (default "#3C5488")

## Value

A cliplot_type object for use with
[`cliplot`](https://vanhungtran.github.io/cliomicplot/reference/cliplot.md).

## Examples

``` r
if (FALSE) { # \dontrun{
# From DESeq2 results
cliplot(log2FoldChange ~ baseMean, data = deseq_res, type = "ma")
} # }
```
