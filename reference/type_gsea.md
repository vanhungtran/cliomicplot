# GSEA-Style Enrichment Dot Plot Type

Creates publication-ready gene-set enrichment dot plots (GSEA-style)
from prerank-style result tables. Terms are ranked along the y-axis, the
enrichment score (e.g. NES) on the x-axis, dot size encodes statistical
significance (-log10 FDR) and dot color encodes the direction of
enrichment. Inspired by cnsplots' `gseaplot()`.

## Usage

``` r
type_gsea(
  top_term = 20,
  term_col = NULL,
  nes_col = NULL,
  fdr_col = NULL,
  dot_size_range = c(2, 7),
  nes_colors = c("#2166AC", "#F7F7F7", "#B2182B"),
  show_fdr_legend = TRUE,
  point_alpha = 0.9
)
```

## Arguments

- top_term:

  Maximum number of terms to display (default 20). Terms are ranked by
  absolute enrichment score, then by FDR.

- term_col:

  Optional name of the term/pathway column. Auto-detected.

- nes_col:

  Optional name of the enrichment-score column. Auto-detected from names
  like `NES`, `Enrichment Score`, `ES`.

- fdr_col:

  Optional name of the FDR/q-value column. Auto-detected from names like
  `FDR q-val`, `FDR`, `qvalue`, `padj`, `Adj.P.Value`.

- dot_size_range:

  Range of dot sizes (default `c(2, 7)`).

- nes_colors:

  Diverging colors for negative/zero/positive enrichment (default
  blue-white-red).

- show_fdr_legend:

  Show the size legend for -log10(FDR) (default TRUE).

- point_alpha:

  Transparency of dots (default 0.9).

## Value

A `cliplot_type` object for use with
[`cliplot`](https://vanhungtran.github.io/cliomicplot/reference/cliplot.md).

## Details

Expects a data frame with one row per pathway/term and at least an
enrichment-score column and an FDR/q-value column. Use the formula
interface `cliplot(NES ~ Term, data = gsea_res, type = "gsea")` or pass
the data frame directly. Column names are auto-detected, so results from
common tools (fgsea, clusterProfiler, GSEA desktop) work out of the box.

## Examples

``` r
if (FALSE) { # \dontrun{
# Typical prerank-style GSEA results
cliplot(gsea_res, type = "gsea", theme = "nature")

# Formula interface
cliplot(NES ~ Clean_Term, data = gsea_res, type = "gsea")
} # }
```
