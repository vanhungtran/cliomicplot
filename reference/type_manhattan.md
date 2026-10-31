# Manhattan Plot for Genome-Wide and Microbial Association Studies

Creates publication-ready Manhattan plots for host GWAS and microbial
genome-wide association studies (mGWAS). Supports multi-chromosome
genomic layouts with alternating chromosome hues and centered axis
labels, as well as single-contig microbial chromosomes. Includes
customizable significance threshold lines, hit highlighting, and
automatic gene/variant labeling.

## Usage

``` r
type_manhattan(
  threshold = 0.05,
  is_already_log10 = FALSE,
  point_size = 1.2,
  point_alpha = 0.85,
  chr_colors = c("#2B5C8F", "#82B0D2"),
  highlight_color = "#E64B35",
  threshold_color = "#888888",
  threshold_linetype = "dashed",
  label_hits = TRUE,
  label_col = NULL,
  max_overlaps = 25,
  xlab = NULL,
  ylab = NULL
)
```

## Arguments

- threshold:

  Significance cutoff. For raw p-values or q-values, defaults to `0.05`
  (or `5e-8` for genome-wide significance). For data already on the
  -log10 scale, specifies the vertical line position.

- is_already_log10:

  Logical. If TRUE, the y-axis values are assumed to already be
  \\-\log\_{10}(p)\\ or \\-\log\_{10}(q)\\. Default FALSE.

- point_size:

  Size of variant points (default 1.2).

- point_alpha:

  Opacity of variant points (default 0.85).

- chr_colors:

  Character vector of two alternating colors for chromosomes (default
  `c("#2B5C8F", "#82B0D2")`).

- highlight_color:

  Color for variants exceeding the significance threshold (default
  `"#E64B35"`).

- threshold_color:

  Color for the horizontal significance line (default `"#888888"`).

- threshold_linetype:

  Line type for the threshold line (default `"dashed"`).

- label_hits:

  Logical. If TRUE, annotates significant variants exceeding the
  threshold with text labels. Default TRUE.

- label_col:

  Column name or character vector containing variant or gene labels. If
  NULL, auto-detects columns named "label", "gene", "snp", or "hit".

- max_overlaps:

  Maximum overlapping labels allowed by ggrepel (default 25).

- xlab:

  Label for x-axis. Defaults to "Chromosome" for multi-chromosome or
  "Chromosome position (bp)" for single-contig genomes.

- ylab:

  Label for y-axis (default `expression(-log[10]~italic(p))`).

## Value

A `cliplot_type` object for use with
[`cliplot`](https://vanhungtran.github.io/cliomicplot/reference/cliplot.md).

## Examples

``` r
if (FALSE) { # \dontrun{
# Simulated microbial GWAS data
set.seed(42)
mgwas_df <- data.frame(
  pos = seq(1000, 500000, by = 5000),
  qvalue = c(runif(95, 0.05, 1.0), runif(5, 1e-5, 0.01)),
  gene = c(rep("", 95), paste0("mut_", 1:5))
)

cliplot(qvalue ~ pos, data = mgwas_df,
        type = type_manhattan(threshold = 0.05, label_col = "gene"))
} # }
```
