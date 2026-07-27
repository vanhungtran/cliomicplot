# DEG Comparison Scatter Type

Compares differential expression results from two independent
comparisons by plotting their log2 fold-changes against each other.
Inspired by xOmicsShiny's DEG comparison feature.

## Usage

``` r
type_deg_compare(
  comp2_data = NULL,
  merge_by = NULL,
  add_lm = TRUE,
  lm_color = "#3C5488",
  lm_se = TRUE,
  sig_color = "#E64B35",
  ns_color = "grey60",
  point_size = 2,
  point_alpha = 0.6,
  label_genes = NULL,
  same_scale = TRUE
)
```

## Arguments

- comp2_data:

  Data frame containing the second comparison results. Must have the
  same gene identifiers as the main data.

- merge_by:

  Column name to merge the two comparisons by (default auto-detected
  from row names or "Gene" column).

- add_lm:

  Add linear regression line (default TRUE)

- lm_color:

  Colour for the regression line (default "#3C5488")

- lm_se:

  Add confidence band around regression line (default TRUE)

- sig_color:

  Colour for genes significant in both comparisons

- ns_color:

  Colour for non-significant genes

- point_size:

  Point size (default 2)

- point_alpha:

  Point transparency (default 0.6)

- label_genes:

  Character vector of gene names to label, or NULL

- same_scale:

  Force same x and y axis limits (default TRUE)

## Value

A `cliplot_type` object for use with
[`cliplot`](https://vanhungtran.github.io/cliomicplot/reference/cliplot.md).

## Details

This plot compares fold-change direction and magnitude between two
experimental contrasts. It helps identify: - Consistently regulated
genes (both up/up or down/down) - Discordantly regulated genes (up in
one, down in the other) - Comparison-specific effects

## Examples

``` r
if (FALSE) { # \dontrun{
# Compare two drug treatments vs control
cliplot(logFC ~ comp1_logFC, data = merged_de,
        type = type_deg_compare(comp2_data = de_results_2))
} # }
```
