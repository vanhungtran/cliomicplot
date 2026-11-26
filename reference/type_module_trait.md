# Module-Trait and Environmental Factor Correlation Heatmap

Creates publication-ready correlation heatmaps linking biological
network modules (e.g. WGCNA module eigengenes, microbial cluster
abundances, metabolic modules) with external environmental factors,
clinical phenotypes, or physiological exposures (e.g., pH, temperature,
humidity, pollution, SCORAD, age, TEWL). Computes and displays
correlation coefficients (\\r\\ or \\\rho\\), exact two-sided p-values,
Benjamini-Hochberg FDR adjustments, significance asterisks, and optional
hierarchical clustering for modules and environmental factors.

## Usage

``` r
type_module_trait(
  method = c("pearson", "spearman"),
  cor_col = NULL,
  p_col = NULL,
  show_labels = c("both", "coef_stars", "coef", "stars", "none"),
  sig_digits = 2,
  colors = c("#2166AC", "#F7F7F7", "#B2182B"),
  limits = c(-1, 1),
  cluster_modules = FALSE,
  cluster_env = FALSE,
  p_cutoff = NULL,
  border_color = "#E2E8F0",
  border_size = 0.35,
  text_size = 3,
  text_color = "auto",
  legend_title = NULL,
  xlab = NULL,
  ylab = NULL
)
```

## Arguments

- method:

  Correlation method: `"pearson"` (default) or `"spearman"`.

- cor_col:

  Column name for correlation coefficients in precomputed data (default
  auto-detected: `"cor"`, `"r"`, `"rho"`, `"estimate"`).

- p_col:

  Column name for p-values in precomputed data (default auto-detected:
  `"pvalue"`, `"p"`, `"pval"`, `"padj"`, `"qvalue"`).

- show_labels:

  Cell annotation format: `"both"` (`"0.42\n(p=0.01)"`, default),
  `"coef_stars"` (`"0.42***"`), `"coef"` (`"0.42"`), `"stars"`
  (`"***"`), or `"none"`.

- sig_digits:

  Number of decimal digits for correlation coefficient (default 2).

- colors:

  Diverging color gradient vector of length 3 (low, mid, high). Default
  `c("#2166AC", "#F7F7F7", "#B2182B")` (standard blue-white-red).

- limits:

  Numeric vector of length 2 for color scale limits (default
  `c(-1, 1)`).

- cluster_modules:

  Logical. If TRUE, reorders module rows using hierarchical clustering.
  Default FALSE.

- cluster_env:

  Logical. If TRUE, reorders environmental factor columns using
  hierarchical clustering. Default FALSE.

- p_cutoff:

  Optional significance threshold (e.g. `0.05`). If provided, cells with
  p-values exceeding this cutoff will have blanked text labels.

- border_color:

  Outline color for tiles (default `"#E2E8F0"`).

- border_size:

  Stroke width for tile borders (default `0.35`).

- text_size:

  Font size for cell annotations (default 3.0).

- text_color:

  Text color. If `"auto"` (default), selects high-contrast text color
  based on cell fill.

- legend_title:

  Title for color legend (default `expression(italic(r))`).

- xlab, ylab:

  Axis labels.

## Value

A `cliplot_type` object for use with
[`cliplot`](https://vanhungtran.github.io/cliomicplot/reference/cliplot.md).

## Examples

``` r
if (FALSE) { # \dontrun{
# Precomputed module-trait correlations
cor_df <- expand.grid(
  Module = c("M1_Inflammation", "M2_SkinBarrier", "M3_Microbiome"),
  Factor = c("Skin_pH", "Humidity", "SCORAD")
)
cor_df$cor <- c(0.45, -0.22, 0.65, -0.38, 0.52, -0.15, 0.72, 0.48, -0.31)
cor_df$pvalue <- c(0.001, 0.08, 1e-5, 0.005, 0.0001, 0.25, 1e-7, 0.0002, 0.01)

cliplot(Module ~ Factor, data = cor_df, type = type_module_trait())
} # }
```
