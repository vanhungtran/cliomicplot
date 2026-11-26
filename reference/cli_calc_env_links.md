# Automated Calculation of Empirical Omics-to-Environment Links

Calculates empirical statistical linkages connecting omics features or
network modules with external clinical or environmental indicators. The
resulting data frame directly feeds into
[`cliplot_env_network`](https://vanhungtran.github.io/cliomicplot/reference/cliplot_env_network.md)
via its `links` argument. Computes module eigengenes (or centroid
abundance profiles), runs bivariate correlation tests (Spearman or
Pearson) or Mantel dissimilarity associations, applies
Benjamini-Hochberg False Discovery Rate (FDR) adjustments, and selects
statistically significant or top-ranked linkages.

## Usage

``` r
cli_calc_env_links(
  comm_data,
  env_data,
  modules = NULL,
  method = c("spearman", "pearson"),
  fdr_cutoff = 0.05,
  top_n = 5,
  min_abs_r = 0.2,
  module_summary = c("eigengene", "mean")
)
```

## Arguments

- comm_data:

  Numeric matrix or data frame of omics/microbiome abundance (samples in
  rows, features in columns).

- env_data:

  Numeric matrix, data frame, or named list of environmental/clinical
  indicators (samples in rows, indicators in columns). If a named list
  of 1 to 4 matrices/data frames is supplied, each domain is processed
  with its domain tag.

- modules:

  Optional named character or factor vector mapping feature IDs to
  module names (e.g. `c("Taxon_1" = "Mod_1", "Taxon_2" = "Mod_1")`). If
  NULL, features are grouped into modules automatically using
  hierarchical clustering, or individual features are tested directly.

- method:

  Correlation metric: `"spearman"` (default, robust to outliers) or
  `"pearson"`.

- fdr_cutoff:

  Numeric significance threshold for Benjamini-Hochberg adjusted
  p-values (default 0.05). If NULL or if no links meet the threshold,
  the top `top_n` links are retained.

- top_n:

  Optional integer specifying the maximum number of links to retain per
  module or environmental domain (default 5).

- min_abs_r:

  Numeric minimum absolute correlation coefficient \\\|r\|\\ required to
  retain a link (default 0.25).

- module_summary:

  Summary method for features within a module: `"eigengene"` (first
  principal component, default) or `"mean"` (standardized mean
  abundance).

## Value

A data frame containing formatted link linkages with columns:

- env:

  Name of the environmental or clinical indicator variable

- module:

  Name of the target network module

- r:

  Bivariate correlation coefficient

- p:

  Raw asymptotic two-sided p-value

- fdr:

  Benjamini-Hochberg adjusted p-value (q-value)

- log10p:

  Negative log10 p-value (\\-\log\_{10}(P)\\)

- sign:

  Direction of association: `"Positive"` or `"Negative"`

- domain:

  Domain name if `env_data` was supplied as a named list

## Examples

``` r
set.seed(42)
comm <- matrix(rpois(50 * 30, lambda = 15), nrow = 50, ncol = 30)
colnames(comm) <- paste0("Taxon_", 1:30)
env <- data.frame(
  pH = rnorm(50, 6.5, 0.8),
  Moisture = rnorm(50, 45, 10),
  Temp = rnorm(50, 22, 4)
)
mods <- setNames(paste0("Mod_", rep(1:3, each = 10)), colnames(comm))
links <- cli_calc_env_links(comm, env, modules = mods, method = "spearman")
head(links)
#>    env module          r          p          domain       fdr   log10p     sign
#> 1 Temp  Mod_3 -0.2697239 0.05847339 External_Domain 0.5262605 1.233042 Negative
```
