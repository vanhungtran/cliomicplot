# Permutation Significance Testing for Cross-Network Structural Differences

Evaluates whether global topological structural metrics (edge density,
natural connectivity, negative-to-positive edge ratio, mean degree, and
clustering coefficient) differ significantly between experimental
conditions (e.g. KO vs WT, Case vs Control, or Healthy vs Lesional).
Generates an empirical null distribution by shuffling sample group
labels \\B\\ times, recomputing correlation networks, and calculating
two-sided permutation p-values.

## Usage

``` r
cli_network_permutation(
  comm_data,
  group,
  ref_group = NULL,
  test_group = NULL,
  cor_method = c("spearman", "pearson"),
  cor_cutoff = 0.35,
  p_cutoff = 0.05,
  n_perm = 199,
  seed = NULL
)
```

## Arguments

- comm_data:

  Numeric matrix or data frame of feature abundances (samples in rows,
  features in columns).

- group:

  Factor or character vector specifying condition assignments for each
  sample. Must match `nrow(comm_data)` and contain at least 2 distinct
  groups.

- ref_group:

  Optional character string designating the baseline reference group. If
  NULL, the first level of `group` is used.

- test_group:

  Optional character string designating the comparison group. If NULL,
  the second level of `group` is used.

- cor_method:

  Correlation metric: `"spearman"` (default) or `"pearson"`.

- cor_cutoff:

  Absolute correlation threshold \\\|r\|\\ to establish network edges
  (default 0.35).

- p_cutoff:

  Significance threshold for correlation p-values (default 0.05).

- n_perm:

  Integer. Number of permutation iterations (default 199).

- seed:

  Optional integer seed for reproducibility.

## Value

A data frame summarizing observed metrics, permuted null means, standard
errors, and two-sided permutation p-values:

- metric:

  Topological network metric

- ref_val:

  Observed metric value in the reference condition

- test_val:

  Observed metric value in the test condition

- delta_obs:

  Observed difference (`test_val - ref_val`)

- perm_mean:

  Mean of the permuted null differences

- perm_se:

  Standard error of the permuted null differences

- p_perm:

  Two-sided empirical permutation p-value

## Examples

``` r
set.seed(42)
comm <- matrix(rpois(40 * 25, lambda = 12), nrow = 40, ncol = 25)
colnames(comm) <- paste0("Taxon_", 1:25)
grp <- rep(c("Control", "Treatment"), each = 20)
perm_res <- cli_network_permutation(comm, grp, n_perm = 49, seed = 42)
print(perm_res)
#> 
#> =================================================================
#>  cliomicplot: Cross-Network Permutation Test
#>  Reference (Control) vs Comparison (Treatment)
#>  Number of Permutations: 49
#> =================================================================
#>                   metric ref_val test_val delta_obs perm_mean perm_se p_perm
#>             Edge Density  0.0500   0.0500    0.0000   -0.0034  0.0228   1.00
#>     Natural Connectivity  0.5612   0.5761    0.0149   -0.0333  0.3004   0.96
#>  Negative/Positive Ratio  0.5000   0.5000    0.0000    0.0141  0.4859   1.00
#>              Mean Degree  1.2000   1.2000    0.0000   -0.0816  0.5461   1.00
#>   Clustering Coefficient  0.0000   0.0000    0.0000    0.0027  0.1197   1.00
#> -----------------------------------------------------------------
#>  Note: Two-sided empirical p-value: (1 + sum(|delta_null| >= |delta_obs|)) / (B + 1)
#> 
```
