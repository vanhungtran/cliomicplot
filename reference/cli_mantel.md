# Mantel and Partial Mantel Test for Omics-Environment Associations

Evaluates association between biological community or module distance
matrices and environmental factor distance matrices using matrix
permutations. Supports partial Mantel tests to control for confounders
such as geographic distance, sample collection batch, sequencing depth,
or subject age.

## Usage

``` r
cli_mantel(
  comm_data,
  env_data,
  dist_comm = c("bray", "jaccard", "euclidean", "manhattan"),
  dist_env = c("euclidean", "manhattan"),
  method = c("pearson", "spearman"),
  n_perm = 999,
  partial_data = NULL,
  by_variable = TRUE,
  seed = 42
)
```

## Arguments

- comm_data:

  Numeric matrix or data frame of community abundances, module profiles,
  or an existing `dist` object (samples in rows, features in columns).

- env_data:

  Numeric matrix or data frame of environmental variables, clinical
  traits, or an existing `dist` object (samples in rows, variables in
  columns).

- dist_comm:

  Dissimilarity metric for community data: `"bray"` (default),
  `"jaccard"`, `"euclidean"`, or `"manhattan"`.

- dist_env:

  Dissimilarity metric for environmental factors: `"euclidean"`
  (default) or `"manhattan"`.

- method:

  Correlation method between distance vectors: `"pearson"` (default) or
  `"spearman"`.

- n_perm:

  Integer. Number of permutation iterations to evaluate statistical
  significance (default 999).

- partial_data:

  Optional matrix, data frame, or `dist` object of confounding
  covariates to condition upon for partial Mantel testing.

- by_variable:

  Logical. If TRUE (default), tests each environmental variable
  individually in addition to the overall multivariate environmental
  matrix.

- seed:

  Optional integer seed for reproducible permutations.

## Value

A data frame containing:

- `variable`: Environmental factor name (or `"All_Variables"`).

- `statistic`: Mantel correlation coefficient \\r\\.

- `pvalue`: Two-sided permutation p-value.

- `padj`: Benjamini-Hochberg FDR-adjusted p-value.

- `signif`: Significance asterisks (\* p \< 0.05, \*\* p \< 0.01, \*\*\*
  p \< 0.001).

- `method`: Test designation (`"Mantel"` or `"Partial_Mantel"`).

## References

Mantel N (1967). The detection of disease clustering and a generalized
regression approach. *Cancer Research*, 27(2): 209-220.
