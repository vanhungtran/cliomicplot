# Calculate Module Eigengenes and Composite Scores

Summarizes high-dimensional omics modules (e.g. microbial clusters, gene
co-expression modules, or metabolite clusters) into single
representative profile vectors per module. Supports:

- `"eigengene"`: First principal component (PC1) via singular value
  decomposition (SVD), with orientation aligned to positively correlate
  with average module expression.

- `"mean_score"`: Mean of z-score standardized member features.

- `"sum"`: Sum of raw feature abundances.

## Usage

``` r
cli_module_summary(
  expr_data,
  modules,
  method = c("eigengene", "mean_score", "sum"),
  prefix = "ME"
)
```

## Arguments

- expr_data:

  Numeric matrix or data frame of molecular or microbial profiles
  (samples in rows, features in columns).

- modules:

  Character or factor vector of module assignments for each feature in
  `expr_data`. Can also be a named vector where names correspond to
  column names of `expr_data`.

- method:

  Character metric: `"eigengene"` (default), `"mean_score"`, or `"sum"`.

- prefix:

  Optional character prefix for output module column names (default
  `"ME"`).

## Value

A data frame with samples in rows and module summary scores in columns.
