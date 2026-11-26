# Evaluate Cross-Network Module Preservation Statistics

Computes module preservation statistics across reference and test
networks based on the Langfelder et al. (2011) framework. Evaluates
whether module density (average intramodular connectivity) and
connectivity patterns (hub gene structure) observed in a reference
network are preserved in an independent test network under a null
permutation distribution.

## Usage

``` r
cli_module_preservation(
  ref_data,
  test_data,
  ref_modules,
  test_modules = NULL,
  n_perm = 100,
  method = c("pearson", "spearman"),
  seed = 42
)
```

## Arguments

- ref_data:

  Numeric matrix or data frame of reference network profiles (samples in
  rows, features/genes/taxa in columns).

- test_data:

  Numeric matrix or data frame of test network profiles (samples in
  rows, features in columns). Must share column names with `ref_data`.

- ref_modules:

  Character or factor vector specifying module membership for each
  feature in `ref_data`.

- test_modules:

  Optional vector of module assignments in test data.

- n_perm:

  Integer. Number of permutation iterations to build the null
  distribution (default 100).

- method:

  Character correlation metric: `"pearson"` (default) or `"spearman"`.

- seed:

  Optional integer seed for reproducible permutations.

## Value

A data frame containing module-level preservation metrics:

- `module`: Module name.

- `size`: Number of member features.

- `z_density`: Standardized score for intramodular correlation density.

- `z_connectivity`: Standardized score for intramodular hub rank
  preservation.

- `z_summary`: Composite preservation score \\(Z\_{density} +
  Z\_{connectivity}) / 2\\.

- `p_density`: Permutation p-value for density preservation.

- `p_connectivity`: Permutation p-value for connectivity preservation.

- `preservation`: Categorical status: `"Strong"` (\\Z \ge 10\\),
  `"Moderate"` (\\2 \le Z \< 10\\), or `"None"` (\\Z \< 2\\).

## References

Langfelder P, Luo R, Oldham MC, Horvath S (2011). Is My Network Module
Preserved and Reproducible? *PLoS Computational Biology*, 7(1):
e1001057.
