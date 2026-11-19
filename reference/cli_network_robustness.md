# Simulate Multi-Network Robustness Under Random and Targeted Node Attacks

Simulates cascading collapse or progressive degradation of biological
networks across conditions, treatments, or cohorts (e.g. KO vs WT vs
OE). Tests resilience under:

- `"random"`: Random species or gene loss (simulated across Monte Carlo
  permutations).

- `"degree"`: Targeted attacks removing highest-degree hub nodes first.

- `"betweenness"`: Targeted attacks removing critical network bottleneck
  nodes first.

Tracks remaining active node fraction, spectral natural connectivity, or
giant component size across progressive node removal fractions.

## Usage

``` r
cli_network_robustness(
  networks,
  attack = c("random", "degree"),
  rm_fractions = seq(0.05, 0.9, by = 0.05),
  metric = c("natural_connectivity", "remain_nodes", "giant_component"),
  n_perm = 30,
  threshold = 0.3,
  seed = 42
)
```

## Arguments

- networks:

  Named list of square numeric adjacency/correlation matrices, or a
  single numeric matrix. Names correspond to condition/cohort labels
  (e.g. `list(KO = mat1, WT = mat2)`).

- attack:

  Character vector specifying attack types: `"random"`, `"degree"`,
  and/or `"betweenness"`. Default `c("random", "degree")`.

- rm_fractions:

  Numeric vector of node removal fractions from 0.05 to 0.95 (default
  `seq(0.05, 0.90, by = 0.05)`).

- metric:

  Metric to monitor: `"natural_connectivity"` (default),
  `"remain_nodes"` (fraction of nodes with at least 1 edge), or
  `"giant_component"` (relative size of largest connected component).

- n_perm:

  Integer. Number of permutations for random removal (default 30).

- threshold:

  Numeric edge threshold to binarize correlation matrices (default 0.3).

- seed:

  Optional integer seed for reproducibility.

## Value

A tidy data frame with columns:

- `group`: Network condition name.

- `attack`: Attack type (`"random"`, `"degree"`, or `"betweenness"`).

- `fraction_removed`: Proportion of nodes deleted.

- `value`: Mean value of the evaluation metric.

- `se`: Standard error (non-zero for random removal).
