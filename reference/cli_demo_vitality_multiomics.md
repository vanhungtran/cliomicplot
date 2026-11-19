# Generate Synthetic Multi-Network Multi-Omics Dataset

Generates paired synthetic datasets matching the 3-condition
experimental setup (e.g. KO, WT, OE) and 4-corner environmental matrix
layout.

## Usage

``` r
cli_demo_vitality_multiomics(n_samples = 30, n_features = 60, seed = 42)
```

## Arguments

- n_samples:

  Number of samples per condition (default 30).

- n_features:

  Number of microbial or gene features (default 60).

- seed:

  Optional integer seed.

## Value

A list containing:

- `ref_expr`: Expression profiles for Reference (WT).

- `test_expr`: Expression profiles for Test (KO).

- `modules`: Named vector of module labels.

- `networks`: List of adjacency/correlation matrices for KO, WT, OE.

- `env_data`: Data frame of 15 environmental variables (pH, N, P, K,
  etc.).

- `links`: Formatted links table ready for
  [`cliplot_env_network`](https://vanhungtran.github.io/cliomicplot/reference/cliplot_env_network.md).
