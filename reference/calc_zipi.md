# Compute Zi (Within-Module Connectivity) and Pi (Participation Coefficient)

Calculates node topological roles within a modular network:
within-module degree z-score (\\Z_i\\) and participation coefficient
(\\P_i\\).

## Usage

``` r
calc_zipi(adj_mat, modules, z_threshold = 2.5, p_threshold = 0.62)
```

## Arguments

- adj_mat:

  Numeric adjacency matrix (symmetric, unweighted or weighted \> 0) with
  node identifiers in row and column names.

- modules:

  Named vector or data frame mapping node names to module IDs.

- z_threshold:

  Numeric cutoff for module hubs on \\Z_i\\ (default 2.5).

- p_threshold:

  Numeric cutoff for connectors on \\P_i\\ (default 0.62).

## Value

A `data.frame` with columns:

- `node`: Node identifier

- `module`: Module assignment

- `degree`: Total degree (number of connections)

- `within_degree`: Intra-module degree (\\k\_{i, s_i}\\)

- `Zi`: Within-module connectivity z-score

- `Pi`: Among-module participation coefficient

- `role`: Classified topological role (`"Network hub"`, `"Module hub"`,
  `"Connector"`, or `"Peripheral"`)

## Examples

``` r
if (FALSE) { # \dontrun{
adj <- matrix(sample(c(0, 1), 100, replace = TRUE, prob = c(0.8, 0.2)), 10, 10)
diag(adj) <- 0
adj <- (adj + t(adj)) / 2
rownames(adj) <- colnames(adj) <- paste0("OTU_", 1:10)
mods <- setNames(rep(c("M1", "M2"), each = 5), rownames(adj))
res <- calc_zipi(adj, mods)
} # }
```
