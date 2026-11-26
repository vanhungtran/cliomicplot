# Differential Network Rewiring and Edge Conservation Analysis

Compares edge architecture and correlation strengths between two
conditions (e.g., Control vs Disease or Wild-Type vs Knockout).
Identifies conserved edges, rewired edges, edge Jaccard similarity, and
evaluates differential correlation significance using Fisher's
z-transformation.

## Usage

``` r
cli_network_rewiring(net1, net2, n1 = 30, n2 = 30, r_threshold = 0.4)
```

## Arguments

- net1:

  Numeric correlation or adjacency matrix for Condition 1.

- net2:

  Numeric correlation or adjacency matrix for Condition 2.

- n1:

  Sample size of Condition 1 (used for Fisher's z-test, default 30).

- n2:

  Sample size of Condition 2 (used for Fisher's z-test, default 30).

- r_threshold:

  Absolute correlation threshold to define active edges (default 0.4).

## Value

A list containing:

- `jaccard_similarity`: Global Jaccard index of conserved edges \\\|E_1
  \cap E_2\| / \|E_1 \cup E_2\|\\.

- `summary`: Counts of Condition 1-specific, Condition 2-specific, and
  conserved edges.

- `edges`: Data frame of all evaluated feature pairs with \\r_1\\,
  \\r_2\\, Fisher's \\Z\\-score, p-value, and classification status
  (`"Conserved"`, `"Specific_Net1"`, `"Specific_Net2"`, `"Rewired"`).

- `node_rewiring`: Data frame of per-node rewiring scores based on
  topological neighborhood divergence.
