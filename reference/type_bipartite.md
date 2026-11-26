# Transkingdom & Multi-Omics Bipartite Network Type

Creates a publication-grade bipartite interaction network connecting two
biological domains or kingdoms (e.g., Bacteria vs Fungi, or Gut
Microbiota vs Serum Metabolites) with signed correlation edges and
cross-domain hub highlighting (Wen et al. iMeta 2025).

## Usage

``` r
type_bipartite(
  domain_a = "Bacteria",
  domain_b = "Fungi",
  layout = c("bipolar", "concentric", "parallel"),
  edge_color_pos = "#4DBBD5",
  edge_color_neg = "#E64B35",
  edge_alpha = 0.65,
  edge_width_range = c(0.3, 1.8),
  node_color_a = "#3B4992",
  node_color_b = "#008B45",
  node_size_range = c(2.5, 6.5),
  label_top = 5,
  label_size = 2.8,
  cor_cutoff = 0.35,
  p_cutoff = 0.05,
  nodes = NULL,
  edges = NULL
)
```

## Arguments

- domain_a:

  Character label for Domain A (default `"Bacteria"`).

- domain_b:

  Character label for Domain B (default `"Fungi"`).

- layout:

  Character string specifying spatial arrangement: `"bipolar"` (default,
  two facing vertical or circular columns), `"concentric"` (Domain A in
  inner circle, Domain B in outer ring), or `"parallel"`.

- edge_color_pos:

  Color for positive cross-domain correlations (default `"#4DBBD5"`,
  cyan).

- edge_color_neg:

  Color for negative cross-domain correlations (default `"#E64B35"`,
  coral red).

- edge_alpha:

  Numeric transparency for cross-domain edges (default 0.65).

- edge_width_range:

  Range of line widths mapped to absolute correlation strength (default
  `c(0.3, 1.8)`).

- node_color_a:

  Color for Domain A nodes (default `"#3B4992"`, deep royal blue).

- node_color_b:

  Color for Domain B nodes (default `"#008B45"`, emerald green).

- node_size_range:

  Numeric vector of length 2 specifying point sizes mapped to degree
  (default `c(2.5, 6.5)`).

- label_top:

  Integer. Number of top cross-domain hubs to label per domain (default
  5).

- label_size:

  Numeric font size for node labels (default 2.8).

- cor_cutoff:

  Numeric minimum absolute correlation threshold to include edges
  (default 0.35).

- p_cutoff:

  Numeric p-value threshold for edge significance (default 0.05).

- nodes:

  Optional data frame of nodes with columns `node`, `domain`, and
  optional `phylum`.

- edges:

  Optional data frame of edges with columns `from`, `to`, `cor` (or
  `weight`), and optional `p_value`.

## Value

A `cliplot_type` object for use with
[`cliplot`](https://vanhungtran.github.io/cliomicplot/reference/cliplot.md).

## Examples

``` r
if (FALSE) { # \dontrun{
cliplot(type = type_bipartite(domain_a = "Bacteria", domain_b = "Metabolites",
                              nodes = nodes_df, edges = edges_df))
} # }
```
