# Dendrogram Type

Creates a dendrogram (hierarchical clustering tree) from data.
Automatically clusters rows and displays the tree structure.

## Usage

``` r
type_dendrogram(
  dist_method = "euclidean",
  hclust_method = "complete",
  k = NULL,
  linewidth = 0.6,
  leaf_size = 3
)
```

## Arguments

- dist_method:

  Distance method: "euclidean", "manhattan", etc.

- hclust_method:

  Clustering method: "complete", "ward.D2", etc.

- k:

  Number of clusters to color (default NULL = no coloring)

- linewidth:

  Line width (default 0.6)

- leaf_size:

  Leaf label size (default 3)

## Value

A cliplot_type object
