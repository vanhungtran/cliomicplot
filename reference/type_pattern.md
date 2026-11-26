# Expression Pattern Clustering Type

Creates publication-ready expression pattern clustering plots. Groups
features (genes/proteins) by their expression profiles across conditions
or time points using k-means, PAM, or soft clustering. Inspired by
xOmicsShiny's pattern module.

## Usage

``` r
type_pattern(
  k = 6,
  method = c("kmeans", "pam", "mfuzz"),
  ncol = 3,
  scale = TRUE,
  line_alpha = 0.35,
  centroid_color = "#E64B35",
  centroid_size = 1.2,
  min_memb = 0.4,
  seed = 123,
  cluster_labels = NULL
)
```

## Arguments

- k:

  Number of clusters (default 6)

- method:

  Clustering method: "kmeans", "pam", or "mfuzz" (default "kmeans")

- ncol:

  Number of columns in the facet grid (default 3)

- scale:

  Scale expression data before clustering? (default TRUE)

- line_alpha:

  Alpha for individual feature lines (default 0.35)

- centroid_color:

  Color for the cluster centroid line (default "#E64B35")

- centroid_size:

  Size for centroid line (default 1.2)

- min_memb:

  For "mfuzz": minimum membership threshold (default 0.4)

- seed:

  Random seed for reproducibility (default 123)

- cluster_labels:

  Optional named vector of cluster labels for annotation

## Value

A `cliplot_type` object for use with
[`cliplot`](https://vanhungtran.github.io/cliomicplot/reference/cliplot.md).

## Details

Pattern clustering reveals groups of features that share similar
expression trajectories across conditions or time points. This is
especially useful for time-series or multi-condition omics experiments.

The input should be a wide-format matrix or data frame where rows are
features (genes/proteins) and columns are conditions/time points.

## Examples

``` r
if (FALSE) { # \dontrun{
# Cluster expression profiles into 6 patterns
cliplot(expr_wide, type = type_pattern(k = 6, method = "kmeans"))

# PAM clustering with 4 clusters
cliplot(expr_wide, type = type_pattern(k = 4, method = "pam"))
} # }
```
