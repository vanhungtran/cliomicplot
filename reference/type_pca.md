# PCA Plot Type

Creates publication-ready PCA/MDS plots for visualizing high-dimensional
omics data. Supports ellipses, sample labeling, and scree plots.

## Usage

``` r
type_pca(
  pc_x = 1,
  pc_y = 2,
  center = TRUE,
  scale. = TRUE,
  add_ellipse = TRUE,
  ellipse_level = 0.95,
  point_size = 3.2,
  point_alpha = 0.9,
  point_stroke = 0.45,
  label_samples = FALSE,
  label_size = 3,
  show_scree = FALSE,
  ellipse_alpha = 0.13,
  show_centroids = TRUE
)
```

## Arguments

- pc_x:

  Principal component for x-axis (default 1)

- pc_y:

  Principal component for y-axis (default 2)

- center:

  Center data before PCA (default TRUE)

- scale.:

  Scale data before PCA (default TRUE)

- add_ellipse:

  Add confidence ellipses (default TRUE)

- ellipse_level:

  Confidence level for ellipses (default 0.95)

- point_size:

  Point size (default 3.2)

- point_alpha:

  Point transparency (default 0.9)

- point_stroke:

  Point outline width.

- label_samples:

  Label sample names (default FALSE)

- label_size:

  Label text size (default 3)

- show_scree:

  Show scree plot as inset (default FALSE)

- ellipse_alpha:

  Fill transparency for confidence ellipses.

- show_centroids:

  Add group centroid markers.

## Value

A cliplot_type object for use with
[`cliplot`](https://vanhungtran.github.io/cliomicplot/reference/cliplot.md).

## Examples

``` r
if (FALSE) { # \dontrun{
# PCA of iris data
pca_data = iris[, 1:4]
cliplot(pca_data, type = "pca", by = iris$Species)

# Custom PCA
cliplot(pca_data, type = type_pca(add_ellipse = TRUE, label_samples = TRUE),
        by = iris$Species)

# From existing PCA results
pca_res = prcomp(iris[, 1:4], scale. = TRUE)
pca_scores = as.data.frame(pca_res$x)
pca_scores$Species = iris$Species
cliplot(PC2 ~ PC1, data = pca_scores, type = "points", by = Species)
} # }
```
