# Compute Dissimilarity / Distance Matrix

Compute Dissimilarity / Distance Matrix

## Usage

``` r
calc_dist_matrix(mat, method = c("bray", "jaccard", "euclidean", "manhattan"))
```

## Arguments

- mat:

  Numeric matrix (samples in rows, features in columns).

- method:

  Distance metric: `"bray"`, `"jaccard"`, `"euclidean"`, or
  `"manhattan"`.

## Value

A `dist` object.
