# Partial Correlation with Covariate Adjustment

Computes first-order or higher-order partial correlation coefficients
between two variables or feature sets after partialling out linear
confounding effects of specified covariates (e.g., age, sex, library
size, batch).

## Usage

``` r
cli_partial_cor(x, y, z = NULL, method = c("pearson", "spearman"))
```

## Arguments

- x:

  Numeric vector or matrix of target variables.

- y:

  Numeric vector or matrix of response variables.

- z:

  Numeric vector, matrix, or data frame of confounding covariates.

- method:

  Correlation metric: `"pearson"` (default) or `"spearman"`.

## Value

A data frame containing estimate, t-statistic, p-value, and FDR padj.
