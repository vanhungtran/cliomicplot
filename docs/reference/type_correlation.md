# Correlation Matrix Plot Type

Creates publication-ready correlation matrix plots with customizable
color scales, significance indicators, and clustering.

## Usage

``` r
type_correlation(
  method = "pearson",
  type = "full",
  add_coef = TRUE,
  coef_size = 3.5,
  color_low = "#4575B4",
  color_mid = "white",
  color_high = "#D73027",
  cluster = FALSE,
  show_diag = FALSE,
  sig_level = 0.05
)
```

## Arguments

- method:

  Correlation method: "pearson", "spearman", "kendall"

- type:

  Visualization type: "full", "upper", "lower"

- add_coef:

  Add correlation coefficients to cells (default TRUE)

- coef_size:

  Size of coefficient text (default 3.5)

- color_low:

  Low end of color gradient (default "#4575B4")

- color_mid:

  Midpoint color (default "white")

- color_high:

  High end of color gradient (default "#D73027")

- cluster:

  Cluster variables (default FALSE)

- show_diag:

  Show diagonal (default FALSE)

- sig_level:

  Significance level for stars (default 0.05)

## Value

A cliplot_type object for use with
[`cliplot`](https://vanhungtran.github.io/cliomicplot/reference/cliplot.md).

## Examples

``` r
if (FALSE) { # \dontrun{
# Correlation of numeric columns
cliplot(mtcars, type = "correlation")

# Custom correlation
cliplot(mtcars, type = type_correlation(method = "spearman", type = "upper"))
} # }
```
