# Bubble Chart Type

Creates bubble charts where point size encodes a third variable. Uses
the `size` aesthetic in ggplot2. The size variable can be provided via
the `z` parameter or via the formula interface as `z ~ x | by` with `y`
passed separately.

## Usage

``` r
type_bubble(
  z = NULL,
  alpha = 0.65,
  scale_max = 15,
  stroke = 0.35,
  scale_size = TRUE
)
```

## Arguments

- z:

  Variable or column name for the size dimension (bubble radius). If
  NULL, tries to find a third numeric column in the data.

- alpha:

  Point transparency (0-1)

- scale_max:

  Maximum bubble size in mm (default 15)

- stroke:

  Point outline stroke width

- scale_size:

  Logical; apply sqrt scaling to area (default TRUE)

## Value

A cliplot_type object for use with
[`cliplot`](https://vanhungtran.github.io/cliomicplot/reference/cliplot.md).

## Examples

``` r
if (FALSE) { # \dontrun{
# Bubble chart with built-in dataset
cliplot(mpg ~ wt, data = mtcars,
        type = type_bubble(z = mtcars$hp))

# Custom bubble scaling
cliplot(Sepal.Length ~ Sepal.Width, data = iris,
        type = type_bubble(z = iris$Petal.Length, scale_max = 20))
} # }
```
