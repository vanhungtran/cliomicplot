# Jitter Plot Type

Creates scatter plots with random jitter to reduce overplotting. Useful
for visualizing discrete or rounded continuous data.

## Usage

``` r
type_jitter(alpha = NULL, size = NULL, width = 0.2, height = 0, stroke = 0.35)
```

## Arguments

- alpha:

  Point transparency (0-1)

- size:

  Point size

- width:

  Amount of horizontal jitter (default 0.2)

- height:

  Amount of vertical jitter (default 0)

- stroke:

  Point outline stroke width

## Value

A cliplot_type object for use with
[`cliplot`](https://vanhungtran.github.io/cliomicplot/reference/cliplot.md).

## Examples

``` r
if (FALSE) { # \dontrun{
# Jitter points to show density
cliplot(mpg ~ factor(cyl), data = mtcars, type = "jitter")

# Custom jitter
cliplot(mpg ~ factor(cyl), data = mtcars,
        type = type_jitter(width = 0.3, height = 0.1))
} # }
```
