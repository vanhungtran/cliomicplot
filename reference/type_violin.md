# Violin Plot Type

Creates publication-ready violin plots with optional boxplot overlay and
jittered points.

## Usage

``` r
type_violin(
  add_boxplot = TRUE,
  add_jitter = TRUE,
  jitter_width = 0.15,
  jitter_alpha = 0.45,
  jitter_size = 1.6,
  violin_alpha = 0.78,
  box_width = 0.2,
  trim = TRUE
)
```

## Arguments

- add_boxplot:

  Overlay boxplot (default TRUE)

- add_jitter:

  Add jittered points (default TRUE)

- jitter_width:

  Width of jitter (default 0.15)

- jitter_alpha:

  Transparency of jittered points (default 0.5)

- jitter_size:

  Size of jittered points (default 1.5)

- violin_alpha:

  Violin transparency (default 0.7)

- box_width:

  Width of boxplot overlay (default 0.2)

- trim:

  Trim violin tails to data range (default TRUE)

## Value

A cliplot_type object for use with
[`cliplot`](https://vanhungtran.github.io/cliomicplot/reference/cliplot.md).

## Examples

``` r
if (FALSE) { # \dontrun{
cliplot(len ~ dose, data = ToothGrowth, type = "violin")

# Custom violin
cliplot(len ~ dose, data = ToothGrowth,
        type = type_violin(add_boxplot = FALSE, jitter_width = 0.1))
} # }
```
