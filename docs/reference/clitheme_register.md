# Register a Custom Theme

Register a custom ggplot2 theme under a given name for use with
[`clitheme`](https://vanhungtran.github.io/cliomicplot/reference/clitheme.md)
and
[`cliplot`](https://vanhungtran.github.io/cliomicplot/reference/cliplot.md).

## Usage

``` r
clitheme_register(name, base = "cli_bw", ...)
```

## Arguments

- name:

  Character string. Name to register the theme under.

- base:

  Character string. Base theme name to build on.

- ...:

  Additional theme elements passed to
  [`ggplot2::theme()`](https://ggplot2.tidyverse.org/reference/theme.html).

## Value

Invisibly returns the registered theme name.
