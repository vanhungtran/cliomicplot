# Move the Legend Outside the Plot

Theme helper that moves a ggplot legend outside the plotting area, the
default layout for multi-panel Nature/Cell/Science figures.

## Usage

``` r
cli_legend_out(position = "right", margin = 8, ...)
```

## Arguments

- position:

  Legend position: `"right"` (default), `"left"`, `"top"`, or
  `"bottom"`.

- margin:

  Extra margin between the plot and the legend (default 8 pt on the
  inner side).

- ...:

  Additional arguments passed to
  [`ggplot2::theme()`](https://ggplot2.tidyverse.org/reference/theme.html).

## Value

A ggplot theme object for use with `+`.

## Examples

``` r
if (FALSE) { # \dontrun{
cliplot(Sepal.Length ~ Petal.Length | Species, data = iris) + cli_legend_out()
} # }
```
