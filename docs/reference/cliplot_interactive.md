# Interactive Plot Mode

Converts a cliomicplot ggplot to an interactive plotly chart with hover
tooltips, zoom, and pan support.

## Usage

``` r
cliplot_interactive(p, tooltip = NULL, width = NULL, height = NULL)
```

## Arguments

- p:

  A ggplot object produced by
  [`cliplot`](https://vanhungtran.github.io/cliomicplot/reference/cliplot.md).

- tooltip:

  Columns to show in hover tooltips (default: all mapped)

- width:

  Plot width (NULL = auto)

- height:

  Plot height (NULL = auto)

## Value

A plotly interactive plot object
