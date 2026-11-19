# Standalone Module Preservation Figure

High-level wrapper to plot cross-network module preservation results.

## Usage

``` r
cliplot_module_preservation(
  data,
  title = NULL,
  subtitle = NULL,
  caption = NULL,
  ...
)
```

## Arguments

- data:

  Data frame returned by
  [`cli_module_preservation`](https://vanhungtran.github.io/cliomicplot/reference/cli_module_preservation.md)
  or containing size and z_summary columns.

- ...:

  Additional styling arguments passed to
  [`type_module_preservation`](https://vanhungtran.github.io/cliomicplot/reference/type_module_preservation.md).

## Value

A `ggplot` object.
