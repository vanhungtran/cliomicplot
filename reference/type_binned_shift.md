# Binned Distribution Shift Type

Creates binned distribution and size shift profiles (Figure R10 in
Nature Metabolism MNP reproduction). Plots binned frequencies across
treatment cohorts alongside difference-to-control shift curves.

## Usage

``` r
type_binned_shift(
  mode = c("distribution", "difference"),
  ref_group = "Ctrl",
  line_width = 0.9,
  point_size = 2.6,
  ...
)
```

## Arguments

- mode:

  One of `"distribution"` (raw binned fractions) or `"difference"`
  (difference to reference group).

- ref_group:

  Name of control/reference cohort (default `"Ctrl"`).

- line_width:

  Linewidth for trajectory lines (default `0.9`).

- point_size:

  Size of bin points (default `2.6`).

- ...:

  Additional arguments.

## Value

A `cliplot_type` object for use with
[`cliplot`](https://vanhungtran.github.io/cliomicplot/reference/cliplot.md).
