# Show a Cliomicplot Palette

Displays a named palette as labeled color swatches.

## Usage

``` r
cli_palette_show(name = "jco")
```

## Arguments

- name:

  Character. Palette name from
  [`cli_palette_list`](https://vanhungtran.github.io/cliomicplot/reference/cli_palette_list.md).

## Value

Invisibly returns the plotted palette from
[`scales::show_col()`](https://scales.r-lib.org/reference/show_col.html).
