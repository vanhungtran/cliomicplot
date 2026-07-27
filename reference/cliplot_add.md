# Add Layers to an Existing Cliomicplot

Adds additional layers (points, lines, text, etc.) to the last
cliomicplot created. Similar to tinyplot's \`tinyplot_add()\`.

## Usage

``` r
cliplot_add(type = "points", ...)
```

## Arguments

- type:

  Plot type for the added layer.

- ...:

  Additional arguments passed to the type's draw function.

## Details

Currently a placeholder. Full layer addition requires tracking the last
ggplot object, which will be implemented in a future version.
