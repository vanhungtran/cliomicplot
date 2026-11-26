# Grammar Theme for Coupled Environmental Networks

Eliminates Cartesian grid lines and axis ticks while formatting compact,
multi-tier publication legends conforming to SciFig publication
standards.

## Usage

``` r
theme_env_network(
  preset = "default",
  legend_position = "top",
  show_module_legend = TRUE,
  link_width = NULL
)
```

## Arguments

- preset:

  Publisher styling preset: `"default"`, `"nature"`, `"clinical"`,
  `"seasonal"`, `"cell"`, `"dark"`, `"lancet"`, `"minimal"`,
  `"multi_tissue"`, `"circadian"`, or `"bipolar"`.

- legend_position:

  Position of legend: `"top"` (default), `"bottom"`, or `"none"`.

- show_module_legend:

  Logical. If TRUE, shows module color legend.

- link_width:

  Optional constant link width.

## Value

A list containing `guides` and a `theme` object.
