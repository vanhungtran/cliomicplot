# Divergent Effect-Size Bubble Heatmap Archetype

Divergent Effect-Size Bubble Heatmap Archetype

## Usage

``` r
type_bubble_diverging(
  palette = "nature",
  shape = "circle",
  midpoint = 0,
  stroke_col = "grey35",
  stroke_width = 0.35,
  fill_name = expression(log[2] ~ "FC"),
  ...
)
```

## Arguments

- palette:

  Named publication palette or color vector:

  - Diverging: `"nature"`, `"rdbu"`, `"bipolar"`, `"lancet"`,
    `"spectral"`, `"piyg"`, `"prgn"`, `"dark"`.

  - Sequential: `"mako"`, `"rocket"`, `"viridis"`, `"plasma"`,
    `"blues"`, `"reds"`, `"ylorrd"`, `"purples"`.

- shape:

  Shape of the bubble glyphs: `"diamond"` (23, default), `"circle"`
  (21), `"square"` (22), `"triangle_up"` or `"triangle"` (24),
  `"triangle_down"` (25), or an integer shape code (21–25).

- midpoint:

  Value representing the midpoint for diverging palettes (default 0).

- stroke_col:

  Outline stroke colour for bubbles (default `"white"`).

- stroke_width:

  Outline stroke width (default 0.65).

- fill_name:

  Legend title for the fill colour scale.

## Value

A `cliplot_type` object.
