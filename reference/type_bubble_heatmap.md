# Bubble Heatmap Archetypes (Diamond, Circle, Square, and Diverging)

Creates a matrix of bubbles where marker-sample combinations are encoded
simultaneously by shape size (e.g., effect magnitude, \|log2FC\|, -log10
P, or abundance) and colour (e.g., continuous significance, directional
NES, or log2FC). Supports multiple glyph shapes (diamond, circle,
square, triangle), sequential and diverging publication palettes,
significance asterisks, and customizable matrix backgrounds (banded,
grid, minimal).

## Usage

``` r
type_bubble_heatmap(
  shape = c("diamond", "circle", "square", "triangle_up", "triangle_down"),
  palette_type = c("sequential", "diverging"),
  palette = NULL,
  fill_low = "#D6E4F0",
  fill_mid = "#59B8A8",
  fill_high = "#A83D62",
  midpoint = 0,
  limits = NULL,
  min_size = 1,
  max_size = 9,
  stroke_col = "white",
  stroke_width = 0.65,
  point_alpha = 0.95,
  grid_style = c("banded", "grid", "minimal", "none"),
  band_alpha = 0.6,
  band_fill = "#F2F5FA",
  cell_border = "#E2E6EF",
  label_metric = c("value", "signif", "none"),
  label_threshold = 1.5,
  signif_cutoffs = c(0.05, 0.01, 0.001),
  signif_labels = c("*", "**", "***"),
  label_size = 2.4,
  label_col = "#182033",
  size_name = "|log₂FC|",
  fill_name = expression(-log[10] ~ "adjusted P"),
  axis_text_size = NULL,
  axis_text_angle = 45,
  transpose = FALSE
)
```

## Arguments

- shape:

  Shape of the bubble glyphs: `"diamond"` (23, default), `"circle"`
  (21), `"square"` (22), `"triangle_up"` or `"triangle"` (24),
  `"triangle_down"` (25), or an integer shape code (21–25).

- palette_type:

  Color gradient type: `"sequential"` (default) or `"diverging"`.

- palette:

  Named publication palette or color vector:

  - Diverging: `"nature"`, `"rdbu"`, `"bipolar"`, `"lancet"`,
    `"spectral"`, `"piyg"`, `"prgn"`, `"dark"`.

  - Sequential: `"mako"`, `"rocket"`, `"viridis"`, `"plasma"`,
    `"blues"`, `"reds"`, `"ylorrd"`, `"purples"`.

- fill_low, fill_mid, fill_high:

  Explicit colour stops for the fill gradient when `palette = NULL`.

- midpoint:

  Value representing the midpoint for diverging palettes (default 0).

- limits:

  Numeric vector of length 2 specifying fill scale limits.

- min_size, max_size:

  Point size range (defaults `min_size = 1.0`, `max_size = 9.0`).

- stroke_col:

  Outline stroke colour for bubbles (default `"white"`).

- stroke_width:

  Outline stroke width (default 0.65).

- point_alpha:

  Bubble fill transparency (default 0.95).

- grid_style:

  Background matrix style: `"banded"` (alternating row stripes with
  borders, default), `"grid"` (clean tile matrix borders without
  stripes), `"minimal"` (subtle grid guides), or `"none"`.

- band_alpha:

  Opacity for alternating row bands in banded style (default 0.6).

- band_fill:

  Fill colour for alternating row bands (default `"#F2F5FA"`).

- cell_border:

  Border colour for guide tiles (default `"#E2E6EF"`).

- label_metric:

  Content of glyph text labels: `"value"` (numeric size value if above
  threshold), `"signif"` (significance asterisks `*`, `**`, `***`), or
  `"none"`.

- label_threshold:

  Size threshold above which cell labels are drawn when
  `label_metric = "value"` (default 1.5).

- signif_cutoffs:

  Significance thresholds for asterisks when `label_metric = "signif"`
  (default `c(0.05, 0.01, 0.001)`).

- signif_labels:

  Asterisk labels corresponding to cutoffs (default
  `c("*", "**", "***")`).

- label_size:

  Font size for cell text labels (default 2.4).

- label_col:

  Font colour for cell text labels (default `"#182033"`).

- size_name:

  Legend title for the size scale.

- fill_name:

  Legend title for the fill colour scale.

- axis_text_size:

  Size of axis text. If NULL (default), adaptively scaled.

- axis_text_angle:

  Angle for x-axis labels (default 45).

- transpose:

  If TRUE, swaps rows and columns (default FALSE).

## Value

A `cliplot_type` object.
