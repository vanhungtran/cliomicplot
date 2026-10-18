# Diamond Bubble Heatmap

Creates a matrix of diamond-shaped bubbles where marker-sample
combinations are encoded by size (e.g., fold-change) and colour (e.g.,
significance).

## Usage

``` r
type_bubble_heatmap(
  label_threshold = 2,
  max_size = 15,
  band_alpha = 0.6,
  band_fill = "#F2F5FA",
  cell_border = "#E2E6EF",
  size_name = "Fold change",
  fill_name = expression(-log[10] ~ "adjusted P"),
  fill_low = "#D6E4F0",
  fill_mid = "#59B8A8",
  fill_high = "#A83D62"
)
```

## Arguments

- label_threshold:

  Size threshold above which cell labels are drawn.

- max_size:

  Maximum point size (default 15).

- band_alpha:

  Opacity for alternating row bands.

- band_fill:

  Fill colour for alternating row bands.

- cell_border:

  Border colour for guide tiles.

- size_name:

  Legend title for the size scale.

- fill_name:

  Legend title for the fill colour scale.

- fill_low, fill_mid, fill_high:

  Colour stops for the fill gradient.

## Value

A `cliplot_type` object.
