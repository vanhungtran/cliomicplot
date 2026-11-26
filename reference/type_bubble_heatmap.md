# Diamond Bubble Heatmap

Creates a matrix of diamond-shaped bubbles where marker-sample
combinations are encoded by size (e.g., fold-change) and colour (e.g.,
significance).

## Usage

``` r
type_bubble_heatmap(
  label_threshold = 1.5,
  max_size = 9,
  label_size = 2.4,
  band_alpha = 0.6,
  band_fill = "#F2F5FA",
  cell_border = "#E2E6EF",
  size_name = "|log₂FC|",
  fill_name = expression(-log[10] ~ "adjusted P"),
  fill_low = "#D6E4F0",
  fill_mid = "#59B8A8",
  fill_high = "#A83D62",
  axis_text_size = NULL,
  axis_text_angle = 45,
  transpose = FALSE
)
```

## Arguments

- label_threshold:

  Size threshold above which cell labels are drawn (default 1.5).

- max_size:

  Maximum point size (default 9).

- label_size:

  Font size for cell text labels (default 2.4).

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

- axis_text_size:

  Size of axis text. If NULL (default), adaptively scaled.

- axis_text_angle:

  Angle for x-axis labels (default 45).

- transpose:

  If TRUE, swaps rows and columns (default FALSE).

## Value

A `cliplot_type` object.
