# Split and Half Violin Geom

A ggplot2 extension geom that renders split violins (alternating left
and right halves for comparing two conditions) or directional half
violins with integrated quantile lines.

## Usage

``` r
geom_split_violin(
  mapping = NULL,
  data = NULL,
  stat = "ydensity",
  position = "identity",
  ...,
  quantiles = NULL,
  quantile_color = "grey25",
  quantile_linewidth = 0.45,
  quantile_linetype = "solid",
  side = "split",
  trim = TRUE,
  scale = "width",
  na.rm = FALSE,
  show.legend = NA,
  inherit.aes = TRUE
)
```

## Arguments

- mapping:

  Set of aesthetic mappings created by
  [`aes`](https://ggplot2.tidyverse.org/reference/aes.html).

- data:

  The data to be displayed in this layer.

- stat:

  The statistical transformation to use on the data for this layer
  (default `"ydensity"`).

- position:

  Position adjustment (default `"identity"`).

- ...:

  Other arguments passed on to
  [`layer`](https://ggplot2.tidyverse.org/reference/layer.html).

- quantiles:

  Numeric vector of quantiles to draw inside the violin (e.g.,
  `c(0.25, 0.5, 0.75)`).

- quantile_color:

  Color for quantile lines (default `"grey25"`).

- quantile_linewidth:

  Line width for quantile lines (default 0.45).

- quantile_linetype:

  Line type for quantile lines (default `"solid"`).

- side:

  Drawing side: `"split"` (alternates left/right by group), `"left"`,
  `"right"`, or `"both"`.

- trim:

  If `TRUE` (default), trim the tails of the violins to the observed
  data range.

- scale:

  Density scaling method: `"width"` (default), `"area"`, or `"count"`.

- na.rm:

  If `FALSE` (default), missing values are removed with a warning.

- show.legend:

  Logical. Should this layer be included in the legends?

- inherit.aes:

  If `FALSE`, overrides the default aesthetics.

## Value

A ggplot2 layer.
