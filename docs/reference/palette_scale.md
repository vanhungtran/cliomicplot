# Build a ggplot2 Scale from a Cliomicplot Palette

A generic scale builder that resolves any named palette from the
cliomicplot registry into a ggplot2 Scale object. Add it to any plot
with \`+\`. This avoids the per-palette function explosion pattern and
keeps the API surface small.

## Usage

``` r
palette_scale(
  palette = "jco",
  aesthetic = c("color", "fill"),
  type = c("discrete", "continuous"),
  alpha = 1,
  reverse = FALSE,
  ...
)

scale_color_cli(palette = "jco", alpha = 1, reverse = FALSE, ...)

scale_fill_cli(palette = "jco", alpha = 1, reverse = FALSE, ...)

scale_color_cli_c(palette = "blues", alpha = 1, reverse = FALSE, ...)

scale_fill_cli_c(palette = "blues", alpha = 1, reverse = FALSE, ...)
```

## Arguments

- palette:

  Character. Name of a palette in the registry (use
  \`cli_palette_list()\` to see choices).

- aesthetic:

  One of \`"color"\` or \`"fill"\`.

- type:

  One of \`"discrete"\` (default) or \`"continuous"\`. Continuous works
  best with sequential/diverging palettes.

- alpha:

  Numeric in (0, 1\]. Opacity applied to all colours.

- reverse:

  Logical. If \`TRUE\`, reverse the palette order.

- ...:

  Further arguments passed to the underlying
  \`ggplot2::scale\_\*\_manual()\` or
  \`ggplot2::scale\_\*\_gradientn()\`.

## Value

A ggplot2 Scale object.

## Examples

``` r
if (FALSE) { # \dontrun{
library(ggplot2)

# Discrete colour
ggplot(iris, aes(Sepal.Length, Petal.Length, colour = Species)) +
  geom_point(size = 3) +
  palette_scale("jama", "color")

# Discrete fill
ggplot(mpg, aes(class, fill = class)) +
  geom_bar() +
  palette_scale("cosmic", "fill")

# Continuous fill (heatmap)
ggplot(faithfuld, aes(waiting, eruptions, fill = density)) +
  geom_tile() +
  palette_scale("heatmap_rdbu", "fill", type = "continuous")
} # }
```
