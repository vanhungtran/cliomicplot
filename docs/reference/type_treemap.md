# Treemap Type

Creates treemaps for visualizing hierarchical proportional data.
Rectangles are sized by value and colored by category. Perfect for
showing composition of budgets, genomic features, or any nested data.

## Usage

``` r
type_treemap(
  alpha = 0.85,
  border_color = "white",
  border_width = 1.5,
  show_labels = TRUE,
  label_size = 3
)
```

## Arguments

- alpha:

  Fill transparency (default 0.85)

- border_color:

  Border color for rectangles (default "white")

- border_width:

  Border width (default 1.5)

- show_labels:

  Show category labels (default TRUE)

- label_size:

  Label text size (default 3)

## Value

A cliplot_type object
