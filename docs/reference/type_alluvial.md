# Alluvial / Sankey Diagram Type

Creates flow diagrams showing transitions between categorical states.
Perfect for patient journeys, customer flows, or any sequential
categorical data.

## Usage

``` r
type_alluvial(
  alpha = 0.6,
  curve_type = "sigmoid",
  node_width = 0.15,
  show_labels = TRUE
)
```

## Arguments

- alpha:

  Flow transparency (default 0.6)

- curve_type:

  Curve type: "sigmoid", "linear", "cubic"

- node_width:

  Node width (default 0.15)

- show_labels:

  Show axis labels (default TRUE)

## Value

A cliplot_type object
