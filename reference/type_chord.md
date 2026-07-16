# Chord Diagram Type

Creates stunning chord diagrams for visualizing relationships and flows
between categories. Uses circlize under the hood. Perfect for showing
patient transitions, gene interactions, or any pairwise flow data.

## Usage

``` r
type_chord(
  alpha = 0.5,
  gap.degree = 4,
  start.degree = 90,
  directional = TRUE,
  link.alpha = 0.5
)
```

## Arguments

- alpha:

  Grid color transparency (default 0.5)

- gap.degree:

  Gap between sectors in degrees (default 4)

- start.degree:

  Starting angle (default 90)

- directional:

  Whether arcs have direction arrows (default TRUE)

- link.alpha:

  Transparency of links (default 0.5)

## Value

A cliplot_type object
