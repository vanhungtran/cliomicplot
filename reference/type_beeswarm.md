# Beeswarm Plot Type

Creates beautiful beeswarm plots — points arranged to avoid overlap
while showing the full distribution. More informative than boxplots
alone.

## Usage

``` r
type_beeswarm(
  point_size = 2,
  point_alpha = 0.7,
  spacing = 0.5,
  method = "swarm"
)
```

## Arguments

- point_size:

  Point size (default 2)

- point_alpha:

  Point transparency (default 0.7)

- spacing:

  Point spacing (default 0.5)

- method:

  Beeswarm method: "swarm", "compactswarm", "hex", "square"

## Value

A cliplot_type object
