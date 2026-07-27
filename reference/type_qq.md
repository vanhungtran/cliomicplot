# Q-Q Plot Type

Creates quantile-quantile plots to assess normality or compare two
distributions.

## Usage

``` r
type_qq(
  distribution = stats::qnorm,
  dparams = list(),
  point_size = 2,
  point_alpha = 0.6,
  show_line = TRUE,
  line_color = "#E64B35"
)
```

## Arguments

- distribution:

  Target distribution function (e.g., stats::qnorm)

- dparams:

  List of distribution parameters

- point_size:

  Point size

- point_alpha:

  Point transparency

- show_line:

  Add reference line (default TRUE)

- line_color:

  Color for reference line

## Value

A cliplot_type object
