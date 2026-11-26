# Lines plot type

Draws lines connecting data points. Supports individual
subject/replicate trajectories via `group`, automatic calculation of
group mean trajectories with standard error bars (`mean ± SE`) or shaded
confidence bands (`show_ribbon`), horizontal positioning dodge to
prevent multi-group collisions, prominent white-stroked mean point
glyphs, and overlaid background observation points.

## Usage

``` r
type_lines(
  linewidth = 1.15,
  alpha = 0.95,
  group = NULL,
  aggregate = c("auto", "mean", "median", "none"),
  add_points = FALSE,
  point_size = 2,
  point_alpha = 0.35,
  show_se = FALSE,
  show_ribbon = FALSE,
  ribbon_alpha = 0.16,
  dodge = 0.45,
  errorbar_width = 0.35,
  errorbar_linewidth = 0.75,
  show_mean_points = TRUE,
  mean_point_size = 3.5
)
```

## Arguments

- linewidth:

  Line width (default 1.15).

- alpha:

  Line transparency (default 0.95).

- group:

  Column name or aesthetic for individual grouping (e.g. `"Individual"`,
  `"Patient"`).

- aggregate:

  Aggregation mode when duplicate observations exist at the same x:
  `"auto"` (default, aggregates by mean if duplicates exist and no group
  is specified), `"mean"`, `"median"`, or `"none"`.

- add_points:

  Logical. Whether to overlay individual observation points (default
  `FALSE`).

- point_size:

  Size of overlaid individual points (default 2.0).

- point_alpha:

  Transparency of overlaid points (default 0.35).

- show_se:

  Logical. Whether to display standard error bars (`mean ± SE`) (default
  `FALSE`).

- show_ribbon:

  Logical. Whether to display a shaded `mean ± SE` ribbon (default
  `FALSE`).

- ribbon_alpha:

  Transparency of shaded ribbon (default 0.16).

- dodge:

  Horizontal position dodge to prevent error bars and points from
  overlapping across groups (default 0.45 when multiple groups exist,
  otherwise 0).

- errorbar_width:

  Width of error bar caps (default 0.35).

- errorbar_linewidth:

  Line width of error bars (default 0.75).

- show_mean_points:

  Logical. Whether to place prominent point markers on the mean line
  (default `TRUE` when aggregating).

- mean_point_size:

  Size of mean point markers (default 3.5).
