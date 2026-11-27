# Scientific Reproduction Scorecard Type

Creates publication-grade scientific reproduction and audit scorecard
tables (Figure R6 in Nature Metabolism MNP reproduction). Renders
categorized audit rows with colored verdict status glyphs, published
statement summaries, and re-analysis findings.

## Usage

``` r
type_scorecard(
  verdict_colors = c(reproduced = "#0F4D92", partly = "#8FB4DC", `not reproduced` =
    "#555555", `no reference` = "#C9C9C9", `not attempted` = "#FFFFFF"),
  border_colors = c(reproduced = "#0F4D92", partly = "#8FB4DC", `not reproduced` =
    "#555555", `no reference` = "#777777", `not attempted` = "#777777"),
  dot_size = 2.6,
  text_size = 3,
  header_size = 3.4,
  column_x = c(dot = 1.2, what = 3.5, pub = 45, rep = 64),
  ...
)
```

## Arguments

- verdict_colors:

  Named character vector of colors for verdict categories:
  `"reproduced"` (default `"#0F4D92"`), `"partly"` (default
  `"#8FB4DC"`), `"not reproduced"` (default `"#555555"`),
  `"no reference"` (default `"#C9C9C9"`), `"not attempted"` (default
  `"#FFFFFF"`).

- border_colors:

  Named character vector of border colors for verdict points.

- dot_size:

  Numeric size of verdict dots (default `2.6`).

- text_size:

  Numeric size of row labels (default `3.0`).

- header_size:

  Numeric size of group section headers (default `3.4`).

- column_x:

  Named numeric vector with x-positions for columns:
  `c(dot = 1.2, what = 3.5, pub = 45, rep = 64)`.

- ...:

  Additional arguments.

## Value

A `cliplot_type` object for use with
[`cliplot`](https://vanhungtran.github.io/cliomicplot/reference/cliplot.md).
