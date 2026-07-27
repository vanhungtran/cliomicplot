# Infographic Bar Chart

Creates a publication or media-ready bar chart with per-bar custom
colours, optional footer boxes with labels, reference lines, and value
annotations above each bar. Designed for polling, survey, and any
comparison where each category carries its own brand or semantic colour.

## Usage

``` r
type_infobar(
  bar_colors = NULL,
  box_colors = NULL,
  box_labels = NULL,
  box_sub_labels = NULL,
  reference_line = NULL,
  reference_label = NULL,
  value_labels = TRUE,
  value_suffix = "",
  change_labels = NULL,
  bar_width = 0.85,
  label_offset = 0.06,
  font_family = "sans",
  bg_color = "white"
)
```

## Arguments

- bar_colors:

  Named character vector mapping category labels to bar fill colours. If
  `NULL`, defaults to the active qualitative palette.

- box_colors:

  Named character vector mapping category labels to footer box fill
  colours. If `NULL`, no footer boxes are drawn.

- box_labels:

  Character vector of labels to draw inside each footer box, in the same
  order as the categories on the x-axis (default: the category names
  themselves). Pass `NULL` to suppress box text.

- box_sub_labels:

  Character vector of secondary labels to draw below each footer box
  (default `NULL`).

- reference_line:

  Numeric y-axis value at which to draw a horizontal reference line
  (default `NULL` = no line).

- reference_label:

  Optional label to place next to the reference line.

- value_labels:

  Logical. Whether to draw the bar value as a label above each bar
  (default `TRUE`).

- value_suffix:

  A string appended to each value label, e.g. `"%"` (default `""`).

- change_labels:

  Character vector of change annotations to draw below the value labels,
  in category order (default `NULL`).

- bar_width:

  Width of bars as a fraction of the category spacing (default 0.85).

- label_offset:

  Distance above each bar for value/change labels, expressed as a
  fraction of the maximum bar value (default 0.06).

- font_family:

  Base font family for all text (default `"sans"`).

- bg_color:

  Background colour for the plot (default `"white"`).

## Value

A `cliplot_type` object for use with
[`cliplot`](https://vanhungtran.github.io/cliomicplot/reference/cliplot.md).

## Formula convention

Call as
`cliplot(value ~ category, data = df, type = "infobar", bar_colors = c(CatA = "#FF0000", CatB = "#00FF00"))`.
Colours and labels are matched to categories by name (for named vectors)
or by position (for unnamed vectors).

## Examples

``` r
if (FALSE) { # \dontrun{
polls <- data.frame(party = c("CDU", "SPD", "AfD"), poll = c(17, 13, 18))
cliplot(poll ~ party, data = polls, type = "infobar",
        bar_colors = c(CDU = "#55B1BE", SPD = "#EC0016", AfD = "#119CD2"),
        value_suffix = "%", reference_line = 5)
} # }
```
