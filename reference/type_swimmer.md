# Swimmer Plot Type

Creates publication-ready swimmer plots for visualizing individual
patient timelines, including treatment duration, response, and key
clinical events.

## Usage

``` r
type_swimmer(
  id = NULL,
  time_start = NULL,
  time_end = NULL,
  bar_fill = NULL,
  event_times = NULL,
  bar_alpha = 0.9,
  sort_by = "time_end",
  event_shape_map = NULL,
  event_color_map = NULL,
  bar_linewidth = 8,
  bar_palette = "jama"
)
```

## Arguments

- id:

  Column name for patient IDs

- time_start:

  Column name for start times (default created from data)

- time_end:

  Column name for end/duration times

- bar_fill:

  Column name for bar coloring (e.g., response category)

- event_times:

  Optional data frame of event markers with columns: `id`, `time`,
  `event` (event type)

- bar_alpha:

  Bar transparency (default 0.9)

- sort_by:

  Column to sort patients by (default "time_end")

- event_shape_map:

  Named vector mapping event types to pch values

- event_color_map:

  Named vector mapping event types to colors

- bar_linewidth:

  Timeline bar thickness.

- bar_palette:

  Palette used for response/treatment bars.

## Value

A cliplot_type object for use with
[`cliplot`](https://vanhungtran.github.io/cliomicplot/reference/cliplot.md).

## Examples

``` r
if (FALSE) { # \dontrun{
# Patient timeline data
pt_data = data.frame(
  ID       = paste0("Pt", 1:20),
  Duration = round(runif(20, 3, 24), 1),
  Response = sample(c("CR","PR","SD","PD"), 20, replace=TRUE),
  Arm      = sample(c("Treatment","Control"), 20, replace=TRUE)
)

# Event markers
events = data.frame(
  ID    = rep(paste0("Pt", 1:20), each = 2),
  Time  = round(runif(40, 0, 24), 1),
  Event = sample(c("Progression","AE","Dose Reduction"), 40, replace=TRUE)
)

cliplot(Duration ~ ID, data = pt_data, type = "swimmer")
} # }
```
