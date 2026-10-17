# Add a Panel Label (A, B, C ...) to a ggplot

Adds a bold panel label in the top-left corner of a ggplot, following
the convention used in Nature/Cell/Science multi-panel figures. Returns
a ggplot layer so it composes with `+`.

## Usage

``` r
cli_panel_label(
  label,
  x = NULL,
  y = NULL,
  hjust = NULL,
  vjust = NULL,
  size = 3.2,
  fontface = "bold",
  color = "black",
  family = "",
  ...
)
```

## Arguments

- label:

  Panel label, e.g. `"A"`, `"B"`, or `"Fig. 1a"`.

- x, y:

  Position of the label. Defaults to the top-left corner of the plotting
  region (`-Inf`, `Inf`).

- hjust, vjust:

  Justification of the label.

- size:

  Label size in ggplot units (default 3.2).

- fontface:

  Font face (default "bold").

- color:

  Label color (default "black").

- family:

  Font family.

- ...:

  Additional arguments passed to
  [`ggplot2::annotate()`](https://ggplot2.tidyverse.org/reference/annotate.html).

## Value

A ggplot layer (from
[`ggplot2::annotate()`](https://ggplot2.tidyverse.org/reference/annotate.html)).

## Examples

``` r
if (FALSE) { # \dontrun{
p1 = cliplot(mpg ~ wt, data = mtcars, theme = "nature") + cli_panel_label("A")
p2 = cliplot(~ cyl, data = mtcars, type = "barplot", theme = "nature") + cli_panel_label("B")
} # }
```
