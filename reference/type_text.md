# Text/Label Plot Type

Adds text labels at (x, y) positions. Supports both plain text and
label-style (with background rectangle). Useful for volcano plots,
scatter plot annotations, and point labeling.

## Usage

``` r
type_text(
  labels = NULL,
  size = 3.5,
  alpha = 0.9,
  style = c("text", "label"),
  repel = TRUE,
  label.padding = 0.25,
  label.size = 0.25
)
```

## Arguments

- labels:

  Character vector of text labels, or column name in data

- size:

  Text size (default 3.5)

- alpha:

  Text transparency

- style:

  "text" for plain text or "label" for text with background box

- repel:

  Use ggrepel to avoid overlapping (default TRUE)

- label.padding:

  Padding around label text when style="label"

- label.size:

  Border size for label boxes

## Value

A cliplot_type object for use with
[`cliplot`](https://vanhungtran.github.io/cliomicplot/reference/cliplot.md).

## Examples

``` r
if (FALSE) { # \dontrun{
# Label top points in a scatter
top_cars <- head(mtcars[order(-mtcars$mpg), ], 5)
cliplot(mpg ~ wt, data = mtcars, type = "points")
cliplot(mpg ~ wt, data = top_cars, type = "text", add = TRUE)

# Label with repel
cliplot(mpg ~ wt, data = mtcars,
        type = type_text(style = "label", repel = TRUE))
} # }
```
