# Save a Figure at Journal-Reference Dimensions

Saves a ggplot at the exact width (and optional height) required by
Nature, Nature Communications, Science, Cell, JAMA, NEJM, Lancet, PLOS,
or IEEE at print-resolution DPI. Emits publisher format compliance
guidance and height checks so submission figures pass publisher gates.

## Usage

``` r
cli_save(
  plot = ggplot2::last_plot(),
  file = "figure.png",
  journal = c("nature", "natcomms", "science", "cell", "jama", "nejm", "lancet", "plos",
    "ieee"),
  column = c("single", "one_half", "double", "full"),
  width = NULL,
  height = NULL,
  dpi = 300,
  units = "in",
  ...
)
```

## Arguments

- plot:

  A ggplot object (defaults to the last plot).

- file:

  Output file path; the extension determines the format (pdf, png, svg,
  jpg, tiff).

- journal:

  One of `"nature"`, `"natcomms"`, `"science"`, `"cell"`, `"jama"`,
  `"nejm"`, `"lancet"`, `"plos"`, `"ieee"`. Ignored when `width` is
  supplied directly.

- column:

  One of `"single"`, `"one_half"`, `"double"`, `"full"`.

- width, height:

  Plot dimensions in inches (or in `units`). When `NULL`, derived from
  the journal reference specifications.

- dpi:

  Resolution in dots per inch (default 300; use 600 for submission-ready
  line art).

- units:

  Units for `width`/`height` (default "in").

- ...:

  Additional arguments passed to
  [`ggplot2::ggsave()`](https://ggplot2.tidyverse.org/reference/ggsave.html).

## Value

Invisibly returns the file path.

## Examples

``` r
if (FALSE) { # \dontrun{
p = cliplot(mpg ~ wt, data = mtcars, theme = "nature")
cli_save(p, "figure1.pdf", journal = "nature", column = "double", dpi = 600)
} # }
```
