# Publication-Ready Plots for Clinical and Multi-Omics Data

Main plotting function for creating publication-ready visualizations.
Supports a formula interface with automatic grouping, faceting, and
journal-specific themes. Built on ggplot2.

## Usage

``` r
cliplot(x, ...)

# Default S3 method
cliplot(
  x = NULL,
  y = NULL,
  ...,
  data = NULL,
  type = NULL,
  by = NULL,
  facet = NULL,
  palette = NULL,
  theme = NULL,
  title = NULL,
  subtitle = NULL,
  caption = NULL,
  xlab = NULL,
  ylab = NULL,
  legend = NULL,
  add = FALSE,
  stat.test = NULL,
  stat.label = NULL,
  file = NULL,
  width = NULL,
  height = NULL,
  km_time = NULL,
  km_event = NULL,
  km_group = NULL
)

# S3 method for class 'formula'
cliplot(
  x,
  data = NULL,
  ...,
  type = NULL,
  by = NULL,
  facet = NULL,
  palette = NULL,
  theme = NULL,
  title = NULL,
  subtitle = NULL,
  caption = NULL,
  xlab = NULL,
  ylab = NULL,
  legend = NULL,
  add = FALSE,
  stat.test = NULL,
  stat.label = NULL,
  file = NULL,
  width = NULL,
  height = NULL
)

# S3 method for class 'data.frame'
cliplot(
  x,
  ...,
  formula = NULL,
  type = NULL,
  by = NULL,
  facet = NULL,
  palette = NULL,
  theme = NULL,
  title = NULL,
  subtitle = NULL,
  caption = NULL,
  xlab = NULL,
  ylab = NULL,
  legend = NULL,
  stat.test = NULL,
  file = NULL,
  width = NULL,
  height = NULL
)

# S3 method for class 'matrix'
cliplot(
  x,
  ...,
  type = NULL,
  theme = NULL,
  title = NULL,
  subtitle = NULL,
  caption = NULL,
  file = NULL,
  width = NULL,
  height = NULL
)
```

## Arguments

- x, y:

  x and y variables. Can be vectors, data frame columns, or specified
  via formula.

- ...:

  Additional arguments passed to the plot type's draw function.

- data:

  A data.frame containing the variables.

- type:

  Plot type. Can be a character string (`"points"`, `"boxplot"`,
  `"violin"`, `"histogram"`, `"density"`, `"barplot"`, `"lines"`,
  `"volcano"`, `"forest"`, `"km"`, `"waterfall"`, `"swimmer"`,
  `"heatmap"`, `"pca"`, `"ma"`, `"correlation"`) or a call to a
  `type_*()` function for customization.

- by:

  Grouping variable. Alternative to the `|` in formula.

- facet:

  Faceting variable(s) as a formula. One-sided formulas (e.g. `~ group`)
  use `facet_wrap`. Two-sided formulas (e.g. `rows ~ cols`) or (e.g.
  `panel ~ logfc + fdr`) use `facet_grid`.

- palette:

  Color palette name or vector of colors. Built-in options: `"jco"`,
  `"nejm"`, `"lancet"`, `"nature"`, `"science"`, `"okabe_ito"`.

- theme:

  Theme name or ggplot2 theme object. See
  [`clitheme`](https://vanhungtran.github.io/cliomicplot/reference/clitheme.md).

- title:

  Plot title.

- subtitle:

  Plot subtitle.

- caption:

  Plot caption.

- xlab, ylab:

  Axis labels.

- legend:

  Legend position (`"right"`, `"left"`, `"top"`, `"bottom"`, `"none"`).

- add:

  Logical. If TRUE, adds to an existing cliplot. Not yet implemented.

- stat.test:

  Statistical test for comparisons. One of `"wilcox.test"`, `"t.test"`,
  `"kruskal.test"`, `"anova"`, `NULL`.

- stat.label:

  Format for statistical labels. `"p.format"`, `"p.signif"`, or
  `"p.adj"`.

- file:

  Optional file path to save the plot (pdf, png, jpg, svg).

- width, height:

  Plot dimensions in inches.

- km_time, km_event, km_group:

  Internal. Column names extracted from a `Surv()` formula for
  Kaplan-Meier plots; set automatically by the formula method and not
  intended to be supplied directly.

- formula:

  A formula of the form `y ~ x | group` or `~ x | group` for one-sided
  plots. The grouping variable after `|` is optional.

## Value

A ggplot object (invisibly).

## Examples

``` r
if (FALSE) { # \dontrun{
# Basic scatter plot
cliplot(mpg ~ wt, data = mtcars, title = "Mileage vs Weight")

# Grouped by variable with automatic legend
cliplot(Sepal.Length ~ Petal.Length | Species, data = iris)

# Box plot with statistical test
cliplot(Sepal.Length ~ Species, data = iris, type = "boxplot",
        stat.test = "kruskal.test", palette = "jco")

# Volcano plot (y ~ x: significance ~ fold-change)
cliplot(-log10(padj) ~ log2FC, data = deg_results, type = "volcano")

# Forest plot
cliplot(HR ~ Variable | Group, data = cox_results, type = "forest")

# Journal theme
cliplot(mpg ~ wt, data = mtcars, theme = "nature")
} # }
```
