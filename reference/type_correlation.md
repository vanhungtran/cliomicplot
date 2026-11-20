# Correlation Matrix Plot Type

Creates publication-ready correlation matrix plots with customizable
color scales, significance indicators, clustering, and adaptive text and
border scaling.

## Usage

``` r
type_correlation(
  method = "pearson",
  type = "full",
  add_coef = TRUE,
  coef_size = 3.5,
  color_low = "#4575B4",
  color_mid = "white",
  color_high = "#D73027",
  cluster = FALSE,
  show_diag = FALSE,
  sig_level = 0.05,
  axis_text_size = NULL,
  axis_text_angle = 45,
  show_axis_text = TRUE,
  tile_border_color = "white",
  tile_border_width = NULL,
  legend_title = "Correlation",
  legend_barheight = 55,
  legend_barwidth = 8
)
```

## Arguments

- method:

  Correlation method: "pearson", "spearman", "kendall"

- type:

  Visualization type: "full", "upper", "lower"

- add_coef:

  Add correlation coefficients to cells (default TRUE)

- coef_size:

  Size of coefficient text (default 3.5)

- color_low:

  Low end of color gradient (default "#4575B4")

- color_mid:

  Midpoint color (default "white")

- color_high:

  High end of color gradient (default "#D73027")

- cluster:

  Cluster variables (default FALSE)

- show_diag:

  Show diagonal (default FALSE)

- sig_level:

  Significance level for stars (default 0.05)

- axis_text_size:

  Size of axis labels. If NULL (default), adaptively scaled based on
  number of variables (e.g. 9pt for \<=8 vars down to 4.5pt for 50
  vars).

- axis_text_angle:

  Angle for x-axis labels (default 45).

- show_axis_text:

  Logical indicating whether to display axis labels (default TRUE).

- tile_border_color:

  Color of tile borders (default "white"). Set to NA or NULL for no
  border.

- tile_border_width:

  Linewidth of tile borders. If NULL (default), adaptively scaled from
  0.8 for \<=15 variables down to 0.18 for 50 variables.

- legend_title:

  Legend title for colorbar (default "Correlation").

- legend_barheight:

  Height of colorbar (default 55, or a grid unit).

- legend_barwidth:

  Width of colorbar (default 8, or a grid unit).

## Value

A cliplot_type object for use with
[`cliplot`](https://vanhungtran.github.io/cliomicplot/reference/cliplot.md).

## Examples

``` r
if (FALSE) { # \dontrun{
# Correlation of numeric columns
cliplot(mtcars, type = "correlation")

# Custom correlation
cliplot(mtcars, type = type_correlation(method = "spearman", type = "upper"))
} # }
```
