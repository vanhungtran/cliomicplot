# Deconvolution Identifiability Plot Type

Creates publication-ready deconvolution identifiability plots that
explain why specific cell types are detected versus zeroed in bulk
tissue deconvolution. Combines marker gene counts (bars) on the primary
axis with pairwise collinearity metrics (triangles) on a secondary axis.

## Usage

``` r
type_identifiability(
  collinearity = NULL,
  status = NULL,
  flag = NULL,
  max_marker_scale = NULL,
  bar_width = 0.62,
  point_size = 2.8,
  status_colors = c(predicted = "#2E8B57", zeroed = "#C0C0C0"),
  flag_colors = c(identifiable = "#2166AC", collinear = "#B2182B", `few markers` =
    "#F4A582", `few markers + collinear` = "#7B3294"),
  ...
)
```

## Arguments

- collinearity:

  Column name or vector for maximum collinearity correlation (Pearson r
  in \[0, 1\]). Default looks for `"max_corr"`.

- status:

  Column name or vector for deconvolution status (e.g. `"predicted"` vs
  `"zeroed"`). Default looks for `"status"`.

- flag:

  Column name or vector for identifiability diagnostic flag (e.g.
  `"identifiable"`, `"collinear"`, `"few markers"`,
  `"few markers + collinear"`). Default looks for `"note"` or `"flag"`.

- max_marker_scale:

  Numeric scaling factor for secondary axis transformation (default
  auto-detected from maximum marker count).

- bar_width:

  Numeric bar width (default 0.62).

- point_size:

  Numeric point size for collinearity glyphs (default 2.8).

- status_colors:

  Named vector of fill colors for deconvolution status (default:
  predicted = `"#2E8B57"`, zeroed = `"#C0C0C0"`).

- flag_colors:

  Named vector of colors for identifiability diagnostic flags (default:
  identifiable = `"#2166AC"`, collinear = `"#B2182B"`, few markers =
  `"#F4A582"`, few markers + collinear = `"#7B3294"`).

- ...:

  Additional arguments.

## Value

A `cliplot_type` object for use with
[`cliplot`](https://vanhungtran.github.io/cliomicplot/reference/cliplot.md).

## Examples

``` r
if (FALSE) { # \dontrun{
df_ident = data.frame(
  cell_type = c("T_cell", "B_cell", "Macrophage", "Keratinocyte"),
  n_markers = c(13, 3, 42, 32),
  max_corr  = c(0.57, 0.90, 0.77, 0.46),
  status    = c("predicted", "predicted", "predicted", "zeroed"),
  note      = c("identifiable", "few markers + collinear", "collinear", "identifiable")
)
cliplot(n_markers ~ cell_type, data = df_ident, type = type_identifiability())
} # }
```
