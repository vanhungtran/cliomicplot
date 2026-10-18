# Rank Abundance Curve Type

Creates a rank-abundance curve that sorts observations by
abundance/intensity and plots them against rank. Supports marginal
distributions (density, histogram, boxplot, violin) on the y-axis,
inspired by xOmicsShiny's expression module.

## Usage

``` r
type_rankabundance(
  log_scale = "10",
  point_color = "#4DBBD5",
  point_size = 1.8,
  point_alpha = 0.7,
  marginal_type = c("density", "histogram", "boxplot", "violin", "densigram", "none"),
  marginal_color = "#3C5488",
  label_genes = NULL,
  label_color = "#E64B35",
  label_size = 3.5
)
```

## Arguments

- log_scale:

  Log-transform the y-axis ("none", "10", or "2"; default "10")

- point_color:

  Point colour (default "#4DBBD5")

- point_size:

  Point size (default 1.8)

- point_alpha:

  Point transparency (default 0.7)

- marginal_type:

  Marginal plot type: "density", "histogram", "boxplot", "violin",
  "densigram", or "none" (default "density")

- marginal_color:

  Fill colour for marginal distribution

- label_genes:

  Character vector of gene names to highlight and label, or NULL for no
  labels

- label_color:

  Label colour for highlighted points (default "#E64B35")

- label_size:

  Label text size (default 3.5)

## Value

A `cliplot_type` object for use with
[`cliplot`](https://vanhungtran.github.io/cliomicplot/reference/cliplot.md).

## Details

A rank-abundance curve sorts all features (genes, proteins, metabolites)
by their abundance and displays them against rank. It is a standard QC
and exploratory plot in omics analysis. The y-axis typically shows
log10-transformed intensity values. Marginal distributions help assess
the overall abundance distribution shape.

## Examples

``` r
if (FALSE) { # \dontrun{
# Basic rank-abundance curve from expression data
cliplot(Intensity ~ RANK, data = expr_data, type = "rankabundance")

# With gene labels and marginal density
cliplot(Intensity ~ RANK, data = expr_data,
        type = type_rankabundance(
          label_genes = c("TNF", "IL6", "TP53"),
          marginal_type = "density"))
} # }
```
