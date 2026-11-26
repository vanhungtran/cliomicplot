# Triangular Correlation Heatmap with Curved Mantel Links

Creates a publication-grade lower-triangular correlation matrix of
environmental or clinical variables connected via smooth bezier arcs to
microbial community taxa, diversity metrics, or multi-omics modules (Wen
et al. iMeta 2025).

## Usage

``` r
type_mantel_triangle(
  env_cor = NULL,
  env_data = NULL,
  comm_data = NULL,
  mantel_res = NULL,
  cor_method = c("spearman", "pearson", "kendall"),
  dist_comm = "bray",
  dist_env = "euclidean",
  p_cutoff = 0.05,
  r_cutoff = 0,
  show_cor_values = TRUE,
  show_sig_stars = TRUE,
  link_palette = NULL,
  link_width_range = c(0.4, 2.2),
  tile_palette = "RdBu",
  node_size = 4,
  label_size = 3,
  curve_curvature = 0.25
)
```

## Arguments

- env_cor:

  Optional square numeric correlation matrix or data frame of
  environmental variables. If NULL, correlation is computed from
  `env_data`.

- env_data:

  Optional data frame or matrix of environmental/clinical variables
  (samples in rows, variables in columns).

- comm_data:

  Optional data frame or matrix of community abundances or multi-omics
  profiles (samples in rows, features in columns).

- mantel_res:

  Optional data frame of pre-computed Mantel test results with columns
  `spec` (community feature), `env` (environmental variable), `r`
  (correlation magnitude), and `p` (p-value).

- cor_method:

  Correlation method for environmental variables: `"pearson"`,
  `"spearman"`, or `"kendall"`.

- dist_comm:

  Dissimilarity metric for community data if computing Mantel tests:
  `"bray"`, `"jaccard"`, or `"euclidean"`.

- dist_env:

  Distance metric for environmental variables: `"euclidean"` or
  `"manhattan"`.

- p_cutoff:

  Numeric significance threshold for drawing links (default 0.05).

- r_cutoff:

  Numeric minimum Mantel r magnitude to display (default 0.0).

- show_cor_values:

  Logical. If TRUE (default), displays correlation values inside
  triangular tiles.

- show_sig_stars:

  Logical. If TRUE, appends significance asterisks to correlation values
  in tiles.

- link_palette:

  Color palette or vector for Mantel p-values (default
  `c("< 0.001" = "#E64B35", "< 0.01" = "#4DBBD5", "< 0.05" = "#00A087", ">= 0.05" = "#B0B5BC")`).

- link_width_range:

  Range of line widths for Mantel r values (default `c(0.4, 2.2)`).

- tile_palette:

  Color palette for the environmental correlation matrix (default
  `"RdYlBu"` or `"coolwarm"`).

- node_size:

  Numeric point size for community nodes along the margin (default 4.0).

- label_size:

  Numeric font size for variable and community labels (default 3.0).

- curve_curvature:

  Numeric curvature for bezier links (default 0.25).

## Value

A `cliplot_type` object for use with
[`cliplot`](https://vanhungtran.github.io/cliomicplot/reference/cliplot.md).

## Examples

``` r
if (FALSE) { # \dontrun{
# With precomputed mantel results:
cliplot(type = type_mantel_triangle(env_data = env_df, mantel_res = mantel_df))
} # }
```
