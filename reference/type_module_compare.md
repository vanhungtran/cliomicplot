# Cross-Network Module Comparison Heatmap

Creates publication-ready cross-network module comparison plots to
evaluate module preservation, overlap, and reorganization across
distinct biological conditions, cohorts, timepoints, or tissues (e.g.,
Discovery vs Replication, Health vs Disease, Infant vs Mother, or
Microbiome vs Host). Evaluates module correspondence using
hypergeometric test statistics, Jaccard similarity indices,
representation factors (fold enrichment), and raw overlap counts.

## Usage

``` r
type_module_compare(
  fill_by = c("log10p", "jaccard", "overlap", "rf"),
  show_counts = TRUE,
  show_p = FALSE,
  show_stars = TRUE,
  text_size = 3.2,
  text_color = "auto",
  colors = c("#F7FBFF", "#9ECAE1", "#2171B5", "#08306B"),
  na_color = "white",
  border_color = "#DCE2EC",
  border_size = 0.3,
  legend_title = NULL,
  show_totals = TRUE,
  total_universe = NULL,
  min_overlap = 0,
  xlab = NULL,
  ylab = NULL
)
```

## Arguments

- fill_by:

  Metric used for tile fill color: `"log10p"` (-\\\log\_{10}(p)\\ from
  hypergeometric test, default), `"jaccard"` (Jaccard similarity index),
  `"overlap"` (count of shared nodes), or `"rf"` (representation factor
  / fold enrichment).

- show_counts:

  Logical. If TRUE, displays the number of shared features in each cell.
  Default TRUE.

- show_p:

  Logical. If TRUE, displays the hypergeometric p-value (or -log10 p) in
  each cell. Default FALSE.

- show_stars:

  Logical. If TRUE, appends significance asterisks (\* p \< 0.05, \*\* p
  \< 0.01, \*\*\* p \< 0.001). Default TRUE.

- text_size:

  Numeric text size for cell annotations (default 3.2).

- text_color:

  Character color for cell annotations. If "auto" (default), chooses
  white for dark tiles and dark grey for light tiles.

- colors:

  Sequential or gradient color vector for fill scale. Default
  `c("#F7FBFF", "#9ECAE1", "#2171B5", "#08306B")`.

- na_color:

  Background color for cells with zero overlap (default `"white"`).

- border_color:

  Outline color for tiles (default `"#DCE2EC"`).

- border_size:

  Outline stroke width (default `0.3`).

- legend_title:

  Legend title string (default depends on `fill_by`).

- show_totals:

  Logical. If TRUE, appends module member counts to axis labels (e.g.
  "M1 (n=45)"). Default TRUE.

- total_universe:

  Optional integer specifying the total shared feature universe size N
  for the hypergeometric test. If NULL, calculated automatically from
  unique features in the input data.

- min_overlap:

  Minimum overlap count required to display text label (default 0).

- xlab, ylab:

  Axis labels.

## Value

A `cliplot_type` object for use with
[`cliplot`](https://vanhungtran.github.io/cliomicplot/reference/cliplot.md).

## Examples

``` r
if (FALSE) { # \dontrun{
# Simulated node-level network module assignments
set.seed(42)
genes <- paste0("Gene_", 1:120)
net1 <- sample(c("M1_Inflam", "M2_Barrier", "M3_Lipid"), 120, replace = TRUE)
net2 <- net1
idx <- sample(1:120, 35)
net2[idx] <- sample(c("ModA_Th2", "ModB_Stratum", "ModC_Sebum"), 35, replace = TRUE)

df_nodes <- data.frame(Discovery = net1, Replication = net2)
cliplot(Replication ~ Discovery, data = df_nodes, type = type_module_compare())
} # }
```
