# Cellular & Taxonomic Lineage Composition Stacked Bars with Clinical Track

Creates publication-ready cellular or taxonomic lineage composition
stacked bar charts (Clinical Archetype 37) with an integrated bottom
clinical phenotype track (e.g. disease severity, treatment cohort, or
responder status).

Addresses the classic ggplot2 single-fill limitation by mapping lineage
fractions to stacked bar fill while rendering the clinical metadata
track via discrete colored segments, allowing both legends to coexist
harmoniously.

## Usage

``` r
type_composition(
  sample_col = NULL,
  lineage_col = NULL,
  share_col = NULL,
  track_col = NULL,
  facet_col = NULL,
  track_y = -0.05,
  track_linewidth = 2.4,
  palette = "nature",
  track_palette = NULL,
  lineage_name = "Cell Lineage",
  track_name = "Clinical Group",
  show_percentages = TRUE,
  reorder_by_track = TRUE,
  xlab = NULL,
  ylab = NULL
)
```

## Arguments

- sample_col:

  Column name for sample or library identifiers.

- lineage_col:

  Column name for cellular lineages or taxa.

- share_col:

  Column name for fractional proportions (0 to 1) or abundance.

- track_col:

  Column name for clinical group, disease stage, or treatment arm.

- facet_col:

  Column name for dataset or cohort faceting.

- track_y:

  Vertical position of the clinical phenotype strip below baseline
  (default `-0.05`).

- track_linewidth:

  Thickness of the clinical track segment (default 2.4).

- palette:

  Discrete color palette name or vector of colors for cell lineages
  (default `"nature"`).

- track_palette:

  Discrete color vector or palette name for clinical groups.

- lineage_name:

  Legend title for cell lineages (default `"Cell Lineage"`).

- track_name:

  Legend title for clinical phenotype track (default
  `"Clinical Group"`).

- show_percentages:

  Logical. If `TRUE` (default), formats y-axis as percentages (0% to
  100%).

- reorder_by_track:

  Logical. If `TRUE` (default), orders samples along x-axis by clinical
  group then sample ID.

- xlab, ylab:

  Axis labels.

## Value

A `cliplot_type` object for use with
[`cliplot`](https://vanhungtran.github.io/cliomicplot/reference/cliplot.md).

## Examples

``` r
if (FALSE) { # \dontrun{
library(cliomicplot)
data(immune_cell_composition)

# Lineage composition with clinical phenotype track
cliplot(
  share ~ gsm,
  data = immune_cell_composition,
  type = type_composition(
    lineage_col = "lineage",
    track_col = "group",
    facet_col = "dataset"
  ),
  title = "Single-Cell Leukocyte Lineage Composition Across Allergic Cohorts"
)
} # }
```
