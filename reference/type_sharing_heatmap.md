# Longitudinal Transmission and Dyad Sharing Heatmap

Creates publication-ready discrete sharing heatmaps (Clinical Archetype
28) to display bacterial strain transmission across mother-infant dyads,
clinical (e.g. baseline infancy T1 versus 12 months T3).

## Usage

``` r
type_sharing_heatmap(
  status_col = NULL,
  shared_label = "Shared",
  shared_color = "#E64B35",
  unshared_color = "#F0F3F8",
  na_color = "white",
  border_color = "#DCE2EC",
  border_size = 0.25,
  legend_title = "Dyad Status",
  xlab = NULL,
  ylab = NULL
)
```

## Arguments

- status_col:

  Column name specifying the binary or categorical sharing status (e.g.
  "Shared", "Unique", "Persistent").

- shared_label:

  Label indicating sharing or transmission (default `"Shared"`).

- shared_color:

  Color for shared or transmitted state (default `"#E64B35"`).

- unshared_color:

  Color for unshared state (default `"#F0F3F8"`).

- na_color:

  Color for missing or unmeasured cells (default `"white"`).

- border_color:

  Cell outline color (default `"#DCE2EC"`).

- border_size:

  Cell outline stroke width (default `0.25`).

- legend_title:

  Title for the legend (default `"Transmission"`).

- xlab, ylab:

  Axis labels.

## Value

A `cliplot_type` object for use with
[`cliplot`](https://vanhungtran.github.io/cliomicplot/reference/cliplot.md).

## Examples

``` r
if (FALSE) { # \dontrun{
# Simulated mother-infant dyad sharing data
set.seed(42)
dyad_df <- expand.grid(
  Species = c("S. epidermidis", "S. hominis", "C. acnes", "M. restricta"),
  Dyad = paste0("Dyad_", 1:12)
)
dyad_df$Status <- ifelse(runif(nrow(dyad_df)) > 0.45, "Shared", "Not shared")

cliplot(Species ~ Dyad, data = dyad_df,
        type = type_sharing_heatmap(status_col = "Status"))
} # }
```
