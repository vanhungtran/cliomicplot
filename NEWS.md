# cliomicplot 0.1.0

Initial development release.

## Features

* `cliplot()` plotting engine with formula, data.frame, matrix, and default
  methods, built on ggplot2.
* 40+ plot types spanning clinical (volcano, forest, Kaplan-Meier, waterfall,
  swimmer), omics (heatmap, PCA/MDS, MA, correlation, boxplot, violin), and
  general/creative visualizations.
* Journal-specific themes (`nature`, `science`, `nejm`, `lancet`, `cell`, ...)
  via `clitheme()`, plus custom theme registration.
* 55+ curated color palettes, including colorblind-safe options, exposed as
  drop-in ggplot2 scales.
* Built-in statistical annotations (`stat.test`) and markdown text rendering.

## Statistical safeguards

* Volcano plots now document the correct `-log10(p) ~ logFC` formula
  orientation; use an FDR-adjusted p-value column for the significance axis.
* `stat.test` emits a warning when >2-group pairwise comparisons are shown
  without multiple-testing correction, and warns (instead of silently doing
  nothing) when the x-axis is continuous, no grouping variable is present, or
  fewer than two groups exist.
