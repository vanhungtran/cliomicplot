# cliomicplot: Publication-Ready Visualizations for Clinical and Multi-Omics Data

cliomicplot provides high-quality, publication-ready visualizations for
clinical and multi-omics data analysis. Built on ggplot2, it offers
specialized plot types and journal-specific themes.

## Design Philosophy

The API design draws on patterns established in the broader R graphics
ecosystem — including the formula interface popularized by base R's
[`plot()`](https://rdrr.io/r/graphics/plot.default.html) and the
type-system approach used in several modern graphics packages. Colour
palettes are curated from published journal style guides and open-source
bioinformatics tools; see the package REFERENCES for full attributions.

## Main Functions

- [`cliplot`](https://vanhungtran.github.io/cliomicplot/reference/cliplot.md) -
  Main plotting function

- [`cliplot_add`](https://vanhungtran.github.io/cliomicplot/reference/cliplot_add.md) -
  Add layers to existing plot

- [`clitheme`](https://vanhungtran.github.io/cliomicplot/reference/clitheme.md) -
  Set plot themes

- [`clipar`](https://vanhungtran.github.io/cliomicplot/reference/clipar.md) -
  Set/query graphical parameters

## Plot Types

- Clinical:
  [`type_volcano`](https://vanhungtran.github.io/cliomicplot/reference/type_volcano.md),
  [`type_forest`](https://vanhungtran.github.io/cliomicplot/reference/type_forest.md),
  [`type_km`](https://vanhungtran.github.io/cliomicplot/reference/type_km.md),
  [`type_waterfall`](https://vanhungtran.github.io/cliomicplot/reference/type_waterfall.md),
  [`type_swimmer`](https://vanhungtran.github.io/cliomicplot/reference/type_swimmer.md)

- Omics:
  [`type_heatmap`](https://vanhungtran.github.io/cliomicplot/reference/type_heatmap.md),
  [`type_pca`](https://vanhungtran.github.io/cliomicplot/reference/type_pca.md),
  [`type_ma`](https://vanhungtran.github.io/cliomicplot/reference/type_ma.md),
  [`type_correlation`](https://vanhungtran.github.io/cliomicplot/reference/type_correlation.md),
  [`type_boxplot`](https://vanhungtran.github.io/cliomicplot/reference/type_boxplot.md),
  [`type_violin`](https://vanhungtran.github.io/cliomicplot/reference/type_violin.md)

- Gallery:
  [`type_ridge`](https://vanhungtran.github.io/cliomicplot/reference/type_ridge.md),
  [`type_lm`](https://vanhungtran.github.io/cliomicplot/reference/type_lm.md),
  [`type_loess`](https://vanhungtran.github.io/cliomicplot/reference/type_loess.md),
  [`type_spineplot`](https://vanhungtran.github.io/cliomicplot/reference/type_spineplot.md),
  [`type_rug`](https://vanhungtran.github.io/cliomicplot/reference/type_rug.md),
  [`type_abline`](https://vanhungtran.github.io/cliomicplot/reference/type_abline.md),
  [`type_qq`](https://vanhungtran.github.io/cliomicplot/reference/type_qq.md)

- R Graph Gallery:
  [`type_chord`](https://vanhungtran.github.io/cliomicplot/reference/type_chord.md),
  [`type_treemap`](https://vanhungtran.github.io/cliomicplot/reference/type_treemap.md),
  [`type_streamgraph`](https://vanhungtran.github.io/cliomicplot/reference/type_streamgraph.md),
  [`type_connected`](https://vanhungtran.github.io/cliomicplot/reference/type_connected.md),
  [`type_circular_bar`](https://vanhungtran.github.io/cliomicplot/reference/type_circular_bar.md),
  [`type_density2d`](https://vanhungtran.github.io/cliomicplot/reference/type_density2d.md),
  [`type_parallel`](https://vanhungtran.github.io/cliomicplot/reference/type_parallel.md),
  [`type_dendrogram`](https://vanhungtran.github.io/cliomicplot/reference/type_dendrogram.md)

- Premier:
  [`type_raincloud`](https://vanhungtran.github.io/cliomicplot/reference/type_raincloud.md),
  [`type_dumbbell`](https://vanhungtran.github.io/cliomicplot/reference/type_dumbbell.md),
  [`type_lollipop`](https://vanhungtran.github.io/cliomicplot/reference/type_lollipop.md),
  [`type_beeswarm`](https://vanhungtran.github.io/cliomicplot/reference/type_beeswarm.md),
  [`type_radar`](https://vanhungtran.github.io/cliomicplot/reference/type_radar.md),
  [`type_alluvial`](https://vanhungtran.github.io/cliomicplot/reference/type_alluvial.md),
  [`type_waffle`](https://vanhungtran.github.io/cliomicplot/reference/type_waffle.md)

- Basic:
  [`type_points`](https://vanhungtran.github.io/cliomicplot/reference/type_points.md),
  [`type_jitter`](https://vanhungtran.github.io/cliomicplot/reference/type_jitter.md),
  [`type_barplot`](https://vanhungtran.github.io/cliomicplot/reference/type_barplot.md),
  [`type_lines`](https://vanhungtran.github.io/cliomicplot/reference/type_lines.md),
  [`type_histogram`](https://vanhungtran.github.io/cliomicplot/reference/type_histogram.md),
  [`type_density`](https://vanhungtran.github.io/cliomicplot/reference/type_density.md),
  [`type_errorbar`](https://vanhungtran.github.io/cliomicplot/reference/type_errorbar.md),
  [`type_ribbon`](https://vanhungtran.github.io/cliomicplot/reference/type_ribbon.md),
  [`type_text`](https://vanhungtran.github.io/cliomicplot/reference/type_text.md),
  [`type_bubble`](https://vanhungtran.github.io/cliomicplot/reference/type_bubble.md)

- Interactive:
  [`cliplot_interactive`](https://vanhungtran.github.io/cliomicplot/reference/cliplot_interactive.md)
  for plotly-powered zoom/pan/hover

- Infographic:
  [`type_trials`](https://vanhungtran.github.io/cliomicplot/reference/type_trials.md),
  [`type_infobar`](https://vanhungtran.github.io/cliomicplot/reference/type_infobar.md)
  for media-ready pipeline and polling-style charts

## See also

Useful links:

- <https://github.com/vanhungtrantran/cliomicplot>

- Report bugs at <https://github.com/vanhungtran/cliomicplot/issues>

## Author

**Maintainer**: Lucas VHH Tran <lucas.tran@ck-care.ch>
