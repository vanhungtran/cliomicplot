
# cliomicplot 0.1.4

## Clinical Figure Archetypes & Publisher Compliance

* **Minimal Clinically Important Difference (MCID) Dumbbell Plot (`type_mcid()`, Archetype 1)**:
  Distinguish genuine clinical relevance from large-cohort nominal significance ($P < 0.001$).
  Connects observed treatment effect sizes to validated instrument MCID thresholds (e.g. DLQI = 4.0, EASI = 6.6),
  with point glyphs scaled by sample size ($N_{\text{treated}}$) and endpoints ordered by effect-to-MCID ratio.

* **Longitudinal Clinical Trajectories with Severity Bands (`type_trajectory()`, Archetype 4)**:
  Visualizes patient-level clinical heterogeneity without spaghetti hairballs. Overlays semi-transparent
  individual trajectories onto shaded background clinical severity bands (Mild, Moderate, Severe), anchored
  by a bold group mean trajectory with confidence ribbon ($95\%$ CI / SE) and visit sample-size annotations ($n = \dots$).

* **Zero-Dependency Native KM & Cumulative Incidence (`type_km()`, Archetype 5)**:
  Completely removed the hard dependency on `survminer`. Driven by a native `ggplot2` + `survival` engine
  supporting cumulative incidence ($[1 - S(t)] \times 100\%$), landmark milestone callouts (`milestones = c(...)`)
  with exact percentages directly on the curve, aligned numbers-at-risk tables, median survival crosshairs,
  and log-rank $p$-values.

* **Clinical Oncology Swimmer Plot Enhancements (`type_swimmer()`, Archetype 13)**:
  Added `ongoing` therapy indicator rendering arrowheads at the ends of timeline bars for patients remaining
  on therapy at database lock. Unified clinical event shape/color contracts and added duration annotations.

* **Publication Forest Plot Enhancements (`type_forest()`, Archetype 9)**:
  Added alternating background row shading stripes (`stripe = TRUE`) to eliminate horizontal eye drift across wide canvases,
  unclipped confidence interval labels, zero-magnitude negative sign suppression (`-0.00` rounded to `0.00`),
  and meta-analysis pooled summary diamonds (`diamond = TRUE`).

* **SCI Full-Box Theme (`theme_cli_scifig()`, `clitheme("scifig")`)**:
  New publication theme adhering to SCI / Elsevier / IEEE journal visual requirements: 4 visible black spines,
  outward-facing tick marks, and subtle dashed reference grid.

* **Expanded Publisher Presets & Compliance Checks (`cli_journal_size()`, `cli_save()`)**:
  Expanded to 9 verified publisher presets (Nature, Nature Communications, Science, Cell, JAMA, NEJM, The Lancet, PLOS, IEEE).
  Added arithmetic printable height verification, support for numeric columns (`1`, `1.5`, `2`), and submission format guidance (warning when raster PNG is supplied for Nature/Science final manuscripts).

* **Exported Standalone Theme Functions**:
  Exported first-class ggplot2 themes `theme_cli_nature()`, `theme_cli_science()`, `theme_cli_cell()`,
  `theme_cli_nejm()`, `theme_cli_lancet()`, `theme_cli_scifig()`, `theme_cli_bw()`, `theme_cli_classic()`,
  `theme_cli_minimal()`, `theme_cli_dark()`, `theme_cli_showcase()`, `theme_cli_broadsheet()`.

* **Complete Vignette Overhaul (Nature, Cell, Science Figure Design Language)**:
  - `journal-plots`: Showcases CNS design philosophy, isometric ROC curves, MCID dumbbells, trajectories with severity bands, KM milestone callouts, swimmer plots with ongoing therapy arrows, striped forest plots with summary diamonds, GSEA, UpSet, SCI full-box frames, and exact mm export.
  - `oncology`: Full end-to-end trial workflow featuring landmark milestones, cumulative incidence, RECIST waterfall and ongoing swimmer, MCID evaluation, and multi-panel composite assembly.
  - `themes-palettes`: Cataloging all 13 themes, open vs full-box frame guidelines, colorblind accessibility (Okabe-Ito, Paul Tol), and publisher canvas sizing.
  - `cliomicplot` & `volcano-plot`: Updated with modern clinical archetypes, two-tier volcano thresholds, and publication-ready export workflows.

# cliomicplot 0.1.3

## Recreating a published paper (SpaMTP, Nature Methods 2026)

* New `recreate-spamtp` vignette: regenerates key figures of the SpaMTP
  manuscript "SpaMTP: a framework for spatial metabolomics data integration
  and analysis" (DOI: 10.1038/s41592-026-03140-8) from the paper's public
  datasets (mouse urinary bladder MALDI-MSI, Parkinson mouse brain
  Visium + MSI, human brain tumour pathways, spotted mouse liver).
* New reproducible build script `inst/extdata/build_spamtp_figures.R`;
  four 300-dpi Nature-style composites added to the README gallery
  (`man/figures/spamtp_fig{1..4}.png`): segmentation maps, spatial
  metabolite intensities, DEM/DEG/NES heatmaps, volcanoes, Visium PCA,
  pathway dot/bar plots and annotation QC.
* Processed datasets (CC-BY-4.0) are fetched from the SpaMTP Zenodo
  community on first run and cached as small analysis bundles at
  `inst/extdata/spamtp_paper/`.

## Robust ComplexHeatmap embedding

* `type_heatmap()` now embeds ComplexHeatmap output as a high-resolution
  raster instead of a captured grob. Captured grobs were silently dropped by
  `patchwork` composites when a neighbouring panel used a non-void theme,
  which blanked out heatmap tiles in multi-panel figures. All paper-figure
  heatmaps (SLE + SpaMTP) were regenerated with the fix.
* `png` added to `Suggests` (used to rasterize the ComplexHeatmap output).
# cliomicplot 0.1.2

## Recreating a published paper (Soares et al. 2025, Biology of Sex Differences)

* New `recreate-paper` vignette: regenerates the four main figures of
  "X-linked transcriptome dysregulation across immune cells in systemic lupus
  erythematosus" (DOI: 10.1186/s13293-025-00750-3) from the paper's own
  supplementary tables, bundled at `inst/extdata/sle_paper/`.
* New reproducible build script `inst/extdata/build_sle_figures.R`; four
  300-dpi Nature-style composite figures added to the README gallery
  (`man/figures/sle_fig{1..4}.png`): X/A-ratio and XIST boxplots, volcano
  small multiples, mean-logFC and NES heatmaps, DIABLO F1/marker barplots,
  X/A-correlation heatmaps, and intercellular-correlation diverging bars.
* `type_heatmap()` gains a `legend_title` argument so the color-bar title can
  be set to e.g. "NES" or "logFC" instead of the default "Z-score".

# cliomicplot 0.1.1

## New cnsplots-inspired plot types

* `type_roc()` - ROC curves with AUC in the legend; compare multiple
  biomarkers/predictors side by side (`rocplot` in cnsplots).
* `type_gsea()` - GSEA-style enrichment dot plots ranking terms by an
  enrichment score, with FDR encoded in dot size and direction in color
  (`gseaplot` in cnsplots).
* `type_upset()` - ggplot2-based UpSet plots for set intersections; accepts a
  named list of sets or a binary membership data frame (`upsetplot`).
* `type_venn()` - Venn diagrams for 1-3 sets, built with ggplot2
  (`vennplot`).
* `type_slope()` - slope plots / slopegraphs for paired before-after
  comparisons (`slopeplot`).
* `type_confusion()` - confusion-matrix heatmaps with accuracy and Fisher
  exact test p-values (`confusionplot`).
* `type_donut()` - donut charts with optional center summary labels
  (`donutplot`).

## Publication-quality helpers

* `cli_panel_label()` - bold A/B/C panel labels for multi-panel figures.
* `cli_legend_out()` - move legends outside the plotting area.
* `cli_journal_size()` - Nature/Science/Cell single-, 1.5-, and double-column
  figure widths in mm and inches.
* `cli_save()` - save figures at journal-reference widths and print DPI.

## Vignettes

* New `journal-plots` vignette: Nature/Cell/Science-quality figures
  (ROC, GSEA dot plot, UpSet, Venn, slope, confusion, donut) built from the
  bundled AD/PD proteomics, stroke time-course, and aging metabolomics data.

## Nature-style figure gallery

* All seven journal-style gallery figures regenerated at 300 dpi with
  Nature/Science/Cell typography: no in-figure titles, bold lowercase panel
  labels, black text on white, and thin black axes.
* New reproducible build script `inst/extdata/build_journal_figures.R`.

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
