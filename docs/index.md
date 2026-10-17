# cliomicplot

# **cliomicplot** _(*Publication-Ready Clinical & Multi-Omics Visualizations*)

------------------------------------------------------------------------

## âš¡ 30 Seconds

``` r

# install.packages("remotes")
# remotes::install_github("vanhungtrantran/cliomicplot")
library(cliomicplot)

# One line = a publication-ready figure
cliplot(Surv(time, status) ~ sex, data = lung,
        type = "km", theme = "nejm", palette = "jama",
        title = "**Overall Survival** by Sex") +
  cli_markdown()
```

That’s it. No
[`ggplot()`](https://ggplot2.tidyverse.org/reference/ggplot.html)
boilerplate, no manual
[`theme()`](https://ggplot2.tidyverse.org/reference/theme.html)
tweaking, no
[`ggsave()`](https://ggplot2.tidyverse.org/reference/ggsave.html) dance.

------------------------------------------------------------------------

## Why cliomicplot?

**Raw ggplot2** for a grouped boxplot with stats:

``` r

library(ggplot2)
library(ggpubr)
ggplot(ToothGrowth, aes(dose, len, fill = factor(dose))) +
  geom_boxplot() +
  stat_compare_means(method = "t.test", label = "p.format") +
  scale_fill_jco() +
  theme_classic() +
  labs(title = "Tooth Length by Dose", x = "Dose (mg)", y = "Length (mm)") +
  theme(legend.position = "none")
```

**cliomicplot:**

``` r

cliplot(len ~ dose, data = ToothGrowth,
        type = "boxplot", palette = "jco",
        stat.test = "t.test", title = "Tooth Length by Dose")
```

> **~80% less code.** Formula interface, auto-legends, journal themes,
> and statistical annotations built in.

| What you get for free | Base ggplot2 | cliomicplot |
|----|----|----|
| Formula interface (`y ~ x \| group`) | â€” | âœ… |
| Auto-legend + auto-facet | Manual | âœ… |
| Statistical annotations | Needs `ggpubr` | âœ… Built-in |
| Journal themes (Nature, NEJM, etc.) | Manual tweaking | âœ… `theme = "nejm"` |
| 55+ colorblind-safe palettes | Manual selection | âœ… `palette = "jco"` |
| Markdown in titles/labels | Needs `ggtext` | âœ… [`cli_markdown()`](https://vanhungtran.github.io/cliomicplot/reference/cli_markdown.md) |
| Save to file | Separate [`ggsave()`](https://ggplot2.tidyverse.org/reference/ggsave.html) | âœ… `file = "plot.pdf"` |
| Persistent global theme | [`theme_set()`](https://ggplot2.tidyverse.org/reference/get_theme.html) | âœ… `clitheme("nature")` |

------------------------------------------------------------------------

## ðŸ“¦ Installation

``` r

# From GitHub
remotes::install_github("vanhungtrantran/cliomicplot")

# Dependencies (installed automatically, but pre-install if needed)
install.packages(c("ggplot2", "scales", "survival", "reshape2", "ggrepel", "ggtext"))

# Optional: richer heatmaps (ComplexHeatmap) and extended survival plotting (survminer)
install.packages(c("ComplexHeatmap", "survminer", "ggpubr", "patchwork"))
```

------------------------------------------------------------------------

## ðŸ§© The Formula API

cliomicplot uses `y ~ x | group` â€” the same convention you already
know.

| Formula                      | What it does                       |
|------------------------------|------------------------------------|
| `y ~ x`                      | y vs. x (scatter/line)             |
| `y ~ x \| group`             | Grouped, auto-legend, auto-palette |
| `~ x \| group`               | One-sided distributional plot      |
| `Surv(time, status) ~ group` | Kaplan-Meier survival              |
| `HR ~ Variable \| Subgroup`  | Forest plot                        |

**All arguments in one call:**

| Argument | Purpose | Example |
|----|----|----|
| `type` | Plot type | `"boxplot"`, `type_km(risk_table = TRUE)` |
| `palette` | Color palette | `"jco"`, `"nejm"`, `"cosmic"` |
| `theme` | Theme | `"nature"`, `"dark"`, `"broadsheet"` |
| `facet` | Faceting formula | `facet = ~ dose` |
| `stat.test` | Statistical test | `"wilcox.test"`, `"anova"` |
| `file` | Save to file | `"figure1.pdf"` |
| `width`, `height` | Output dimensions | `width = 8`, `height = 6` |

### Global Defaults: `clipar()`

Set defaults for all plots â€” like
[`par()`](https://rdrr.io/r/graphics/par.html) for base R:

``` r

clipar(palette.qualitative = "nejm", stat.test = "wilcox.test",
       file.width = 8, file.height = 6, file.res = 600)

clipar("palette.qualitative")  # query a single parameter
# [1] "nejm"
```

------------------------------------------------------------------------

## ðŸ–¼ï¸? Gallery

Every figure is a single
[`cliplot()`](https://vanhungtran.github.io/cliomicplot/reference/cliplot.md)
call. Click any image to enlarge.

### Clinical Oncology â€” 7 Lines of Code

``` r

library(cliomicplot)
library(survival)
clitheme("nejm")

# 1. Kaplan-Meier survival with risk table
cliplot(Surv(time, status) ~ sex, data = lung,
        type = type_km(risk_table = TRUE, pval = TRUE, conf_int = TRUE))

# 2. Forest plot from Cox model
cliplot(HR ~ Variable, data = cox_results, type = "forest")

# 3. Waterfall â€” tumor response
cliplot(BestChange ~ Patient, data = imtrial,
        type = type_waterfall(show_labels = TRUE))

# 4. Swimmer â€” patient timelines
cliplot(Duration ~ Patient, data = imtrial,
        type = type_swimmer(bar_fill = "BestResp", event_times = events_df))
```

|  |  |  |  |
|:--:|:--:|:--:|:--:|
| **Kaplan-Meier** | **Forest Plot** | **Waterfall** | **Swimmer** |
| [![KM](reference/figures/km.png)](https://vanhungtran.github.io/cliomicplot/man/figures/km.png) | [![Forest](reference/figures/forest.png)](https://vanhungtran.github.io/cliomicplot/man/figures/forest.png) | [![Waterfall](reference/figures/waterfall.png)](https://vanhungtran.github.io/cliomicplot/man/figures/waterfall.png) | [![Swimmer](reference/figures/swimmer.png)](https://vanhungtran.github.io/cliomicplot/man/figures/swimmer.png) |

### Clinical Trials & Cohort Studies (Journal Archetypes)

New clinical visualization archetypes adhering to Nature, Cell, and
Science reporting standards and real-world clinical cohort figures:

``` r

# 1. Minimal Clinically Important Difference (MCID) Dumbbell (Archetype 1)
# Compare observed effect sizes vs validated MCID threshold
cliplot(Estimate ~ Endpoint, data = mcid_df,
        type = type_mcid(mcid = "MCID", n_treated = "N"))

# 2. Longitudinal Trajectory with Clinical Severity Bands (Archetype 4)
# Spaghetti paths + bold mixed-model mean + shaded Mild/Moderate/Severe bands
cliplot(Score ~ Visit | Patient, data = longitudinal_df,
        type = type_trajectory(show_mean = TRUE, show_n = TRUE))

# 3. Kaplan-Meier with Cumulative Incidence & Landmark Milestones (Archetype 5)
# Zero external dependencies; exact percentages at 6mo, 1y, 2y callouts
cliplot(Surv(time, status) ~ sex, data = lung,
        type = type_km(cumulative_incidence = TRUE, milestones = c(180, 365, 730)))

# 4. Swimmer Plot with Ongoing Indicators (Archetype 13)
# Ongoing therapy arrowheads at bar ends + RECIST event milestones
cliplot(Duration ~ Patient, data = pt_data,
        type = type_swimmer(bar_fill = "Response", ongoing = "Ongoing", event_times = events))

# 5. Forest Plot with Alternating Stripes & Pooled Diamonds (Archetype 9)
# Subtle row striping eliminates eye drift across wide canvases
cliplot(HR ~ Variable, data = meta_df,
        type = type_forest(stripe = TRUE, diamond = TRUE, show_values = TRUE))
```

### Multi-Omics â€” Differential Expression in 3 Lines

``` r

# Volcano plot (y ~ x: significance ~ fold-change)
cliplot(-log10(padj) ~ logFC, data = deg_results,
        type = type_volcano(label_genes = "significant"))

# MA plot with LOESS smoothing
cliplot(logFC ~ baseMean, data = deg_results, type = "ma")

# Correlation heatmap
cliplot(expression_matrix, type = type_correlation(method = "spearman"))
```

|  |  |  |
|:--:|:--:|:--:|
| **Volcano** | **MA Plot** | **Correlation** |
| [![volcano](reference/figures/volcano.png)](https://vanhungtran.github.io/cliomicplot/man/figures/volcano.png) | [![MA](reference/figures/ma.png)](https://vanhungtran.github.io/cliomicplot/man/figures/ma.png) | [![Correlation](reference/figures/correlation.png)](https://vanhungtran.github.io/cliomicplot/man/figures/correlation.png) |
| **Heatmap** | **PCA** |  |
| [![heatmap](reference/figures/heatmap.png)](https://vanhungtran.github.io/cliomicplot/man/figures/heatmap.png) | [![PCA](reference/figures/pca.png)](https://vanhungtran.github.io/cliomicplot/man/figures/pca.png) |  |

### Nature / Cell / Science (cnsplots-Inspired)

New plot types inspired by the
[cnsplots](https://github.com/faridrashidi/cnsplots) gallery, rendered
in Nature/Science style (no in-figure titles, 300 dpi) â€” see
[`vignette("journal-plots")`](https://vanhungtran.github.io/cliomicplot/articles/journal-plots.md)
for the full real-data workflow.

|  |  |  |
|:--:|:--:|:--:|
| **ROC Curves** | **GSEA Dot Plot** | **UpSet** |
| [![ROC](reference/figures/roc.png)](https://vanhungtran.github.io/cliomicplot/man/figures/roc.png) | [![GSEA](reference/figures/gsea.png)](https://vanhungtran.github.io/cliomicplot/man/figures/gsea.png) | [![UpSet](reference/figures/upset.png)](https://vanhungtran.github.io/cliomicplot/man/figures/upset.png) |
| **Venn** | **Slope** | **Confusion Matrix** |
| [![Venn](reference/figures/venn.png)](https://vanhungtran.github.io/cliomicplot/man/figures/venn.png) | [![Slope](reference/figures/slope.png)](https://vanhungtran.github.io/cliomicplot/man/figures/slope.png) | [![Confusion](reference/figures/confusion.png)](https://vanhungtran.github.io/cliomicplot/man/figures/confusion.png) |
| **Donut** |  |  |
| [![Donut](reference/figures/donut.png)](https://vanhungtran.github.io/cliomicplot/man/figures/donut.png) |  |  |

### Recreating a Published Paper (Soares et al. 2025, *Biology of Sex Differences*)

The main figures of a real publication - “X-linked transcriptome
dysregulation across immune cells in systemic lupus erythematosus” (DOI:
`10.1186/s13293-025-00750-3`) - were recreated from the paper’s own
supplementary tables (bundled in `inst/extdata/sle_paper/`), rendered at
300 dpi in Nature style. See
[`vignette("recreate-paper")`](https://vanhungtran.github.io/cliomicplot/articles/recreate-paper.md)
and `inst/extdata/build_sle_figures.R`.

|  |  |  |
|:--:|:--:|:--:|
| **Fig. 1: X-linked dysregulation** | **Fig. 2: DIABLO classification** | **Fig. 3: XIST dysregulation** |
| [![SLE Fig 1](reference/figures/sle_fig1.png)](https://vanhungtran.github.io/cliomicplot/man/figures/sle_fig1.png) | [![SLE Fig 2](reference/figures/sle_fig2.png)](https://vanhungtran.github.io/cliomicplot/man/figures/sle_fig2.png) | [![SLE Fig 3](reference/figures/sle_fig3.png)](https://vanhungtran.github.io/cliomicplot/man/figures/sle_fig3.png) |
| **Fig. 4: XIST interactome** |  |  |
| [![SLE Fig 4](reference/figures/sle_fig4.png)](https://vanhungtran.github.io/cliomicplot/man/figures/sle_fig4.png) |  |  |

Plot types used: `type_boxplot`, `type_volcano`, `type_heatmap`,
`type_barplot` + `patchwork` composites.

### Recreating a Published Paper (SpaMTP, *Nature Methods* 2026)

The main figures of the SpaMTP manuscript - “SpaMTP: a framework for
spatial metabolomics data integration and analysis” (DOI:
`10.1038/s41592-026-03140-8`) - were recreated from the paper’s public
datasets (processed Seurat objects mirrored on the [SpaMTP Zenodo
community](https://zenodo.org/communities/spamtp), CC-BY-4.0), rendered
at 300 dpi in Nature style. See
[`vignette("recreate-spamtp")`](https://vanhungtran.github.io/cliomicplot/articles/recreate-spamtp.md)
and `inst/extdata/build_spamtp_figures.R`.

|  |  |  |
|:--:|:--:|:--:|
| **Fig. 1: Bladder MALDI-MSI** | **Fig. 2: Multi-omic mouse brain** | **Fig. 3: Brain tumour pathways** |
| [![SpaMTP Fig 1](reference/figures/spamtp_fig1.png)](https://vanhungtran.github.io/cliomicplot/man/figures/spamtp_fig1.png) | [![SpaMTP Fig 2](reference/figures/spamtp_fig2.png)](https://vanhungtran.github.io/cliomicplot/man/figures/spamtp_fig2.png) | [![SpaMTP Fig 3](reference/figures/spamtp_fig3.png)](https://vanhungtran.github.io/cliomicplot/man/figures/spamtp_fig3.png) |
| **Fig. 4: Spotted mouse liver** |  |  |
| [![SpaMTP Fig 4](reference/figures/spamtp_fig4.png)](https://vanhungtran.github.io/cliomicplot/man/figures/spamtp_fig4.png) |  |  |

Datasets: mouse urinary bladder MALDI-MSI (PRIDE `PXD001283`), Parkinson
mouse brain Visium + MALDI (SciLifeLab
`10.17044/scilifelab.22770161.v2`), human brain tumour single-cell
pathways (BioImage Archive `S-BIAD14260`), and spotted mouse liver
MALDI-MSI (METASPACE). Plot types used: `type_heatmap`, `type_volcano`,
`type_boxplot` + spatial scatter maps and `patchwork` composites.

### Distributions & Statistics

|  |  |  |
|:--:|:--:|:--:|
| **Boxplot** | **Violin** | **Ridge** |
| [![boxplot](reference/figures/boxplot.png)](https://vanhungtran.github.io/cliomicplot/man/figures/boxplot.png) | [![violin](reference/figures/violin.png)](https://vanhungtran.github.io/cliomicplot/man/figures/violin.png) | [![Ridge](reference/figures/ridge.png)](https://vanhungtran.github.io/cliomicplot/man/figures/ridge.png) |
| **Density** | **Histogram** | **Q-Q Plot** |
| [![Density](reference/figures/density.png)](https://vanhungtran.github.io/cliomicplot/man/figures/density.png) | [![Histogram](reference/figures/histogram.png)](https://vanhungtran.github.io/cliomicplot/man/figures/histogram.png) | [![QQ](reference/figures/qq.png)](https://vanhungtran.github.io/cliomicplot/man/figures/qq.png) |
| **Beeswarm** | **Raincloud** | **Jitter** |
| [![Beeswarm](reference/figures/beeswarm.png)](https://vanhungtran.github.io/cliomicplot/man/figures/beeswarm.png) | [![Raincloud](reference/figures/raincloud.png)](https://vanhungtran.github.io/cliomicplot/man/figures/raincloud.png) | [![Jitter](reference/figures/jitter.png)](https://vanhungtran.github.io/cliomicplot/man/figures/jitter.png) |

### Trend & Comparison

|  |  |  |
|:--:|:--:|:--:|
| **Scatter** | **Lines** | **LM / LOESS** |
| [![scatter](reference/figures/scatter.png)](https://vanhungtran.github.io/cliomicplot/man/figures/scatter.png) | [![Lines](reference/figures/lines.png)](https://vanhungtran.github.io/cliomicplot/man/figures/lines.png) | [![LM](reference/figures/lm.png)](https://vanhungtran.github.io/cliomicplot/man/figures/lm.png) |
| **Bubble** | **Dumbbell** | **Lollipop** |
| [![Bubble](reference/figures/bubble.png)](https://vanhungtran.github.io/cliomicplot/man/figures/bubble.png) | [![Dumbbell](reference/figures/dumbbell.png)](https://vanhungtran.github.io/cliomicplot/man/figures/dumbbell.png) | [![Lollipop](reference/figures/lollipop.png)](https://vanhungtran.github.io/cliomicplot/man/figures/lollipop.png) |

### Infographic & Pipeline

|  |  |  |
|:--:|:--:|:--:|
| **Clinical Trials Pipeline** | **Infographic Bar** | **Diamond Bubble Heatmap** |
| [![Trials](reference/figures/trials.png)](https://vanhungtran.github.io/cliomicplot/man/figures/trials.png) | [![Infobar](reference/figures/infobar.png)](https://vanhungtran.github.io/cliomicplot/man/figures/infobar.png) | [![BubbleHeatmap](reference/figures/bubble_heatmap.png)](https://vanhungtran.github.io/cliomicplot/man/figures/bubble_heatmap.png) |

### R Graph Gallery Favorites

|  |  |  |
|:--:|:--:|:--:|
| **Chord** | **Treemap** | **Dendrogram** |
| [![Chord](reference/figures/chord.png)](https://vanhungtran.github.io/cliomicplot/man/figures/chord.png) | [![Treemap](reference/figures/treemap.png)](https://vanhungtran.github.io/cliomicplot/man/figures/treemap.png) | [![Dendrogram](reference/figures/dendrogram.png)](https://vanhungtran.github.io/cliomicplot/man/figures/dendrogram.png) |
| **Circular Bar** | **Connected Scatter** | **2D Density** |
| [![Circbar](reference/figures/circular_bar.png)](https://vanhungtran.github.io/cliomicplot/man/figures/circular_bar.png) | [![Connected](reference/figures/connected.png)](https://vanhungtran.github.io/cliomicplot/man/figures/connected.png) | [![2D Density](reference/figures/density2d.png)](https://vanhungtran.github.io/cliomicplot/man/figures/density2d.png) |
| **Parallel Coords** | **Spineplot** | **Waffle** |
| [![Parallel](reference/figures/parallel.png)](https://vanhungtran.github.io/cliomicplot/man/figures/parallel.png) | [![Spineplot](reference/figures/spineplot.png)](https://vanhungtran.github.io/cliomicplot/man/figures/spineplot.png) | [![Waffle](reference/figures/waffle.png)](https://vanhungtran.github.io/cliomicplot/man/figures/waffle.png) |

Full list: `points`, `jitter`, `barplot`, `lines`, `histogram`,
`density`, `errorbar`, `ribbon`, `boxplot`, `violin`, `ridge`,
`volcano`, `forest`, `km`, `waterfall`, `swimmer`, `heatmap`, `pca`,
`ma`, `correlation`, `text`, `bubble`, `bubble_heatmap`, `lm`, `loess`,
`spineplot`, `rug`, `abline`, `qq`, `raincloud`, `dumbbell`, `lollipop`,
`beeswarm`, `radar`, `alluvial`, `waffle`, `chord`, `treemap`,
`streamgraph`, `connected`, `circular_bar`, `density2d`, `parallel`,
`dendrogram`, `trials`, `infobar`, `rankabundance`, `pattern`,
`deg_compare`, `roc`, `gsea`, `upset`, `venn`, `slope`, `confusion`,
`donut`.

------------------------------------------------------------------------

## ðŸŽ¨ Themes â€” Publication Ready

11 built-in themes. One function call.

``` r

clitheme("nature")     # persistent theme for all subsequent plots
cliplot(mpg ~ wt, data = mtcars)
clitheme()             # reset to default
```

Or per-plot:

``` r

cliplot(mpg ~ wt, data = mtcars, theme = "dark")
```

| Theme | Style | Journal / Use |
|----|----|----|
| `cli_bw` | Clean black-and-white | Default |
| `cli_classic` | Classic with axis lines | Base R aesthetic |
| `cli_minimal` | Minimalist | Modern general use |
| `nature` | Nature Publishing Group | Nature journals |
| `science` | Science / AAAS | *Science* |
| `nejm` | NEJM | *New England Journal of Medicine* |
| `lancet` | The Lancet | *The Lancet* |
| `cell` | Cell Press | *Cell* and family |
| `broadsheet` | Print-optimized with subtle grid | General publication |
| `showcase` | Polished presentation | Slides, posters |
| `dark` | Dark background | Talks, dashboards |

Register custom themes with
`clitheme_register("mytheme", base = "cli_bw", ...)`.

------------------------------------------------------------------------

## ðŸŒˆ Color Palettes

55+ built-in palettes. Discrete, sequential, diverging, and
journal-specific.

``` r

cli_palette_list()               # see all names
cli_palette_show("cosmic")       # preview a palette

# In plots
cliplot(mpg ~ wt | cyl, data = mtcars, palette = "npg")

# Standalone ggplot2 scales
ggplot(iris, aes(Sepal.Length, Petal.Length, color = Species)) +
  geom_point(size = 3) +
  palette_scale("jama", "color")
```

| Category | Palettes |
|----|----|
| Journals | `jco`, `nejm`, `lancet`, `npg`, `jama`, `bmj`, `frontiers` |
| Genomics | `ucscgb`, `igv`, `locuszoom`, `cosmic`, `cosmic_sig`, `gsea`, `volcano` |
| Accessibility | `okabe_ito`, `tableau10`, `tol_muted` |
| D3 / Web | `d3_category10`, `d3_category20`, `flatui`, `material`, `bs5` |
| Sequential | `blues`, `reds`, `greens`, `purples`, `oranges` |
| Diverging | `heatmap_rdbu`, `heatmap_rdylbu`, `heatmap_prgn`, `rd_yl_gn`, `spectral`, `pi_yg` |
| Dark | `neon`, `cyberpunk` |
| Soft | `pastel`, `soft` |
| Sci-fi | `futurama`, `rickandmorty`, `simpsons`, `startrek` |

------------------------------------------------------------------------

## ðŸ“? Markdown Text

Rich text in titles and labels with
[`cli_markdown()`](https://vanhungtran.github.io/cliomicplot/reference/cli_markdown.md):

``` r

cliplot(len ~ dose, data = ToothGrowth, type = "boxplot",
        title = "**Tooth Length** by Vitamin C Dose",
        subtitle = "*p* &lt; 0.05 for all pairwise comparisons",
        ylab = "Length (mm)") +
  cli_markdown()
```

------------------------------------------------------------------------

## ðŸ”§ For Standalone ggplot2 Use

Use
[`palette_scale()`](https://vanhungtran.github.io/cliomicplot/reference/palette_scale.md)
to drop cliomicplot palettes into any ggplot:

``` r

library(ggplot2)
ggplot(iris, aes(Sepal.Length, Petal.Length, color = Species)) +
  geom_point() +
  palette_scale("cosmic", "color")

# Continuous palettes too
ggplot(faithfuld, aes(waiting, eruptions, fill = density)) +
  geom_tile() +
  palette_scale("heatmap_rdbu", "fill", type = "continuous")
```

------------------------------------------------------------------------

## ðŸ“š Vignettes

- **Getting Started** â€”
  [`vignette("cliomicplot")`](https://vanhungtran.github.io/cliomicplot/articles/cliomicplot.md)
- **Volcano Plot Guide** â€”
  [`vignette("volcano-plot")`](https://vanhungtran.github.io/cliomicplot/articles/volcano-plot.md)
- **Oncology Workflows** â€”
  [`vignette("oncology")`](https://vanhungtran.github.io/cliomicplot/articles/oncology.md)
  â€” 7 figures: KM, forest, waterfall, swimmer, dose-response,
  time-course, PD biomarkers
- **Multi-Omics** â€”
  [`vignette("multiomics")`](https://vanhungtran.github.io/cliomicplot/articles/multiomics.md)
  â€” 10 figures: PCA, correlation, volcano, MA, pattern clustering,
  rank-abundance, DEG comparison, bubble heatmap & more
- **Recreating a Published Figure** -
  [`vignette("recreate-paper")`](https://vanhungtran.github.io/cliomicplot/articles/recreate-paper.md) -
  regenerates the figures of Soares et al. 2025 (*Biology of Sex
  Differences*, SLE X-linked dysregulation) from the paper’s
  supplementary data: X/A-ratio and XIST boxplots, volcano small
  multiples, logFC/NES heatmaps, DIABLO F1/marker barplots,
  intercellular correlation diverging bars
- **Recreating a Published Paper (Spatial Metabolomics)** -
  [`vignette("recreate-spamtp")`](https://vanhungtran.github.io/cliomicplot/articles/recreate-spamtp.md) -
  regenerates the figures of the SpaMTP manuscript (Wemans et al. 2026,
  *Nature Methods*) from the paper’s public datasets: bladder MALDI-MSI
  segmentation and DEM heatmaps, multi-omic mouse brain MSI/Visium,
  tumour pathway NES heatmaps, and spotted-liver annotation QC
- **Journal-Quality Figures** â€”
  [`vignette("journal-plots")`](https://vanhungtran.github.io/cliomicplot/articles/journal-plots.md)
  â€” cnsplots-inspired ROC, GSEA dot plot, UpSet, Venn, slope,
  confusion, donut + Nature/Cell/Science export helpers
- **Themes & Palettes** â€”
  [`vignette("themes-palettes")`](https://vanhungtran.github.io/cliomicplot/articles/themes-palettes.md)

------------------------------------------------------------------------

## ðŸ”— References

- **tinyplot** â€” formula-interface inspiration
  ([github.com/grantmcdermott/tinyplot](https://github.com/grantmcdermott/tinyplot))
- **ggsci** â€” journal palette inspiration
  ([nanx.me/ggsci](https://nanx.me/ggsci/))
- **survminer** â€” survival plotting engine
- **ComplexHeatmap** â€” heatmap engine for omics data

------------------------------------------------------------------------

## ðŸ“„ License

MIT Â© 2026 Lucas VHH Tran
