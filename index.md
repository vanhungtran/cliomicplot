# cliomicplot

# **cliomicplot** _(*Publication-Ready Clinical & Multi-Omics Visualizations*)

------------------------------------------------------------------------

## ⚡ 30 Seconds

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
| Formula interface (`y ~ x \| group`) | — | ✅ |
| Auto-legend + auto-facet | Manual | ✅ |
| Statistical annotations | Needs `ggpubr` | ✅ Built-in |
| Journal themes (Nature, NEJM, etc.) | Manual tweaking | ✅ `theme = "nejm"` |
| 55+ colorblind-safe palettes | Manual selection | ✅ `palette = "jco"` |
| Markdown in titles/labels | Needs `ggtext` | ✅ [`cli_markdown()`](https://vanhungtran.github.io/cliomicplot/reference/cli_markdown.md) |
| Save to file | Separate [`ggsave()`](https://ggplot2.tidyverse.org/reference/ggsave.html) | ✅ `file = "plot.pdf"` |
| Persistent global theme | [`theme_set()`](https://ggplot2.tidyverse.org/reference/get_theme.html) | ✅ `clitheme("nature")` |

------------------------------------------------------------------------

## 📦 Installation

``` r
# From GitHub
remotes::install_github("vanhungtrantran/cliomicplot")

# Dependencies (installed automatically, but pre-install if needed)
install.packages(c("ggplot2", "scales", "survival", "reshape2", "ggrepel", "ggtext"))

# Optional: richer heatmaps (ComplexHeatmap) and extended survival plotting (survminer)
install.packages(c("ComplexHeatmap", "survminer", "ggpubr", "patchwork"))
```

------------------------------------------------------------------------

## 🧩 The Formula API

cliomicplot uses `y ~ x | group` — the same convention you already know.

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

Set defaults for all plots — like
[`par()`](https://rdrr.io/r/graphics/par.html) for base R:

``` r
clipar(palette.qualitative = "nejm", stat.test = "wilcox.test",
       file.width = 8, file.height = 6, file.res = 600)

clipar("palette.qualitative")  # query a single parameter
# [1] "nejm"
```

------------------------------------------------------------------------

## 🖼️ Gallery

Every figure is a single
[`cliplot()`](https://vanhungtran.github.io/cliomicplot/reference/cliplot.md)
call. Click any image to enlarge.

### Clinical

|  |  |  |
|:--:|:--:|:--:|
| **Kaplan-Meier** | **Forest Plot** | **Volcano** |
| [![KM](reference/figures/km.png)](https://vanhungtran.github.io/cliomicplot/man/figures/km.png) | [![Forest](reference/figures/forest.png)](https://vanhungtran.github.io/cliomicplot/man/figures/forest.png) | [![volcano](reference/figures/volcano.png)](https://vanhungtran.github.io/cliomicplot/man/figures/volcano.png) |
| **Waterfall** | **Swimmer** | **MA Plot** |
| [![Waterfall](reference/figures/waterfall.png)](https://vanhungtran.github.io/cliomicplot/man/figures/waterfall.png) | [![Swimmer](reference/figures/swimmer.png)](https://vanhungtran.github.io/cliomicplot/man/figures/swimmer.png) | [![MA](reference/figures/ma.png)](https://vanhungtran.github.io/cliomicplot/man/figures/ma.png) |

### Omics

|  |  |  |
|:--:|:--:|:--:|
| **Heatmap** | **PCA** | **Correlation** |
| [![heatmap](reference/figures/heatmap.png)](https://vanhungtran.github.io/cliomicplot/man/figures/heatmap.png) | [![PCA](reference/figures/pca.png)](https://vanhungtran.github.io/cliomicplot/man/figures/pca.png) | [![Correlation](reference/figures/correlation.png)](https://vanhungtran.github.io/cliomicplot/man/figures/correlation.png) |

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
`deg_compare`.

------------------------------------------------------------------------

## 🎨 Themes — Publication Ready

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

## 🌈 Color Palettes

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

## 📝 Markdown Text

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

## 🔧 For Standalone ggplot2 Use

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

## 📚 Vignettes

- **Getting Started** —
  [`vignette("cliomicplot")`](https://vanhungtran.github.io/cliomicplot/articles/cliomicplot.md)
- **Volcano Plot Guide** —
  [`vignette("volcano-plot")`](https://vanhungtran.github.io/cliomicplot/articles/volcano-plot.md)
- **Oncology Workflows** —
  [`vignette("oncology")`](https://vanhungtran.github.io/cliomicplot/articles/oncology.md)
  — 7 figures: KM, forest, waterfall, swimmer, dose-response,
  time-course, PD biomarkers
- **Multi-Omics** —
  [`vignette("multiomics")`](https://vanhungtran.github.io/cliomicplot/articles/multiomics.md)
  — 10 figures: PCA, correlation, volcano, MA, pattern clustering,
  rank-abundance, DEG comparison, bubble heatmap & more
- **Themes & Palettes** —
  [`vignette("themes-palettes")`](https://vanhungtran.github.io/cliomicplot/articles/themes-palettes.md)

------------------------------------------------------------------------

## 🔗 References

- **tinyplot** — formula-interface inspiration
  ([github.com/grantmcdermott/tinyplot](https://github.com/grantmcdermott/tinyplot))
- **ggsci** — journal palette inspiration
  ([nanx.me/ggsci](https://nanx.me/ggsci/))
- **survminer** — survival plotting engine
- **ComplexHeatmap** — heatmap engine for omics data

------------------------------------------------------------------------

## 📄 License

MIT © 2026 Lucas VHH Tran
