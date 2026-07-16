# References and Attributions

## API Design Inspiration

The formula-based API (`y ~ x | group`) follows a pattern established by
base R’s `plot.formula()` and adopted by many packages in the R
ecosystem. The type system (factory functions returning draw/data
closures) is a general software engineering pattern used across multiple
R graphics frameworks.

## Colour Palette Sources

Colour hex values are factual data derived from the following published
sources:

| Palette | Source |
|----|----|
| `jco` | *Journal of Clinical Oncology* style guide |
| `nejm` | *New England Journal of Medicine* style guide |
| `lancet` | *Lancet Oncology* published figure colours |
| `npg` | *Nature Reviews Cancer* published figure colours |
| `jama` | *JAMA* published figure colours |
| `bmj` | *BMJ* Living Style Guide |
| `science` / `aaas` | *Science* / AAAS published figure colours |
| `frontiers` | Frontiers logo colour palette |
| `ucscgb` | UCSC Genome Browser chromosome colour specification |
| `igv` | Integrative Genomics Viewer chromosome colours |
| `locuszoom` | LocusZoom GWAS visualization tool |
| `cosmic` / `cosmic_sig` | COSMIC database (Catalogue of Somatic Mutations in Cancer) |
| `gsea` | GSEA GenePattern software |
| `uchicago` | University of Chicago identity guidelines |
| `d3_category10` / `d3_category20` | D3.js library (Mike Bostock) |
| `tableau10` | Tableau Software default palette |
| `okabe_ito` | Okabe & Ito (2008) colourblind-safe palette |
| `tol_muted` | Paul Tol’s muted colour scheme |
| `material` | Google Material Design colour system |
| `flatui` | Flat UI Colors project |
| `bs5` | Bootstrap 5 CSS framework |
| `futurama` | Fan-created colour extraction from *Futurama* (20th Century Fox) |
| `rickandmorty` | Fan-created colour extraction from *Rick and Morty* (Adult Swim) |
| `simpsons` | Fan-created colour extraction from *The Simpsons* (20th Century Fox) |
| `startrek` | Fan-created colour extraction from *Star Trek* (CBS/Paramount) |

## Theme Inspirations

- `theme_cli_broadsheet()` — General newspaper/broadsheet print design
  conventions with faint horizontal rules and compact typography.
- `theme_cli_nature()`, `theme_cli_science()`, `theme_cli_cell()`,
  `theme_cli_nejm()`, `theme_cli_lancet()` — Approximations of journal
  formatting conventions for pre-submission figure preparation.

## Software Dependencies

cliomicplot builds on **ggplot2** (Wickham, 2016) and the following
ecosystem packages: scales, ggtext, ggrepel, patchwork, ggpubr,
survminer, survival, reshape2, RColorBrewer, and circlize. All are used
according to their published APIs.
