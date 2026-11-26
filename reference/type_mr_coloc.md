# Cis-pQTL Mendelian Randomization and Colocalization Heatmap

Creates publication-ready Mendelian Randomization (MR) and Bayesian
colocalization heatmaps (Clinical Archetype 36) for evaluating causal
genetic links between molecular exposures (e.g. plasma pQTLs, eQTLs, or
single-cell marker signatures) and complex disease endpoints.

Evaluates Wald ratio effect sizes (Z-score or beta) as a diverging
gradient, overlays odds ratios where p \< 0.05, outlines tiles passing
multiple-testing thresholds (e.g. Bonferroni), places corner glyphs for
robust colocalization (PP.H4 \>= 0.80), and mutes discordant
associations (PP.H3 \>= 0.80, such as MHC/HLA linkage disequilibrium
artifacts) in italicized grey text.

## Usage

``` r
type_mr_coloc(
  beta_col = NULL,
  se_col = NULL,
  z_col = NULL,
  p_col = NULL,
  or_col = NULL,
  pp4_col = NULL,
  pp3_col = NULL,
  fdr_col = NULL,
  set_col = NULL,
  family_col = NULL,
  p_threshold = 0.05,
  bonferroni = TRUE,
  coloc_threshold = 0.8,
  discordant_threshold = 0.8,
  text_size = 2,
  text_format = "%.2f",
  glyph_shape = 17,
  glyph_size = 1,
  glyph_nudge = c(0.36, 0.3),
  colors = c("#2166AC", "white", "#B2182B"),
  limits = c(-6, 6),
  fill_name = "z of Wald ratio",
  border_color = "grey85",
  border_linewidth = 0.25,
  frame_color = "black",
  frame_linewidth = 0.55,
  show_caption = TRUE,
  caption = NULL,
  xlab = NULL,
  ylab = NULL
)
```

## Arguments

- beta_col:

  Column name for MR effect estimate (beta or Wald ratio).

- se_col:

  Column name for standard error of the MR estimate.

- z_col:

  Column name for Z-score (beta / SE). Auto-computed if `beta_col` and
  `se_col` are present.

- p_col:

  Column name for association p-value.

- or_col:

  Column name for odds ratio per SD (or effect size for text display).

- pp4_col:

  Column name for colocalization posterior probability of hypothesis 4
  (shared causal variant, \\\text{PP.H4}\\).

- pp3_col:

  Column name for colocalization posterior probability of hypothesis 3
  (distinct causal variants / LD artifact, \\\text{PP.H3}\\).

- fdr_col:

  Column name for FDR-adjusted q-value.

- set_col:

  Column name for exposure grouping (e.g. marker protein class: effector
  vs memory).

- family_col:

  Column name for outcome disease family (e.g. Cardiovascular, Allergic,
  Autoimmune).

- p_threshold:

  Numeric p-value threshold for displaying numerical effect sizes inside
  tiles (default 0.05).

- bonferroni:

  Logical or numeric. If `TRUE` (default), computes 0.05 / N over all
  pairs and draws a prominent black border around significant tiles. If
  numeric, uses the specified threshold. Set to `FALSE` to disable.

- coloc_threshold:

  Minimum PP.H4 to display the colocalization corner triangle glyph
  (default 0.80).

- discordant_threshold:

  Minimum PP.H3 to format text as muted italic grey (default 0.80),
  flagging distinct variants in the same locus.

- text_size:

  Font size for numbers inside tiles (default 2.0).

- text_format:

  String format for effect numbers (default "%.2f").

- glyph_shape:

  Point shape for colocalization marker (default 17, solid triangle).

- glyph_size:

  Size of colocalization corner marker (default 1.0).

- glyph_nudge:

  Numeric vector of length 2 specifying x and y nudge for the corner
  glyph (default `c(0.36, 0.30)`).

- colors:

  Diverging color gradient vector of length 3 (low, mid, high). Default
  `c("#2166AC", "white", "#B2182B")`.

- limits:

  Symmetrical color scale limits for \\Z\\-score (default `c(-6, 6)`).
  Values beyond are squished.

- fill_name:

  Legend title for color gradient (default `"z of Wald ratio"`).

- border_color:

  Grid border color between tiles (default `"grey85"`).

- border_linewidth:

  Grid line width between tiles (default 0.25).

- frame_color:

  Outline color for Bonferroni-significant tiles (default `"black"`).

- frame_linewidth:

  Outline stroke width for Bonferroni-significant tiles (default 0.55).

- show_caption:

  Logical. If `TRUE` (default), appends a standardized publication
  caption explaining numbers, frames, triangles, and italicized text.

- caption:

  Optional override caption string.

- xlab, ylab:

  Axis labels.

## Value

A `cliplot_type` object for use with
[`cliplot`](https://vanhungtran.github.io/cliomicplot/reference/cliplot.md).

## Examples

``` r
if (FALSE) { # \dontrun{
library(cliomicplot)
data(pqtl_mr_coloc)

# Full multi-panel MR & Colocalization matrix
cliplot(
  symbol ~ label_short,
  data = pqtl_mr_coloc,
  type = type_mr_coloc(
    set_col = "set_label",
    family_col = "family_label",
    p_threshold = 0.05,
    bonferroni = TRUE,
    coloc_threshold = 0.80
  ),
  title = "Cis-pQTL Mendelian Randomization & Colocalization Matrix",
  subtitle = "UKB-PPP Plasma Proteomics vs FinnGen R12 Endpoints"
)
} # }
```
