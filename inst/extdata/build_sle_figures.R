# ===========================================================================
# Recreate the main figures of Soares et al. 2025, Biology of Sex Differences
# "X-linked transcriptome dysregulation across immune cells in systemic
# lupus erythematosus" (DOI: 10.1186/s13293-025-00750-3; PMID: 40999523)
# using cliomicplot plot types and the paper's own supplementary tables
# (bundled as .rda files in inst/extdata/sle_paper/).
#
# Run from the package root:
#   Rscript inst/extdata/build_sle_figures.R
#
# Produces man/figures/sle_fig{1..4}.png at 300 dpi, Nature-style
# (no in-figure titles, bold lowercase panel labels, black on white,
# thin black axes). Figure layouts mirror the paper's Figure 1-4.
# ===========================================================================

suppressMessages(devtools::load_all(".", quiet = TRUE))
suppressMessages(library(ggplot2))
suppressMessages(library(patchwork))
clipar(stat.test = NULL)

fig_dir  = "man/figures"
data_dir = "inst/extdata/sle_paper"
dir.create(fig_dir, recursive = TRUE, showWarnings = FALSE)

resolve_data = function(filename) {
  pkg_path = system.file("extdata", "sle_paper", filename, package = "cliomicplot")
  if (pkg_path != "" && file.exists(pkg_path)) return(pkg_path)
  file.path(data_dir, filename)
}

load_data = function(filename) {
  e = new.env(parent = emptyenv())
  load(resolve_data(filename), envir = e)
  as.list(e)
}

save_fig = function(p, file, width_mm, height_mm, dpi = 300) {
  ggplot2::ggsave(
    filename = file.path(fig_dir, file),
    plot     = p,
    width    = width_mm,
    height   = height_mm,
    units    = "mm",
    dpi      = dpi
  )
  cat("saved", file, sprintf("(%dx%d mm, %d dpi)\n", width_mm, height_mm, dpi))
}

# ---- Cell-type order (immune lineage groups, as in the paper) -------------
lineage_order = c(
  "CD16p_Mono", "CL_Mono", "Int_Mono", "NC_Mono", "LDG", "Neu", "mDC", "pDC",
  "Naive_CD4", "Mem_CD4", "Tfh", "Th1", "Th2", "Th17",
  "Fr._I_nTreg", "Fr._II_eTreg", "Fr._III_T",
  "Naive_CD8", "CM_CD8", "EM_CD8", "TEMRA_CD8",
  "Naive_B", "DN_B", "SM_B", "USM_B", "Plasmablast", "NK"
)
lineage_map = c(
  CD16p_Mono = "Myeloid", CL_Mono = "Myeloid", Int_Mono = "Myeloid",
  NC_Mono = "Myeloid", LDG = "Myeloid", Neu = "Myeloid",
  mDC = "Myeloid", pDC = "Myeloid",
  Naive_CD4 = "CD4 T", Mem_CD4 = "CD4 T", Tfh = "CD4 T", Th1 = "CD4 T",
  Th2 = "CD4 T", Th17 = "CD4 T", Fr._I_nTreg = "CD4 T",
  Fr._II_eTreg = "CD4 T", Fr._III_T = "CD4 T",
  Naive_CD8 = "CD8 T", CM_CD8 = "CD8 T", EM_CD8 = "CD8 T", TEMRA_CD8 = "CD8 T",
  Naive_B = "B cell", DN_B = "B cell", SM_B = "B cell",
  USM_B = "B cell", Plasmablast = "B cell", NK = "NK"
)
lineage_colors = c(Myeloid = "#4DBBD5", `CD4 T` = "#E64B35", `CD8 T` = "#00A087",
                   `B cell` = "#F39B7F", NK = "#8491B4")

rep_cells = c("NK", "NC_Mono", "pDC", "Naive_CD4", "CM_CD8", "Naive_B")

short_term = function(x, n = 44) {
  x = gsub("^GO[:_]|^KEGG[:_]|_", " ", x)
  x = gsub("\\s+", " ", x)
  out = ifelse(nchar(x) > n, paste0(substr(x, 1, n - 3), "..."), x)
  out
}

hm_label = function(p, lab) {
  p = p + cli_panel_label(lab)
  suppressMessages(p +
    ggplot2::coord_cartesian(xlim = c(0, 1), ylim = c(0, 1),
                             expand = FALSE, clip = "off") +
    ggplot2::theme(plot.margin = ggplot2::margin(12, 6, 6, 6)))
}

# Effect-size strip (Cohen d / logFC) with BH-adjusted significance, as in the paper
cohens_d = function(x, y) {
  nx = length(x); ny = length(y)
  sp = sqrt(((nx - 1) * stats::var(x) + (ny - 1) * stats::var(y)) / (nx + ny - 2))
  (mean(x) - mean(y)) / sp
}

effect_strip = function(pat, value_col, effect_fun, fill_name) {
  cts = levels(pat$Cell_Type)
  df = do.call(rbind, lapply(cts, function(ct) {
    s = pat[pat$Cell_Type == ct & pat$Condition == "SLE", value_col]
    h = pat[pat$Cell_Type == ct & pat$Condition == "HC", value_col]
    p = suppressWarnings(stats::wilcox.test(s, h)$p.value)
    data.frame(Cell_Type = ct, effect = effect_fun(s, h), p = p)
  }))
  df$FDR = stats::p.adjust(df$p, method = "BH")
  df$sig = ifelse(df$FDR < 0.05, "*", "")
  df$Cell_Type = factor(df$Cell_Type, levels = cts)
  df
}

strip_plot = function(df, fill_name) {
  ggplot(df, aes(Cell_Type, 1, fill = effect)) +
    geom_tile(color = "white", linewidth = 0.3) +
    geom_text(aes(label = sig), size = 2.6, fontface = "bold") +
    scale_fill_gradient2(low = "#2166AC", mid = "#F7F7F7", high = "#B2182B",
                         midpoint = 0, name = fill_name) +
    scale_y_continuous(limits = c(0.5, 1.5), expand = c(0, 0)) +
    theme_void(base_size = 8) +
    theme(plot.margin = margin(0, 0, 0, 0), legend.position = "none")
}
set.seed(42)

# ===========================================================================
# FIGURE 1 - X-linked transcriptome dysregulation across immune cells
# ===========================================================================
pat = load_data("sle_patients.rda")$sle_patients
pat$Cell_Type = factor(pat$Cell_Type, levels = lineage_order)
pat$Condition = factor(pat$Condition, levels = c("SLE", "HC"))

# a) X/A-ratio distribution, SLE vs HC, 27 immune cell types
# a) X/A-ratio distribution, SLE vs HC, 27 immune cell types, with Cohen-d effect strip
p1a_strip = strip_plot(effect_strip(pat, "X_Ratio", cohens_d), "Cohen d")
p1a_box = cliplot(X_Ratio ~ Cell_Type | Condition, data = pat,
              type = type_boxplot(add_jitter = TRUE, jitter_size = 0.55,
                                  jitter_alpha = 0.3, width = 0.65),
              theme = "nature", palette = "nejm") +
  labs(x = NULL, y = "X/A ratio") +
  cli_panel_label("a") +
  theme(axis.text.x = element_text(angle = 60, hjust = 1, size = 6.5),
        legend.position = "top",
        legend.title = element_blank())
p1a = (p1a_strip / p1a_box) + plot_layout(heights = c(0.8, 9), axes = "collect")
de = load_data("sle_xlinked_de.rda")$sle_xlinked_de
de$Cell_Type = factor(de$Cell_Type, levels = lineage_order)

vol_list = lapply(seq_along(rep_cells), function(i) {
  ct  = rep_cells[i]
  v   = de[de$Cell_Type == ct, ]
  rownames(v) = v$Gene
  show_x = i %in% 4:6
  show_y = i %in% c(1, 4)
  p = cliplot(-log10(FDR) ~ logFC, data = v, type = "volcano",
              theme = "nature") +
    labs(subtitle = ct,
         x = if (show_x) "log2 fold change (SLE vs HC)" else NULL,
         y = if (show_y) expression(-log[10] ~ FDR) else NULL) +
    theme(plot.subtitle = element_text(size = 8, face = "bold", hjust = 0.5),
          legend.position = "none",
          axis.title.x = if (show_x) element_text(size = 8) else element_blank(),
          axis.title.y = if (show_y) element_text(size = 8) else element_blank())
  if (i == 1) p = p + cli_panel_label("b")
  p
})
p1b = wrap_plots(vol_list, nrow = 2, ncol = 3)
de_cc = de[de$Classification != "Non-significant", ]
de_cc$Lineage = lineage_map[as.character(de_cc$Cell_Type)]
de_cc$Up       = de_cc$Classification == "Up-regulated"
lin_n = table(lineage_map)

consistent_genes = lapply(unique(lineage_map), function(L) {
  sub = de_cc[de_cc$Lineage == L, ]
  n   = lin_n[[L]]
  uc  = tapply(sub$Up, sub$Gene, sum)
  dc  = tapply(sub$Up == FALSE, sub$Gene, sum)
  mu  = tapply(sub$logFC[sub$Up], sub$Gene[sub$Up], mean)
  md  = tapply(sub$logFC[!sub$Up], sub$Gene[!sub$Up], mean)
  gu  = names(uc)[uc >= 0.8 * n]
  gd  = names(dc)[dc >= 0.8 * n]
  data.frame(Gene = c(gu, gd), Lineage = L,
             logFC = c(mu[gu], md[gd]), stringsAsFactors = FALSE)
})
consistent_genes = do.call(rbind, consistent_genes)

mat1c = reshape2::acast(consistent_genes, Gene ~ Lineage, value.var = "logFC")
mat1c[is.na(mat1c)] = 0
row_n  = apply(mat1c != 0, 1, sum)
row_mx = apply(abs(mat1c), 1, max)
mat1c  = mat1c[order(row_n, row_mx, decreasing = TRUE), , drop = FALSE]
mat1c  = mat1c[seq_len(min(42, nrow(mat1c))), , drop = FALSE]

p1c = cliplot(mat1c,
              type = type_heatmap(scale = "none", cluster_rows = FALSE,
                                  cluster_cols = FALSE, show_rownames = TRUE,
                                  show_colnames = TRUE, legend_title = "mean logFC",
                                  color_low = "#2166AC", color_mid = "#F7F7F7",
                                  color_high = "#B2182B"),
              theme = "nature")
p1c = hm_label(p1c, "c")

# d) NES heatmap of immune-related KEGG pathways (X/A-ratio correlation GSEA)
gxr = load_data("sle_gsea_xratio_kegg.rda")$sle_gsea_xratio_kegg
names(gxr)[names(gxr) == "FDR q-val"] = "FDR"
gxr$Cell_Type = factor(gxr$Cell_Type, levels = lineage_order)
gxr = gxr[gxr$FDR < 0.05, ]
term_n = table(gxr$Term)
keep_t = names(term_n)[term_n >= 3]
gxr = gxr[gxr$Term %in% keep_t, ]
mean_abs = tapply(abs(gxr$NES), gxr$Term, mean)
top_t    = names(sort(mean_abs, decreasing = TRUE))[1:24]
gxr      = gxr[gxr$Term %in% top_t, ]
gxr$TermShort = short_term(gxr$Term)

mat1d = reshape2::acast(gxr, TermShort ~ Cell_Type, value.var = "NES")
mat1d = mat1d[short_term(top_t), , drop = FALSE]   # keep NES-ranked order
mat1d[is.na(mat1d)] = 0

p1d = cliplot(mat1d,
              type = type_heatmap(scale = "none", cluster_rows = FALSE,
                                  cluster_cols = FALSE, show_rownames = TRUE,
                                  show_colnames = TRUE, legend_title = "NES",
                                  color_low = "#2166AC", color_mid = "#F7F7F7",
                                  color_high = "#B2182B"),
              theme = "nature")
p1d = hm_label(p1d, "d")

fig1 = (patchwork::wrap_elements(p1a) / p1b / (p1c | p1d)) +
  plot_layout(heights = c(1, 1.15, 1.45))
save_fig(fig1, "sle_fig1.png", 190, 300)

# ===========================================================================
# FIGURE 2 - Integration of immune transcriptome profiles (DIABLO)
# ===========================================================================
perf = load_data("sle_diablo_performance.rda")$sle_diablo_performance
m5   = load_data("sle_diablo_markers.rda")$sle_diablo_markers
names(m5)[names(m5) == "X/A_Ratio_Correlation"] = "XAR_cor"

# total selected markers per model (unique genes, components 1 + 2)
mk = aggregate(Genes ~ Chr_Based_Model, data = m5,
               FUN = function(x) length(unique(x)))
perf = merge(perf, mk, by = "Chr_Based_Model", all.x = TRUE)
perf$Chr_Based_Model = factor(perf$Chr_Based_Model,
                              levels = c(paste0("chr", 1:22), "chrX", "AllChr"))
perf = perf[order(perf$Chr_Based_Model), ]
marker_scale = max(perf$Genes) / max(perf$F1)

# a) model performance: F1 score (bars) + number of markers (points, 2nd axis)
p2a = cliplot(F1 ~ Chr_Based_Model, data = perf,
              type = type_barplot(stat = "identity"),
              theme = "nature", palette = "nejm") +
  labs(x = NULL, y = "F1 score") +
  geom_point(data = perf, aes(x = Chr_Based_Model, y = Genes / marker_scale),
             color = "white", size = 1.7, stroke = 0.4) +
  geom_line(data = perf, aes(x = Chr_Based_Model, y = Genes / marker_scale, group = 1),
            color = "grey25", linewidth = 0.4) +
  scale_y_continuous(limits = c(0, 1), expand = expansion(mult = c(0, 0.06)),
                     sec.axis = sec_axis(~ . * marker_scale,
                                          name = "Selected markers")) +
  cli_panel_label("a") +
  theme(axis.text.x = element_text(angle = 60, hjust = 1, size = 6.5))

# b) percentage of marker genes correlated with X/A-ratio per model
m5$SigCor = m5$Cor_FDR < 0.05 & abs(m5$XAR_cor) > 0.3
m5u = m5[!duplicated(m5[, c("Chr_Based_Model", "Genes")]), ]
corr_pct = aggregate(SigCor ~ Chr_Based_Model, data = m5u,
                     FUN = function(x) 100 * mean(x))
corr_pct$Chr_Based_Model = factor(corr_pct$Chr_Based_Model,
                                  levels = c(paste0("chr", 1:22), "chrX", "AllChr"))
corr_pct = corr_pct[order(corr_pct$Chr_Based_Model), ]
names(corr_pct)[2] = "pct"

p2b = cliplot(pct ~ Chr_Based_Model, data = corr_pct,
              type = type_barplot(stat = "identity"),
              theme = "nature", palette = "nejm") +
  labs(x = NULL, y = "X/A-correlated markers (%)") +
  cli_panel_label("b") +
  theme(axis.text.x = element_text(angle = 60, hjust = 1, size = 6.5))

# c) heatmap of X/A-ratio correlations for Comp1 markers
m5c1 = m5[m5$Component == "Comp1", ]
m5c1$Cell_Type = factor(m5c1$Cell_Type, levels = lineage_order)
g_n  = table(m5c1$Genes)
keep_g = names(g_n)[g_n >= 3]
m5c2 = m5c1[m5c1$Genes %in% keep_g, ]
cor_avg = aggregate(XAR_cor ~ Genes + Cell_Type,
                    data = m5c2, FUN = mean)
mat2c = reshape2::acast(cor_avg, Genes ~ Cell_Type,
                        value.var = "XAR_cor")
mat2c[is.na(mat2c)] = 0
g_mx = apply(abs(mat2c), 1, max)
mat2c = mat2c[order(g_mx, decreasing = TRUE), , drop = FALSE]
mat2c = mat2c[seq_len(min(34, nrow(mat2c))), , drop = FALSE]

p2c = cliplot(mat2c,
              type = type_heatmap(scale = "none", cluster_rows = FALSE,
                                  cluster_cols = FALSE, show_rownames = TRUE,
                                  show_colnames = TRUE,
                                  legend_title = "X/A-ratio corr.",
                                  color_low = "#2166AC", color_mid = "#F7F7F7",
                                  color_high = "#B2182B"),
              theme = "nature")
p2c = hm_label(p2c, "c")

fig2 = (p2a / p2b) | p2c
save_fig(fig2, "sle_fig2.png", 190, 175)

# ===========================================================================
# FIGURE 3 - XIST lncRNA dysregulation across immune cells
# ===========================================================================
# a) XIST expression levels, SLE vs HC, 27 immune cell types, with logFC effect strip
p3a_strip = strip_plot(effect_strip(pat, "XIST_logCPMs",
                          function(s, h) mean(s) - mean(h)), "logFC")
p3a_box = cliplot(XIST_logCPMs ~ Cell_Type | Condition, data = pat,
              type = type_boxplot(add_jitter = TRUE, jitter_size = 0.55,
                                  jitter_alpha = 0.3, width = 0.65),
              theme = "nature", palette = "nejm") +
  labs(x = NULL, y = "XIST expression (log CPMs)") +
  cli_panel_label("a") +
  theme(axis.text.x = element_text(angle = 60, hjust = 1, size = 6.5),
        legend.position = "top",
        legend.title = element_blank())
p3a = (p3a_strip / p3a_box) + plot_layout(heights = c(0.8, 9), axes = "collect")
gxi = load_data("sle_gsea_xist.rda")$sle_gsea_xist
names(gxi)[names(gxi) == "FDR q-val"] = "FDR"
gxi$Cell_Type = factor(gxi$Cell_Type, levels = lineage_order)
go = gxi[gxi$DB == "GO_BP" & gxi$FDR < 0.05, ]
term_n = table(go$Term)
keep_t = names(term_n)[term_n >= 3]
go = go[go$Term %in% keep_t, ]
mean_abs = tapply(abs(go$NES), go$Term, mean)
top_t    = names(sort(mean_abs, decreasing = TRUE))[1:26]
go       = go[go$Term %in% top_t, ]
go$TermShort = short_term(go$Term)

mat3b = reshape2::acast(go, TermShort ~ Cell_Type, value.var = "NES")
mat3b = mat3b[short_term(top_t), , drop = FALSE]
mat3b[is.na(mat3b)] = 0

p3b = cliplot(mat3b,
              type = type_heatmap(scale = "none", cluster_rows = FALSE,
                                  cluster_cols = FALSE, show_rownames = TRUE,
                                  show_colnames = TRUE, legend_title = "NES",
                                  color_low = "#2166AC", color_mid = "#F7F7F7",
                                  color_high = "#B2182B"),
              theme = "nature")
p3b = hm_label(p3b, "b")

# c) intercellular XIST correlations: % positive (up) / negative (down)
icc = load_data("sle_intercellular_cor.rda")$sle_intercellular_cor
icc = icc[icc$FDR < 0.05 & abs(icc$XIST_Correlation) > 0.5, ]
icc$Pos = icc$XIST_Correlation > 0
pos_agg = aggregate(Pos ~ Origin_Cell_Line, data = icc, FUN = mean)
names(pos_agg) = c("Cell", "pos")
pos_agg$neg = 1 - pos_agg$pos
long = rbind(
  data.frame(Cell = pos_agg$Cell, Direction = "Positive",
             value = 100 * pos_agg$pos),
  data.frame(Cell = pos_agg$Cell, Direction = "Negative",
             value = -100 * pos_agg$neg)
)
long$Cell = factor(long$Cell, levels = lineage_order[lineage_order %in% long$Cell])
long$Direction = factor(long$Direction, levels = c("Positive", "Negative"))

p3c = cliplot(value ~ Cell | Direction, data = long,
              type = type_barplot(stat = "identity"),
              theme = "nature", palette = "nejm") +
  geom_hline(yintercept = 0, linewidth = 0.4) +
  scale_y_continuous(labels = function(x) abs(x),
                     limits = c(-100, 100), expand = expansion(mult = c(0, 0.02))) +
  labs(x = NULL, y = "Significant correlations (%)") +
  cli_panel_label("c") +
  theme(axis.text.x = element_text(angle = 60, hjust = 1, size = 6.5),
        legend.position = "top",
        legend.title = element_blank())

fig3 = patchwork::wrap_elements(p3a) / p3b / p3c + plot_layout(heights = c(1, 1.3, 0.9))
save_fig(fig3, "sle_fig3.png", 190, 265)

# ===========================================================================
# FIGURE 4 - XIST-interactome dysregulation across immune cells
# ===========================================================================
# a) heatmap of logFC for XIST-interactome genes (FDR < 0.05; white = NS)
ide = load_data("sle_interactome_de.rda")$sle_interactome_de
ide$Cell_Type = factor(ide$Cell_Type, levels = lineage_order)
ide$logFC_plot = ifelse(ide$FDR < 0.05, ide$logFC, 0)

mat4a = reshape2::acast(ide, Gene ~ Cell_Type, value.var = "logFC_plot")
g_n  = apply(mat4a != 0, 1, sum)
g_mx = apply(abs(mat4a), 1, max)
mat4a = mat4a[order(g_n, g_mx, decreasing = TRUE), , drop = FALSE]
mat4a = mat4a[seq_len(min(46, nrow(mat4a))), , drop = FALSE]

p4a = cliplot(mat4a,
              type = type_heatmap(scale = "none", cluster_rows = FALSE,
                                  cluster_cols = FALSE, show_rownames = TRUE,
                                  show_colnames = TRUE, legend_title = "logFC",
                                  color_low = "#2166AC", color_mid = "#F7F7F7",
                                  color_high = "#B2182B"),
              theme = "nature")
p4a = hm_label(p4a, "a")

# b) row-scaled heatmap of XIST-interactome regression loadings
il = load_data("sle_interactome_loadings.rda")$sle_interactome_loadings
il$Cell_Type = factor(il$Cell_Type, levels = lineage_order)
cat(sprintf("Loading range: %.3g - %.3g\n", min(il$Loadings), max(il$Loadings)))

mat4b = reshape2::acast(il, Gene ~ Cell_Type, value.var = "Loadings")
mat4b[is.na(mat4b)] = 0
g_n  = apply(mat4b != 0, 1, sum)
g_mx = apply(mat4b, 1, max)
mat4b = mat4b[order(g_n, g_mx, decreasing = TRUE), , drop = FALSE]
mat4b = mat4b[seq_len(min(46, nrow(mat4b))), , drop = FALSE]

p4b = cliplot(mat4b,
              type = type_heatmap(scale = "row", cluster_rows = FALSE,
                                  cluster_cols = FALSE, show_rownames = TRUE,
                                  show_colnames = TRUE, legend_title = "row z",
                                  color_low = "#2166AC", color_mid = "#F7F7F7",
                                  color_high = "#B2182B"),
              theme = "nature")
p4b = hm_label(p4b, "b")

fig4 = p4a / p4b + plot_layout(heights = c(1, 1))
save_fig(fig4, "sle_fig4.png", 190, 225)

cat("All 4 SLE paper figures regenerated in", fig_dir, "\n")
