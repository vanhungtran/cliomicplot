# ==============================================================================
# cliomicplot: Nature Communications (PMC11607458) Replication Figures
# Dataset: 41467_2024_54685_MOESM4_ESM.xlsx
# Study: Granzyme K+ CD8+ T cells promote neutrophilic inflammation in nasal polyps
# High-fidelity reproduction matching published styling, palettes, tags, and layouts
# ==============================================================================

library(cliomicplot)
library(ggplot2)
library(readxl)
library(patchwork)
library(ggsignif)
library(ggrepel)

excel_path <- "C:/Users/lucas/Downloads/41467_2024_54685_MOESM4_ESM.xlsx"
out_dir <- "C:/Users/lucas/.gemini/antigravity/brain/a8fc441c-e35f-47ad-9d3c-bef516138cce"
inst_dir <- "c:/Users/lucas/OneDrive/Statistics/cliomicplot/inst/examples/nature_communications"

cat("Loading and processing Nature Communications data...\n")

# Common theme specification matching Nature Communications
theme_natcommun <- function(base_size = 9, base_family = "sans") {
  clitheme(preset = "nature", base_size = base_size, base_family = base_family) +
    theme(
      axis.line         = element_line(color = "black", linewidth = 0.4),
      axis.ticks        = element_line(color = "black", linewidth = 0.4),
      axis.ticks.length = unit(0.12, "cm"),
      axis.title        = element_text(size = base_size, face = "plain", color = "black"),
      axis.text         = element_text(size = base_size - 1, color = "black"),
      plot.tag          = element_text(size = base_size + 3, face = "bold", family = base_family),
      plot.tag.position = c(0, 1),
      plot.margin       = margin(6, 6, 6, 6),
      panel.grid.major  = element_blank(),
      panel.grid.minor  = element_blank(),
      legend.title      = element_text(size = base_size - 1, face = "bold"),
      legend.text       = element_text(size = base_size - 1.5),
      strip.background  = element_blank(),
      strip.text        = element_text(size = base_size, face = "bold", color = "black")
    )
}

# ------------------------------------------------------------------------------
# 1. Figure 1e: Major Immune Lineage Composition Across Blood and Nasal Tissue
# ------------------------------------------------------------------------------
cat("Processing Figure 1e (Lineage Composition)...\n")
raw_1e <- readxl::read_excel(excel_path, sheet = "Figure 1e", skip = 2)
raw_1e <- raw_1e[!is.na(raw_1e$sample) & raw_1e$sample != "sample", ]

lineage_cols <- c("CD4+T", "CD8+T", "γδT", "NK", "B cells", "ILC2", "Myeloid cells")
for (col in lineage_cols) raw_1e[[col]] <- as.numeric(raw_1e[[col]])
raw_1e$total <- rowSums(raw_1e[, lineage_cols], na.rm = TRUE)

raw_1e$Compartment <- ifelse(raw_1e$group %in% c("CBL", "NP-BL"), "Blood", "Nasal tissue")
raw_1e$Compartment <- factor(raw_1e$Compartment, levels = c("Blood", "Nasal tissue"))

sample_order <- raw_1e$sample
raw_1e$sample <- factor(raw_1e$sample, levels = sample_order)

df_1e_long <- do.call(rbind, lapply(lineage_cols, function(lin) {
  data.frame(
    Sample      = raw_1e$sample,
    Group       = factor(raw_1e$group, levels = c("CBL", "NP-BL", "CIT", "NP")),
    Compartment = raw_1e$Compartment,
    Lineage     = factor(lin, levels = rev(lineage_cols)),
    Count       = raw_1e[[lin]],
    Share       = (raw_1e[[lin]] / raw_1e$total) * 100
  )
}))

# Exact published lineage colors
pal_1e <- c(
  "CD4+T"         = "#3B75AF", # sky / cerulean blue
  "CD8+T"         = "#DE3533", # crimson / bright red
  "γδT"           = "#F58518", # bright orange
  "NK"            = "#92C650", # chartreuse / yellow-green
  "B cells"       = "#2CA02C", # medium emerald green
  "ILC2"          = "#9467BD", # medium purple / violet
  "Myeloid cells" = "#E377C2"  # rose-pink / magenta
)

p_fig1e <- ggplot(df_1e_long, aes(x = Sample, y = Share, fill = Lineage)) +
  geom_bar(stat = "identity", position = "stack", width = 0.82, color = "black", linewidth = 0.2) +
  facet_grid(~ Compartment, scales = "free_x", space = "free_x") +
  scale_fill_manual(values = pal_1e, breaks = lineage_cols, name = "Major lineages") +
  scale_y_continuous(limits = c(0, 100.1), expand = c(0, 0), breaks = seq(0, 100, 25)) +
  labs(
    tag  = "e",
    x    = NULL,
    y    = "Cell proportion (%)"
  ) +
  theme_natcommun(base_size = 9) +
  theme(
    axis.text.x      = element_text(angle = 45, hjust = 1, vjust = 1, size = 7.5),
    legend.position  = "right",
    legend.key.size  = unit(0.35, "cm"),
    strip.text       = element_text(size = 9.5, face = "bold", margin = margin(b = 4))
  )

ggsave(file.path(out_dir, "NatCommun_Figure1e_Lineage_Composition.png"),
       p_fig1e, width = 9.2, height = 4.5, dpi = 300)
ggsave(file.path(inst_dir, "NatCommun_Figure1e_Lineage_Composition.png"),
       p_fig1e, width = 9.2, height = 4.5, dpi = 300)
cat("Figure 1e generated.\n")

# ------------------------------------------------------------------------------
# 2. Figure 2d: CD8+ T Cell Subcluster Dynamics Across Compartments
# ------------------------------------------------------------------------------
cat("Processing Figure 2d (CD8 Subsets)...\n")
raw_2d <- readxl::read_excel(excel_path, sheet = "Figure 2d", skip = 2)
raw_2d <- raw_2d[!is.na(raw_2d$sample) & raw_2d$sample != "sample", ]

cd8_subsets <- c("CD8+ naive", "CD8+ TRM", "GZMB+CD8", "GZMK+CD8", "GNLY+ CD8",
                 "CD8+ MAIT", "NEAT1+ CD8", "CD8+ cycling", "CD8+ Tpex")
for (col in cd8_subsets) raw_2d[[col]] <- as.numeric(raw_2d[[col]])
raw_2d$total <- rowSums(raw_2d[, cd8_subsets], na.rm = TRUE)

raw_2d$Compartment <- ifelse(raw_2d$group %in% c("CBL", "NP-BL"), "Blood", "Nasal tissue")
raw_2d$Compartment <- factor(raw_2d$Compartment, levels = c("Blood", "Nasal tissue"))
raw_2d$sample <- factor(raw_2d$sample, levels = sample_order)

df_2d_long <- do.call(rbind, lapply(cd8_subsets, function(sub) {
  data.frame(
    Sample      = raw_2d$sample,
    Group       = factor(raw_2d$group, levels = c("CBL", "NP-BL", "CIT", "NP")),
    Compartment = raw_2d$Compartment,
    Subset      = factor(sub, levels = rev(cd8_subsets)),
    Count       = raw_2d[[sub]],
    Share       = (raw_2d[[sub]] / raw_2d$total) * 100
  )
}))

# Exact 9 published CD8 subset colors
pal_2d <- c(
  "CD8+ naive"   = "#E377C2", # magenta / hot pink-purple
  "CD8+ TRM"     = "#499894", # teal / slate-cyan
  "GZMB+CD8"     = "#DE3533", # bright red
  "GZMK+CD8"     = "#F58518", # bright orange
  "GNLY+ CD8"    = "#FB9A99", # coral / salmon pink
  "CD8+ MAIT"    = "#8C564B", # olive-brown / khaki-bronze
  "NEAT1+ CD8"   = "#E7BA52", # golden yellow
  "CD8+ cycling" = "#6A3D9A", # deep violet / dark purple
  "CD8+ Tpex"    = "#1F78B4"  # dark petrol / deep teal-blue
)

p_fig2d <- ggplot(df_2d_long, aes(x = Sample, y = Share, fill = Subset)) +
  geom_bar(stat = "identity", position = "stack", width = 0.82, color = "black", linewidth = 0.2) +
  facet_grid(~ Compartment, scales = "free_x", space = "free_x") +
  scale_fill_manual(values = pal_2d, breaks = cd8_subsets, name = "CD8+ T cell subsets") +
  scale_y_continuous(limits = c(0, 100.1), expand = c(0, 0), breaks = seq(0, 100, 25)) +
  labs(
    tag  = "d",
    x    = NULL,
    y    = "Cell proportion (%)"
  ) +
  theme_natcommun(base_size = 9) +
  theme(
    axis.text.x      = element_text(angle = 45, hjust = 1, vjust = 1, size = 7.5),
    legend.position  = "right",
    legend.key.size  = unit(0.35, "cm"),
    strip.text       = element_text(size = 9.5, face = "bold", margin = margin(b = 4))
  )

ggsave(file.path(out_dir, "NatCommun_Figure2d_CD8_Subsets.png"),
       p_fig2d, width = 9.2, height = 4.5, dpi = 300)
ggsave(file.path(inst_dir, "NatCommun_Figure2d_CD8_Subsets.png"),
       p_fig2d, width = 9.2, height = 4.5, dpi = 300)
cat("Figure 2d generated.\n")

# ------------------------------------------------------------------------------
# 3. Figure 2f: Single-Cell Differential Expression Scatter Plot (GZMK vs Others)
# ------------------------------------------------------------------------------
cat("Processing Figure 2f (Volcano / Scatter)...\n")
raw_2f <- readxl::read_excel(excel_path, sheet = "Figure 2f", skip = 1)
raw_2f <- raw_2f[!is.na(raw_2f$gene) & !is.na(raw_2f$avg_log2FC), ]

df_volcano <- data.frame(
  gene       = raw_2f$gene,
  logfc      = as.numeric(raw_2f$avg_log2FC),
  pvalue     = as.numeric(raw_2f$p_val_adj),
  pct.1      = as.numeric(raw_2f$pct.1),
  pct.2      = as.numeric(raw_2f$pct.2)
)
df_volcano$delta_pct <- df_volcano$pct.1 - df_volcano$pct.2

df_volcano$status <- "NS"
df_volcano$status[df_volcano$logfc > 0.5 & df_volcano$pvalue < 0.05] <- "Up"
df_volcano$status[df_volcano$logfc < -0.5 & df_volcano$pvalue < 0.05] <- "Down"
df_volcano$status <- factor(df_volcano$status, levels = c("Up", "Down", "NS"))

df_volcano <- df_volcano[order(-abs(df_volcano$logfc)), ]
df_volcano <- df_volcano[!duplicated(df_volcano$gene), ]

# Genes highlighted in published figure
paper_genes <- c("GZMK", "HLA-DRA", "CMC1", "CST7", "DUSP2", "HLA-DRB1", "HLA-DQA1",
                 "HLA-DPA1", "GZMA", "HLA-DPB1", "OXNAD1", "HOPX", "CD55", "CD7",
                 "KLRC1", "KLRB1", "GZMB")
df_volcano$label <- ifelse(df_volcano$gene %in% paper_genes, df_volcano$gene, "")

pal_volcano <- c("Up" = "#DE3533", "Down" = "#3B75AF", "NS" = "#D0D0D0")

p_fig2f <- ggplot(df_volcano, aes(x = delta_pct, y = logfc, color = status)) +
  geom_point(alpha = 0.75, size = 1.3) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "#E69F00", linewidth = 0.4) +
  geom_hline(yintercept = c(-0.5, 0.5), linetype = "dashed", color = "#E69F00", linewidth = 0.4) +
  geom_text_repel(
    aes(label = label),
    size          = 2.8,
    fontface      = "italic",
    color         = "black",
    max.overlaps  = 30,
    box.padding   = 0.35,
    point.padding = 0.25,
    segment.color = "grey30",
    segment.size  = 0.3
  ) +
  scale_color_manual(values = pal_volcano, name = expression(italic(P)[adj] < 0.05), labels = c("Up", "Down", "NS")) +
  scale_x_continuous(limits = c(-0.35, 0.85), breaks = seq(-0.25, 0.75, 0.25)) +
  scale_y_continuous(limits = c(-1.2, 2.2), breaks = seq(-1, 2, 1)) +
  labs(
    tag  = "f",
    x    = expression(Delta~"Percent of cells"),
    y    = expression("Average"~log[2]*"(Fold change)")
  ) +
  theme_natcommun(base_size = 9) +
  theme(
    legend.position = "right",
    legend.title    = element_text(size = 8, face = "bold"),
    legend.text     = element_text(size = 7.5)
  )

ggsave(file.path(out_dir, "NatCommun_Figure2f_Volcano_CD8_GZMK.png"),
       p_fig2f, width = 6.2, height = 4.8, dpi = 300)
ggsave(file.path(inst_dir, "NatCommun_Figure2f_Volcano_CD8_GZMK.png"),
       p_fig2f, width = 6.2, height = 4.8, dpi = 300)
cat("Figure 2f generated.\n")

# ------------------------------------------------------------------------------
# 4. Figure 2g: GSEA Reactome & Immune Pathway Enrichment
# ------------------------------------------------------------------------------
cat("Processing Figure 2g (GSEA Pathways)...\n")
raw_2g <- readxl::read_excel(excel_path, sheet = "Figure 2g", skip = 1)
raw_2g <- raw_2g[!is.na(raw_2g$NES), ]

clean_pathway <- function(x) {
  x <- gsub("^REACTOME_", "", x)
  x <- gsub("^SANA_", "", x)
  x <- gsub("^BASSO_", "", x)
  x <- gsub("_", " ", x)
  tools::toTitleCase(tolower(x))
}

raw_2g$PathName <- clean_pathway(raw_2g$Description)
raw_2g$NES      <- as.numeric(raw_2g$NES)
raw_2g$p.adjust <- as.numeric(raw_2g$p.adjust)
raw_2g$negLogP  <- -log10(raw_2g$p.adjust)
raw_2g$PathName <- factor(raw_2g$PathName, levels = raw_2g$PathName[order(raw_2g$NES)])

p_fig2g <- ggplot(raw_2g, aes(x = NES, y = PathName, fill = negLogP)) +
  geom_bar(stat = "identity", width = 0.68, color = "black", linewidth = 0.3) +
  scale_fill_gradient(low = "#4DBBD5", high = "#E64B35", name = expression(-log[10](italic(P)[adj]))) +
  labs(
    tag  = "g",
    x    = "Normalized Enrichment Score (NES)",
    y    = NULL
  ) +
  theme_natcommun(base_size = 9) +
  theme(
    axis.text.y       = element_text(size = 8),
    legend.position   = "right",
    legend.key.height = unit(0.5, "cm"),
    legend.key.width  = unit(0.3, "cm")
  )

ggsave(file.path(out_dir, "NatCommun_Figure2g_GSEA_Enrichment.png"),
       p_fig2g, width = 7.5, height = 4.5, dpi = 300)
ggsave(file.path(inst_dir, "NatCommun_Figure2g_GSEA_Enrichment.png"),
       p_fig2g, width = 7.5, height = 4.5, dpi = 300)
cat("Figure 2g generated.\n")

# ------------------------------------------------------------------------------
# 5. Figure 2i: Immunological Disease Association Dotplot
# ------------------------------------------------------------------------------
cat("Processing Figure 2i (KEGG Disease Dotplot)...\n")
raw_2i <- readxl::read_excel(excel_path, sheet = "Figure 2i", skip = 1)
raw_2i <- raw_2i[!is.na(raw_2i$Description) & !is.na(raw_2i$Count), ]

raw_2i$GeneRatioNum <- vapply(strsplit(raw_2i$GeneRatio, "/"), function(x) as.numeric(x[1]) / as.numeric(x[2]), numeric(1))
raw_2i$negLogP <- -log10(raw_2i$p.adjust)

top_kegg <- head(raw_2i[order(raw_2i$p.adjust), ], 12)
top_kegg$Description <- factor(top_kegg$Description, levels = top_kegg$Description[order(top_kegg$GeneRatioNum)])

p_fig2i <- ggplot(top_kegg, aes(x = GeneRatioNum, y = Description)) +
  geom_point(aes(size = Count, color = negLogP)) +
  scale_color_gradient(low = "#FBB4AE", high = "#B2182B", name = expression(-log[10](italic(P)[adj]))) +
  scale_size_continuous(range = c(2.8, 6.5), name = "Gene counts") +
  labs(
    tag  = "i",
    x    = "Gene ratio",
    y    = NULL
  ) +
  theme_natcommun(base_size = 9) +
  theme(
    axis.text.y        = element_text(size = 8),
    panel.grid.major.y = element_line(color = "grey90", linewidth = 0.25),
    legend.position    = "right",
    legend.key.height  = unit(0.45, "cm"),
    legend.key.width   = unit(0.3, "cm")
  )

ggsave(file.path(out_dir, "NatCommun_Figure2i_KEGG_Dotplot.png"),
       p_fig2i, width = 7.0, height = 4.6, dpi = 300)
ggsave(file.path(inst_dir, "NatCommun_Figure2i_KEGG_Dotplot.png"),
       p_fig2i, width = 7.0, height = 4.6, dpi = 300)
cat("Figure 2i generated.\n")

# ------------------------------------------------------------------------------
# 6. Figure 5h: GZMK vs GZMB Proportions in CD8+ T Cells (CIT vs NP)
# ------------------------------------------------------------------------------
cat("Processing Figure 5h (Visium HD / Proportion in CD8+ T Cells)...\n")
raw_5h <- readxl::read_excel(excel_path, sheet = "Figure 5h")

gzmk_cit <- as.numeric(raw_5h[2, 4:7])
gzmk_np  <- as.numeric(raw_5h[2, 9:14])
gzmb_cit <- as.numeric(raw_5h[3, 4:7])
gzmb_np  <- as.numeric(raw_5h[3, 9:14])

df_5h <- data.frame(
  Tissue    = factor(c(rep("CIT", 4), rep("NP", 6), rep("CIT", 4), rep("NP", 6)), levels = c("CIT", "NP")),
  CellType  = factor(c(rep("CD8T_GZMK", 10), rep("CD8T_GZMB", 10)), levels = c("CD8T_GZMK", "CD8T_GZMB")),
  Frequency = c(gzmk_cit, gzmk_np, gzmb_cit, gzmb_np)
)

# Exact published colors: bright orange for GZMK, beige/taupe for GZMB
pal_5h <- c("CD8T_GZMK" = "#F58518", "CD8T_GZMB" = "#D3B89D")

p_fig5h <- ggplot(df_5h, aes(x = Tissue, y = Frequency, fill = CellType)) +
  geom_boxplot(width = 0.52, outlier.shape = NA, alpha = 0.85, color = "black", linewidth = 0.4) +
  geom_jitter(width = 0.12, size = 1.8, color = "black", alpha = 0.9) +
  facet_wrap(~ CellType, scales = "free_y") +
  scale_fill_manual(values = pal_5h) +
  geom_signif(
    comparisons = list(c("CIT", "NP")),
    annotations = c("P = 0.0667"),
    data        = subset(df_5h, CellType == "CD8T_GZMK"),
    y_position  = max(df_5h$Frequency[df_5h$CellType == "CD8T_GZMK"]) * 1.08,
    textsize    = 2.8,
    tip_length  = 0,
    color       = "black",
    vjust       = -0.2
  ) +
  geom_signif(
    comparisons = list(c("CIT", "NP")),
    annotations = c("ns"),
    data        = subset(df_5h, CellType == "CD8T_GZMB"),
    y_position  = max(df_5h$Frequency[df_5h$CellType == "CD8T_GZMB"]) * 1.08,
    textsize    = 2.8,
    tip_length  = 0,
    color       = "black",
    vjust       = -0.2
  ) +
  labs(
    tag  = "h",
    x    = NULL,
    y    = expression("% of"~"CD8"^"+"~"T cells")
  ) +
  theme_natcommun(base_size = 9) +
  theme(
    legend.position = "none",
    strip.text      = element_text(size = 9.5, face = "bold")
  )

ggsave(file.path(out_dir, "NatCommun_Figure5h_GZMK_vs_GZMB.png"),
       p_fig5h, width = 6.8, height = 4.5, dpi = 300)
ggsave(file.path(inst_dir, "NatCommun_Figure5h_GZMK_vs_GZMB.png"),
       p_fig5h, width = 6.8, height = 4.5, dpi = 300)
cat("Figure 5h generated.\n")

# ------------------------------------------------------------------------------
# 7. Figure 7b: TCR Repertoire Alpha Diversity Across Compartments
# ------------------------------------------------------------------------------
cat("Processing Figure 7b (TCR Diversity)...\n")
raw_7b <- readxl::read_excel(excel_path, sheet = "Figure 7b")

shannon_cbl   <- as.numeric(raw_7b[6, 3:7])
shannon_np_bl <- as.numeric(raw_7b[6, 9:16])
shannon_cit   <- as.numeric(raw_7b[6, 18:21])
shannon_np    <- as.numeric(raw_7b[6, 23:30])

df_7b_shannon <- data.frame(
  Group   = factor(
    c(rep("CBL", length(shannon_cbl)),
      rep("NP-BL", length(shannon_np_bl)),
      rep("CIT", length(shannon_cit)),
      rep("NP", length(shannon_np))),
    levels = c("CBL", "NP-BL", "CIT", "NP")
  ),
  Shannon = c(shannon_cbl, shannon_np_bl, shannon_cit, shannon_np)
)

pal_cohorts <- c("CBL" = "#3B75AF", "NP-BL" = "#DE3533", "CIT" = "#2CA02C", "NP" = "#9467BD")

p_fig7b <- ggplot(df_7b_shannon, aes(x = Group, y = Shannon, color = Group)) +
  geom_boxplot(width = 0.52, fill = "white", outlier.shape = NA, linewidth = 0.55) +
  geom_jitter(width = 0.15, size = 1.8, alpha = 0.85) +
  scale_color_manual(values = pal_cohorts) +
  geom_signif(
    comparisons = list(c("CBL", "CIT"), c("NP-BL", "CIT")),
    annotations = c("P = 0.0381", "P = 0.0356"),
    y_position  = c(6.2, 5.8),
    textsize    = 2.8,
    tip_length  = 0,
    color       = "black"
  ) +
  scale_y_continuous(limits = c(4.0, 6.5), breaks = seq(4.0, 6.5, 0.5)) +
  labs(
    tag  = "b",
    x    = NULL,
    y    = "Shannon entropy index"
  ) +
  theme_natcommun(base_size = 9) +
  theme(
    legend.position = "none",
    axis.text.x     = element_text(size = 8.5)
  )

ggsave(file.path(out_dir, "NatCommun_Figure7b_TCR_Diversity.png"),
       p_fig7b, width = 5.8, height = 4.5, dpi = 300)
ggsave(file.path(inst_dir, "NatCommun_Figure7b_TCR_Diversity.png"),
       p_fig7b, width = 5.8, height = 4.5, dpi = 300)
cat("Figure 7b generated.\n")

# ------------------------------------------------------------------------------
# 8. Figure 7c: Clonal Expansion Hierarchy Across T Cell Subsets
# ------------------------------------------------------------------------------
cat("Processing Figure 7c (TCR Clonal Expansion)...\n")
raw_7c <- readxl::read_excel(excel_path, sheet = "Figure 7c", skip = 1)
raw_7c <- raw_7c[!is.na(raw_7c$tissue) & !is.na(raw_7c$cloneType), ]

clone_levels <- c("1", "2-5", "5-10", "10-20", "20-30", "30-50", ">50")
raw_7c$cloneType <- factor(raw_7c$cloneType, levels = rev(clone_levels))
raw_7c$tissue    <- factor(raw_7c$tissue, levels = c("CBL", "NP-BL", "CIT", "NP"))

sub_7c_np <- raw_7c[raw_7c$tissue %in% c("CIT", "NP"), ]
sub_7c_np$proportion <- as.numeric(sub_7c_np$proportion) * 100

# Sequential light-grey-to-dark-navy gradient
pal_tcr <- c(
  "1"     = "#D9D9D9", # light cool grey
  "2-5"   = "#C6DBEF", # pale ice blue
  "5-10"  = "#9ECAE1", # light blue
  "10-20" = "#6BAED6", # medium sky blue
  "20-30" = "#4292C6", # royal blue
  "30-50" = "#2171B5", # deep blue
  ">50"   = "#084594"  # dark navy blue
)

p_fig7c <- ggplot(sub_7c_np, aes(x = celltype, y = proportion, fill = cloneType)) +
  geom_bar(stat = "identity", position = "stack", width = 0.75, color = "black", linewidth = 0.2) +
  facet_grid(~ tissue) +
  scale_fill_manual(values = pal_tcr, breaks = clone_levels, name = "Clone size") +
  scale_y_continuous(limits = c(0, 100.1), expand = c(0, 0), breaks = seq(0, 100, 25)) +
  labs(
    tag  = "c",
    x    = NULL,
    y    = "Proportion (%)"
  ) +
  theme_natcommun(base_size = 9) +
  theme(
    axis.text.x      = element_text(angle = 45, hjust = 1, vjust = 1, size = 7.5),
    legend.position  = "right",
    legend.key.size  = unit(0.35, "cm"),
    strip.text       = element_text(size = 9.5, face = "bold", margin = margin(b = 4))
  )

ggsave(file.path(out_dir, "NatCommun_Figure7c_Clonal_Expansion.png"),
       p_fig7c, width = 8.8, height = 4.5, dpi = 300)
ggsave(file.path(inst_dir, "NatCommun_Figure7c_Clonal_Expansion.png"),
       p_fig7c, width = 8.8, height = 4.5, dpi = 300)
cat("Figure 7c generated.\n")

cat("\nALL NATURE COMMUNICATIONS FIGURES GENERATED SUCCESSFULLY!\n")
