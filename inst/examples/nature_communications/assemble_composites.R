# ==============================================================================
# cliomicplot: Nature Communications (PMC11607458) Multi-Panel Composites
# Figure 2 (panels d, f, g, i) and Figure 7 (panels b, c)
# High-fidelity multi-panel layout matching published article structure
# ==============================================================================

library(cliomicplot)
library(ggplot2)
library(patchwork)
library(readxl)
library(ggrepel)
library(ggsignif)

out_dir <- "C:/Users/lucas/.gemini/antigravity/brain/a8fc441c-e35f-47ad-9d3c-bef516138cce"
inst_dir <- "c:/Users/lucas/OneDrive/Statistics/cliomicplot/inst/examples/nature_communications"
excel_path <- "C:/Users/lucas/Downloads/41467_2024_54685_MOESM4_ESM.xlsx"

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
# 1. Figure 2 Components
# ------------------------------------------------------------------------------

# Panel 2d: CD8 Subsets
raw_2d <- readxl::read_excel(excel_path, sheet = "Figure 2d", skip = 2)
raw_2d <- raw_2d[!is.na(raw_2d$sample) & raw_2d$sample != "sample", ]
cd8_subsets <- c("CD8+ naive", "CD8+ TRM", "GZMB+CD8", "GZMK+CD8", "GNLY+ CD8",
                 "CD8+ MAIT", "NEAT1+ CD8", "CD8+ cycling", "CD8+ Tpex")
for (col in cd8_subsets) raw_2d[[col]] <- as.numeric(raw_2d[[col]])
raw_2d$total <- rowSums(raw_2d[, cd8_subsets], na.rm = TRUE)
raw_2d$Compartment <- ifelse(raw_2d$group %in% c("CBL", "NP-BL"), "Blood", "Nasal tissue")
raw_2d$Compartment <- factor(raw_2d$Compartment, levels = c("Blood", "Nasal tissue"))
sample_order <- raw_2d$sample
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

pal_2d <- c(
  "CD8+ naive"   = "#E377C2",
  "CD8+ TRM"     = "#499894",
  "GZMB+CD8"     = "#DE3533",
  "GZMK+CD8"     = "#F58518",
  "GNLY+ CD8"    = "#FB9A99",
  "CD8+ MAIT"    = "#8C564B",
  "NEAT1+ CD8"   = "#E7BA52",
  "CD8+ cycling" = "#6A3D9A",
  "CD8+ Tpex"    = "#1F78B4"
)

p_2d <- ggplot(df_2d_long, aes(x = Sample, y = Share, fill = Subset)) +
  geom_bar(stat = "identity", position = "stack", width = 0.82, color = "black", linewidth = 0.2) +
  facet_grid(~ Compartment, scales = "free_x", space = "free_x") +
  scale_fill_manual(values = pal_2d, breaks = cd8_subsets, name = "CD8+ T cell subsets") +
  scale_y_continuous(limits = c(0, 100.1), expand = c(0, 0), breaks = seq(0, 100, 25)) +
  labs(tag = "d", x = NULL, y = "Cell proportion (%)") +
  theme_natcommun(base_size = 8.5) +
  theme(
    axis.text.x      = element_text(angle = 45, hjust = 1, vjust = 1, size = 7),
    legend.position  = "right",
    legend.key.size  = unit(0.32, "cm"),
    strip.text       = element_text(size = 9, face = "bold")
  )

# Panel 2f: Volcano / Scatter
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

paper_genes <- c("GZMK", "HLA-DRA", "CMC1", "CST7", "DUSP2", "HLA-DRB1", "HLA-DQA1",
                 "HLA-DPA1", "GZMA", "HLA-DPB1", "OXNAD1", "HOPX", "CD55", "CD7",
                 "KLRC1", "KLRB1", "GZMB")
df_volcano$label <- ifelse(df_volcano$gene %in% paper_genes, df_volcano$gene, "")

p_2f <- ggplot(df_volcano, aes(x = delta_pct, y = logfc, color = status)) +
  geom_point(alpha = 0.75, size = 1.1) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "#E69F00", linewidth = 0.35) +
  geom_hline(yintercept = c(-0.5, 0.5), linetype = "dashed", color = "#E69F00", linewidth = 0.35) +
  geom_text_repel(
    aes(label = label),
    size          = 2.4,
    fontface      = "italic",
    color         = "black",
    max.overlaps  = 25,
    box.padding   = 0.3,
    point.padding = 0.2,
    segment.color = "grey30",
    segment.size  = 0.25
  ) +
  scale_color_manual(values = c("Up" = "#DE3533", "Down" = "#3B75AF", "NS" = "#D0D0D0"),
                     name = expression(italic(P)[adj] < 0.05)) +
  scale_x_continuous(limits = c(-0.35, 0.85), breaks = seq(-0.25, 0.75, 0.25)) +
  scale_y_continuous(limits = c(-1.2, 2.2), breaks = seq(-1, 2, 1)) +
  labs(tag = "f", x = expression(Delta~"Percent of cells"), y = expression("Average"~log[2]*"(Fold change)")) +
  theme_natcommun(base_size = 8.5) +
  theme(legend.position = "right", legend.key.size = unit(0.3, "cm"))

# Panel 2g: GSEA Barplot
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

p_2g <- ggplot(raw_2g, aes(x = NES, y = PathName, fill = negLogP)) +
  geom_bar(stat = "identity", width = 0.68, color = "black", linewidth = 0.25) +
  scale_fill_gradient(low = "#4DBBD5", high = "#E64B35", name = expression(-log[10](italic(P)[adj]))) +
  labs(tag = "g", x = "Normalized Enrichment Score (NES)", y = NULL) +
  theme_natcommun(base_size = 8.5) +
  theme(
    axis.text.y       = element_text(size = 7.5),
    legend.position   = "right",
    legend.key.height = unit(0.4, "cm"),
    legend.key.width  = unit(0.25, "cm")
  )

# Panel 2i: KEGG Dotplot
raw_2i <- readxl::read_excel(excel_path, sheet = "Figure 2i", skip = 1)
raw_2i <- raw_2i[!is.na(raw_2i$Description) & !is.na(raw_2i$Count), ]
raw_2i$GeneRatioNum <- vapply(strsplit(raw_2i$GeneRatio, "/"), function(x) as.numeric(x[1]) / as.numeric(x[2]), numeric(1))
raw_2i$negLogP <- -log10(raw_2i$p.adjust)
top_kegg <- head(raw_2i[order(raw_2i$p.adjust), ], 10)
top_kegg$Description <- factor(top_kegg$Description, levels = top_kegg$Description[order(top_kegg$GeneRatioNum)])

p_2i <- ggplot(top_kegg, aes(x = GeneRatioNum, y = Description)) +
  geom_point(aes(size = Count, color = negLogP)) +
  scale_color_gradient(low = "#FBB4AE", high = "#B2182B", name = expression(-log[10](italic(P)[adj]))) +
  scale_size_continuous(range = c(2.5, 6), name = "Gene counts") +
  labs(tag = "i", x = "Gene ratio", y = NULL) +
  theme_natcommun(base_size = 8.5) +
  theme(
    axis.text.y        = element_text(size = 7.5),
    panel.grid.major.y = element_line(color = "grey90", linewidth = 0.25),
    legend.position    = "right",
    legend.key.height  = unit(0.35, "cm"),
    legend.key.width   = unit(0.25, "cm")
  )

# Assemble Figure 2 multi-panel composite
p_fig2_composite <- (p_2d) / (p_2f | (p_2g / p_2i)) +
  plot_layout(heights = c(1.0, 1.4)) +
  plot_annotation(
    title = "Nature Communications (2024) 15:10413 - Figure 2 Replication",
    subtitle = "Phenotypic enrichment and functional transcriptome of GZMK+CD8+ effector T cells in nasal polyps",
    theme = theme(
      plot.title    = element_text(size = 12, face = "bold", family = "sans"),
      plot.subtitle = element_text(size = 9, color = "grey30", family = "sans")
    )
  )

ggsave(file.path(out_dir, "NatCommun_Figure2_Composite.png"),
       p_fig2_composite, width = 13.5, height = 10.5, dpi = 300)
ggsave(file.path(inst_dir, "NatCommun_Figure2_Composite.png"),
       p_fig2_composite, width = 13.5, height = 10.5, dpi = 300)
cat("NatCommun_Figure2_Composite.png generated.\n")

# ------------------------------------------------------------------------------
# 2. Figure 7 Composite (Panels b and c)
# ------------------------------------------------------------------------------

# Panel 7b: Diversity
raw_7b <- readxl::read_excel(excel_path, sheet = "Figure 7b")
shannon_cbl   <- as.numeric(raw_7b[6, 3:7])
shannon_np_bl <- as.numeric(raw_7b[6, 9:16])
shannon_cit   <- as.numeric(raw_7b[6, 18:21])
shannon_np    <- as.numeric(raw_7b[6, 23:30])

df_7b <- data.frame(
  Group   = factor(c(rep("CBL", 5), rep("NP-BL", 8), rep("CIT", 4), rep("NP", 8)), levels = c("CBL", "NP-BL", "CIT", "NP")),
  Shannon = c(shannon_cbl, shannon_np_bl, shannon_cit, shannon_np)
)

pal_cohorts <- c("CBL" = "#3B75AF", "NP-BL" = "#DE3533", "CIT" = "#2CA02C", "NP" = "#9467BD")

p_7b <- ggplot(df_7b, aes(x = Group, y = Shannon, color = Group)) +
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
  labs(tag = "b", x = NULL, y = "Shannon entropy index") +
  theme_natcommun(base_size = 9) +
  theme(legend.position = "none", axis.text.x = element_text(size = 8.5))

# Panel 7c: Clonal Expansion
raw_7c <- readxl::read_excel(excel_path, sheet = "Figure 7c", skip = 1)
raw_7c <- raw_7c[!is.na(raw_7c$tissue) & !is.na(raw_7c$cloneType) & raw_7c$cloneType != "NA", ]
clone_levels <- c("1", "2-5", "5-10", "10-20", "20-30", "30-50", ">50")
raw_7c$cloneType <- factor(raw_7c$cloneType, levels = rev(clone_levels))
raw_7c$tissue    <- factor(raw_7c$tissue, levels = c("CBL", "NP-BL", "CIT", "NP"))
sub_7c_np <- raw_7c[raw_7c$tissue %in% c("CIT", "NP"), ]
sub_7c_np$proportion <- as.numeric(sub_7c_np$proportion) * 100

pal_tcr <- c(
  "1"     = "#D9D9D9",
  "2-5"   = "#C6DBEF",
  "5-10"  = "#9ECAE1",
  "10-20" = "#6BAED6",
  "20-30" = "#4292C6",
  "30-50" = "#2171B5",
  ">50"   = "#084594"
)

p_7c <- ggplot(sub_7c_np, aes(x = celltype, y = proportion, fill = cloneType)) +
  geom_bar(stat = "identity", position = "stack", width = 0.75, color = "black", linewidth = 0.2) +
  facet_grid(~ tissue) +
  scale_fill_manual(values = pal_tcr, breaks = clone_levels, name = "Clone size") +
  scale_y_continuous(limits = c(0, 100.1), expand = c(0, 0), breaks = seq(0, 100, 25)) +
  labs(tag = "c", x = NULL, y = "Proportion (%)") +
  theme_natcommun(base_size = 9) +
  theme(
    axis.text.x      = element_text(angle = 45, hjust = 1, vjust = 1, size = 7.5),
    legend.position  = "right",
    legend.key.size  = unit(0.35, "cm"),
    strip.text       = element_text(size = 9.5, face = "bold", margin = margin(b = 4))
  )

p_fig7_composite <- (p_7b | p_7c) +
  plot_layout(widths = c(1, 1.45)) +
  plot_annotation(
    title = "Nature Communications (2024) 15:10413 - Figure 7 TCR Repertoire Composite",
    subtitle = "Repertoire alpha diversity reduction and selective clonal hyper-expansion in nasal polyp tissue",
    theme = theme(
      plot.title    = element_text(size = 12, face = "bold", family = "sans"),
      plot.subtitle = element_text(size = 9, color = "grey30", family = "sans")
    )
  )

ggsave(file.path(out_dir, "NatCommun_Figure7_TCR_Composite.png"),
       p_fig7_composite, width = 13, height = 5.6, dpi = 300)
ggsave(file.path(inst_dir, "NatCommun_Figure7_TCR_Composite.png"),
       p_fig7_composite, width = 13, height = 5.6, dpi = 300)
cat("NatCommun_Figure7_TCR_Composite.png generated.\n")

cat("\nALL COMPOSITES GENERATED SUCCESSFULLY!\n")
