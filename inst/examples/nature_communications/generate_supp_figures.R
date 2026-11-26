# ==============================================================================
# Nature Communications (2024) 15:10413
# Granzyme K+ CD8+ T cells interact with fibroblasts to promote neutrophilic
# inflammation in nasal polyps
#
# Supplementary Figures: Figure S2c, S3c, S4c, S4d, S11j, S11k, S12c
# ==============================================================================

suppressPackageStartupMessages({
  library(cliomicplot)
  library(ggplot2)
  library(readxl)
  library(dplyr)
  library(tidyr)
  library(patchwork)
})

excel_file <- "C:/Users/lucas/Downloads/41467_2024_54685_MOESM4_ESM.xlsx"
out_dir <- "c:/Users/lucas/OneDrive/Statistics/cliomicplot/inst/examples/nature_communications"
brain_dir <- "C:/Users/lucas/.gemini/antigravity/brain/a8fc441c-e35f-47ad-9d3c-bef516138cce"

theme_natcom <- function(base_size = 8) {
  theme_classic(base_size = base_size, base_family = "sans") +
    theme(
      axis.line = element_line(color = "black", linewidth = 0.4),
      axis.ticks = element_line(color = "black", linewidth = 0.4),
      axis.ticks.length = unit(0.12, "cm"),
      axis.title = element_text(size = rel(1.0), color = "black"),
      axis.text = element_text(size = rel(0.85), color = "black"),
      plot.title = element_text(size = rel(1.05), face = "bold", hjust = 0),
      plot.subtitle = element_text(size = rel(0.85), color = "grey30", hjust = 0),
      plot.tag = element_text(size = rel(1.4), face = "bold", hjust = 0, vjust = 1),
      legend.title = element_text(size = rel(0.9), face = "bold"),
      legend.text = element_text(size = rel(0.85)),
      legend.position = "right",
      legend.key.size = unit(0.35, "cm"),
      strip.background = element_blank(),
      strip.text = element_text(size = rel(0.95), face = "bold")
    )
}

# ------------------------------------------------------------------------------
# 1. Figure S2c: Lineage proportions across 25 donors
# ------------------------------------------------------------------------------
message("Processing Figure S2c (Lineage Proportions)...")
raw_s2c <- read_excel(excel_file, sheet = "Figure S2c", skip = 1)
raw_s2c <- raw_s2c[!is.na(raw_s2c$sample) & raw_s2c$sample != "sample", ]

lineage_cols <- c("CD4Tratio", "CD8Tratio", "γδTratio", "Nkratio", "B cellsratio", "ILC2ratio", "Myeloid cellsratio")
clean_lineages <- c("CD4+ T", "CD8+ T", "γδ T", "NK", "B cells", "ILC2", "Myeloid")

df_s2c_list <- list()
for (i in seq_along(lineage_cols)) {
  col <- lineage_cols[i]
  df_s2c_list[[col]] <- data.frame(
    sample = raw_s2c$sample,
    group  = raw_s2c$group,
    lineage = clean_lineages[i],
    ratio   = as.numeric(raw_s2c[[col]]) * 100
  )
}
df_s2c <- bind_rows(df_s2c_list)
df_s2c$group <- factor(df_s2c$group, levels = c("CBL", "NP-BL", "CIT", "NP"))
df_s2c$lineage <- factor(df_s2c$lineage, levels = clean_lineages)

p_s2c <- ggplot(df_s2c, aes(x = group, y = ratio, fill = group)) +
  geom_boxplot(width = 0.6, outlier.shape = NA, linewidth = 0.35, alpha = 0.85) +
  geom_jitter(width = 0.15, size = 1.0, shape = 21, fill = "white", stroke = 0.35) +
  facet_wrap(~ lineage, scales = "free_y", nrow = 2) +
  scale_fill_manual(values = c("CBL" = "#1D35B8", "NP-BL" = "#E01A1A", "CIT" = "#1B8A2B", "NP" = "#701B8C"), guide = "none") +
  labs(
    tag = "c",
    title = "Supplementary Figure 2c | Immune cell lineage proportions across sampling compartments",
    x = NULL,
    y = "Proportion (% of CD45+ cells)"
  ) +
  theme_natcom(base_size = 7.5) +
  theme(axis.text.x = element_text(angle = 35, hjust = 1))

# ------------------------------------------------------------------------------
# 2. Figure S3c: CD8+ T cell subsets across 25 donors
# ------------------------------------------------------------------------------
message("Processing Figure S3c (CD8 Subsets)...")
raw_s3c <- read_excel(excel_file, sheet = "Figure S3c", skip = 1)
raw_s3c <- raw_s3c[!is.na(raw_s3c$sample) & raw_s3c$sample != "sample", ]

sub_cols <- c("CD8+ naïve ratio", "CD8+ TRM ratio", "GZMB+CD8 ratio", "GZMK+CD8 ratio", "GNLY+ CD8 ratio", "CD8+ MAITratio", "NEAT1+ CD8 ratio", "CD8+ cycling ratio", "CD8+ Tpex ratio")
clean_sub <- c("CD8+ naive", "CD8+ TRM", "GZMB+CD8", "GZMK+CD8", "GNLY+ CD8", "CD8+ MAIT", "NEAT1+ CD8", "CD8+ cycling", "CD8+ Tpex")

df_s3c_list <- list()
for (i in seq_along(sub_cols)) {
  col <- sub_cols[i]
  df_s3c_list[[col]] <- data.frame(
    sample = raw_s3c$sample,
    group  = raw_s3c$group,
    subset = clean_sub[i],
    ratio  = as.numeric(raw_s3c[[col]]) * 100
  )
}
df_s3c <- bind_rows(df_s3c_list)
df_s3c$group <- factor(df_s3c$group, levels = c("CBL", "NP-BL", "CIT", "NP"))
df_s3c$subset <- factor(df_s3c$subset, levels = clean_sub)

p_s3c <- ggplot(df_s3c, aes(x = group, y = ratio, fill = group)) +
  geom_boxplot(width = 0.6, outlier.shape = NA, linewidth = 0.35, alpha = 0.85) +
  geom_jitter(width = 0.15, size = 1.0, shape = 21, fill = "white", stroke = 0.35) +
  facet_wrap(~ subset, scales = "free_y", nrow = 3) +
  scale_fill_manual(values = c("CBL" = "#1D35B8", "NP-BL" = "#E01A1A", "CIT" = "#1B8A2B", "NP" = "#701B8C"), guide = "none") +
  labs(
    tag = "c",
    title = "Supplementary Figure 3c | CD8+ T cell subset frequencies across 4 clinical groups",
    x = NULL,
    y = expression("Proportion (% of CD8"^{"+"}~"T cells)")
  ) +
  theme_natcom(base_size = 7.5) +
  theme(axis.text.x = element_text(angle = 35, hjust = 1))

# ------------------------------------------------------------------------------
# 3. Figure S12c: STARTRAC expansion score
# ------------------------------------------------------------------------------
message("Processing Figure S12c (STARTRAC Expansion)...")
raw_s12c <- read_excel(excel_file, sheet = "Figure S12c", skip = 1)
colnames(raw_s12c) <- c("group", "score")
raw_s12c$score <- as.numeric(raw_s12c$score)
raw_s12c$group <- factor(raw_s12c$group, levels = c("CBL", "NP-BL", "CIT", "NP"))

p_s12c <- ggplot(raw_s12c, aes(x = group, y = score, fill = group)) +
  geom_boxplot(width = 0.55, outlier.shape = NA, linewidth = 0.4, alpha = 0.85) +
  geom_jitter(width = 0.18, size = 1.4, shape = 21, fill = "white", stroke = 0.4) +
  scale_fill_manual(values = c("CBL" = "#1D35B8", "NP-BL" = "#E01A1A", "CIT" = "#1B8A2B", "NP" = "#701B8C"), guide = "none") +
  labs(
    tag = "c",
    title = "Supplementary Figure 12c | STARTRAC expansion",
    x = NULL,
    y = "STARTRAC-expa score"
  ) +
  theme_natcom()

# ------------------------------------------------------------------------------
# Save Figures
# ------------------------------------------------------------------------------
save_panel <- function(p, filename, width, height) {
  p_path <- file.path(out_dir, filename)
  b_path <- file.path(brain_dir, filename)
  ggsave(p_path, p, width = width, height = height, dpi = 300)
  ggsave(b_path, p, width = width, height = height, dpi = 300)
}

save_panel(p_s2c, "NatCommun_FigureS2c_Lineages.png", 8.8, 5.0)
save_panel(p_s3c, "NatCommun_FigureS3c_CD8_Subsets.png", 9.2, 6.8)
save_panel(p_s12c, "NatCommun_FigureS12c_STARTRAC_Exp.png", 3.8, 3.6)

message("ALL SUPPLEMENTARY FIGURES GENERATED SUCCESSFULLY!")
