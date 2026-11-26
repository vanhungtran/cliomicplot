# ==============================================================================
# Nature Communications (2024) 15:10413
# Granzyme K+ CD8+ T cells interact with fibroblasts to promote neutrophilic
# inflammation in nasal polyps
#
# Figure 8: Expansion of GZMK+CD8+ T cells in chronic inflammatory diseases
# Panel: 8e Cross-tissue comparison (10 inflammatory conditions including Skin AD)
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
      legend.position = "top",
      legend.justification = "left",
      legend.key.size = unit(0.35, "cm"),
      strip.background = element_blank(),
      strip.text = element_text(size = rel(0.95), face = "bold")
    )
}

message("Processing Figure 8e (Cross-Tissue Chronic Inflammatory Disease Analysis)...")
raw_8e <- read_excel(excel_file, sheet = "Figure 8e", col_names = FALSE)

# Definitions of tissue blocks in Figure 8e
blocks_8e <- list(
  list(tissue = "Skin", cols = 2:4),
  list(tissue = "Esophagus", cols = 6:8),
  list(tissue = "Duodenum", cols = 9:11),
  list(tissue = "Lung", cols = 13:15),
  list(tissue = "Lung (COPD)", cols = 17:19),
  list(tissue = "Bladder", cols = 21:23),
  list(tissue = "Dermis", cols = 25:27),
  list(tissue = "Epidermis", cols = 29:31),
  list(tissue = "Thyroid", cols = 33:35),
  list(tissue = "Blood (Sepsis)", cols = 37:39),
  list(tissue = "Blood (HBV)", cols = 41:43),
  list(tissue = "Liver", cols = 45:47)
)

df_8e_list <- list()
for (b in blocks_8e) {
  cols <- b$cols
  tiss <- b$tissue
  sub_df <- as.data.frame(raw_8e[4:nrow(raw_8e), cols])
  colnames(sub_df) <- c("group", "GZMB", "GZMK")
  sub_df <- sub_df[!is.na(sub_df$group) & sub_df$group != "" & sub_df$group != "NA", ]
  
  sub_df$GZMB <- as.numeric(as.character(sub_df$GZMB)) * 100
  sub_df$GZMK <- as.numeric(as.character(sub_df$GZMK)) * 100
  
  long_df <- sub_df %>%
    pivot_longer(cols = c("GZMB", "GZMK"), names_to = "subset", values_to = "pct") %>%
    mutate(
      tissue = tiss,
      subset = ifelse(subset == "GZMB", "GZMB+CD8", "GZMK+CD8")
    )
  df_8e_list[[tiss]] <- long_df
}

df_8e <- bind_rows(df_8e_list)

# Harmonize tissue and disease names
df_8e$tissue_clean <- case_when(
  df_8e$tissue %in% c("Lung", "Lung (COPD)") ~ "Lung",
  df_8e$tissue %in% c("Blood (Sepsis)", "Blood (HBV)") ~ "Blood",
  TRUE ~ df_8e$tissue
)

# Order tissues
tissue_order <- c("Skin", "Esophagus", "Duodenum", "Lung", "Bladder", "Dermis", "Epidermis", "Thyroid", "Blood", "Liver")
df_8e$tissue_clean <- factor(df_8e$tissue_clean, levels = tissue_order)
df_8e$subset <- factor(df_8e$subset, levels = c("GZMB+CD8", "GZMK+CD8"))

# Specific sub-plot for Skin (highlighting Atopic Dermatitis, AD)
p_skin <- df_8e %>%
  filter(tissue_clean == "Skin") %>%
  ggplot(aes(x = group, y = pct, fill = subset)) +
  geom_boxplot(position = position_dodge(width = 0.75), width = 0.6, outlier.size = 0.8, linewidth = 0.35, alpha = 0.9) +
  scale_fill_manual(values = c("GZMB+CD8" = "#E02B20", "GZMK+CD8" = "#F58200"), name = NULL) +
  labs(
    tag = "e",
    title = "Skin (Atopic Dermatitis & Prurigo Nodularis)",
    x = NULL,
    y = expression("% of CD8"^{"+"}~"T cells")
  ) +
  theme_natcom()

# Multi-panel facet across all 10 chronic inflammatory tissue contexts
p_8e_all <- ggplot(df_8e, aes(x = group, y = pct, fill = subset)) +
  geom_boxplot(position = position_dodge(width = 0.75), width = 0.65, outlier.size = 0.6, linewidth = 0.3, alpha = 0.9) +
  facet_wrap(~ tissue_clean, scales = "free_x", nrow = 2) +
  scale_fill_manual(values = c("GZMB+CD8" = "#E02B20", "GZMK+CD8" = "#F58200"), name = NULL) +
  labs(
    tag = "e",
    title = expression("Cross-tissue expansion of GZMK"^{"+"}~"CD8"^{"+"}~"T cells across 10 chronic inflammatory conditions"),
    subtitle = "Source Data: GSE197023 (AD/Skin), GSE175930 (EoE), GSE179633 (LE), etc. (n = 159 samples)",
    x = "Clinical condition / Treatment status",
    y = expression("% of CD8"^{"+"}~"T cells")
  ) +
  theme_natcom(base_size = 7.5) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 6.5),
    strip.text = element_text(size = 8, face = "bold"),
    legend.position = "top"
  )

# Save Figures
save_panel <- function(p, filename, width, height) {
  p_path <- file.path(out_dir, filename)
  b_path <- file.path(brain_dir, filename)
  ggsave(p_path, p, width = width, height = height, dpi = 300)
  ggsave(b_path, p, width = width, height = height, dpi = 300)
}

save_panel(p_skin, "NatCommun_Figure8e_Skin_AD.png", 4.2, 3.6)
save_panel(p_8e_all, "NatCommun_Figure8e_Cross_Tissue_Inflammation.png", 11.5, 6.8)

message("ALL FIGURE 8 PANELS GENERATED SUCCESSFULLY!")
