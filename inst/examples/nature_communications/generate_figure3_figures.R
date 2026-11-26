# ==============================================================================
# cliomicplot: Nature Communications (PMC11607458) Replication - Figure 3
# Panels 3g, 3j, 3l, 3n and Composite
# Study: Granzyme K+ CD8+ T cells promote neutrophilic inflammation in nasal polyps
# ==============================================================================

library(cliomicplot)
library(ggplot2)
library(readxl)
library(patchwork)
library(ggsignif)

excel_path <- "C:/Users/lucas/Downloads/41467_2024_54685_MOESM4_ESM.xlsx"
out_dir <- "C:/Users/lucas/.gemini/antigravity/brain/a8fc441c-e35f-47ad-9d3c-bef516138cce"
inst_dir <- "c:/Users/lucas/OneDrive/Statistics/cliomicplot/inst/examples/nature_communications"

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
# 1. Figure 3g: Flow Cytometry Verification in Extended Cohort (95 samples)
# ------------------------------------------------------------------------------
cat("Processing Figure 3g...\n")
raw_3g <- as.data.frame(read_excel(excel_path, sheet = "Figure 3g", col_names = FALSE))

cbl_gzmk  <- na.omit(as.numeric(as.character(unlist(raw_3g[3, 2:24]))))
npbl_gzmk <- na.omit(as.numeric(as.character(unlist(raw_3g[3, 25:52]))))
cit_gzmk  <- na.omit(as.numeric(as.character(unlist(raw_3g[3, 53:67]))))
np_gzmk   <- na.omit(as.numeric(as.character(unlist(raw_3g[3, 68:97]))))

cbl_gzmb  <- na.omit(as.numeric(as.character(unlist(raw_3g[4, 2:24]))))
npbl_gzmb <- na.omit(as.numeric(as.character(unlist(raw_3g[4, 25:52]))))
cit_gzmb  <- na.omit(as.numeric(as.character(unlist(raw_3g[4, 53:67]))))
np_gzmb   <- na.omit(as.numeric(as.character(unlist(raw_3g[4, 68:97]))))

df_3g_gzmk <- data.frame(
  Group = factor(c(rep("CBL", length(cbl_gzmk)),
                   rep("NP-BL", length(npbl_gzmk)),
                   rep("CIT", length(cit_gzmk)),
                   rep("NP", length(np_gzmk))),
                 levels = c("CBL", "NP-BL", "CIT", "NP")),
  Frequency = c(cbl_gzmk, npbl_gzmk, cit_gzmk, np_gzmk),
  Marker = "GZMK+GZMB- CD8+ T cells"
)

df_3g_gzmb <- data.frame(
  Group = factor(c(rep("CBL", length(cbl_gzmb)),
                   rep("NP-BL", length(npbl_gzmb)),
                   rep("CIT", length(cit_gzmb)),
                   rep("NP", length(np_gzmb))),
                 levels = c("CBL", "NP-BL", "CIT", "NP")),
  Frequency = c(cbl_gzmb, npbl_gzmb, cit_gzmb, np_gzmb),
  Marker = "GZMK-GZMB+ CD8+ T cells"
)

df_3g <- rbind(df_3g_gzmk, df_3g_gzmb)

pal_cohorts <- c("CBL" = "#3B75AF", "NP-BL" = "#DE3533", "CIT" = "#2CA02C", "NP" = "#9467BD")

p_fig3g <- ggplot(df_3g, aes(x = Group, y = Frequency, color = Group)) +
  geom_boxplot(width = 0.52, fill = "white", outlier.shape = NA, linewidth = 0.5) +
  geom_jitter(width = 0.18, size = 1.3, alpha = 0.75) +
  facet_wrap(~ Marker, scales = "free_y") +
  scale_color_manual(values = pal_cohorts) +
  geom_signif(
    comparisons = list(c("CIT", "NP")),
    annotations = c("P < 0.0001"),
    data        = subset(df_3g, Marker == "GZMK+GZMB- CD8+ T cells"),
    y_position  = max(df_3g$Frequency[df_3g$Marker == "GZMK+GZMB- CD8+ T cells"]) * 1.05,
    textsize    = 2.8,
    tip_length  = 0,
    color       = "black"
  ) +
  labs(
    tag  = "g",
    x    = NULL,
    y    = expression("% in"~"CD8"^"+"~"T cells")
  ) +
  theme_natcommun(base_size = 9) +
  theme(legend.position = "none")

ggsave(file.path(out_dir, "NatCommun_Figure3g_Flow_Validation.png"),
       p_fig3g, width = 7.5, height = 4.5, dpi = 300)
ggsave(file.path(inst_dir, "NatCommun_Figure3g_Flow_Validation.png"),
       p_fig3g, width = 7.5, height = 4.5, dpi = 300)
cat("Figure 3g generated.\n")

# ------------------------------------------------------------------------------
# 2. Figure 3j: Receptor Expression MFI (CXCR4, PD-1, EOMES, T-bet)
# ------------------------------------------------------------------------------
cat("Processing Figure 3j...\n")
raw_3j <- as.data.frame(read_excel(excel_path, sheet = "Figure 3j", col_names = FALSE))

receptors <- c("CXCR4", "PD-1", "EOMES", "T-bet")
rows_rec <- c(3, 4, 5, 6)

df_3j_list <- lapply(seq_along(receptors), function(i) {
  r <- rows_rec[i]
  rec_name <- receptors[i]
  gzmk_vals <- na.omit(as.numeric(as.character(unlist(raw_3j[r, 3:19]))))
  gzmb_vals <- na.omit(as.numeric(as.character(unlist(raw_3j[r, 21:37]))))
  n <- min(length(gzmk_vals), length(gzmb_vals))
  data.frame(
    Donor    = rep(paste0("D", 1:n), 2),
    Receptor = rec_name,
    Subset   = factor(c(rep("GZMK+CD8", n), rep("GZMB+CD8", n)), levels = c("GZMK+CD8", "GZMB+CD8")),
    MFI      = c(gzmk_vals[1:n], gzmb_vals[1:n])
  )
})
df_3j <- do.call(rbind, df_3j_list)

pal_subsets <- c("GZMK+CD8" = "#F58518", "GZMB+CD8" = "#3B75AF")

p_fig3j <- ggplot(df_3j, aes(x = Subset, y = MFI, fill = Subset)) +
  geom_boxplot(width = 0.5, outlier.shape = NA, alpha = 0.8, color = "black", linewidth = 0.4) +
  geom_line(aes(group = Donor), color = "grey60", linewidth = 0.3) +
  geom_point(size = 1.6, shape = 21, color = "black") +
  facet_wrap(~ Receptor, scales = "free_y", nrow = 1) +
  scale_fill_manual(values = pal_subsets) +
  labs(
    tag  = "j",
    x    = NULL,
    y    = "Mean Fluorescence Intensity (MFI)"
  ) +
  theme_natcommun(base_size = 8.5) +
  theme(
    legend.position = "none",
    axis.text.x     = element_text(angle = 30, hjust = 1, size = 7.5)
  )

ggsave(file.path(out_dir, "NatCommun_Figure3j_Receptor_MFI.png"),
       p_fig3j, width = 8.8, height = 4.2, dpi = 300)
ggsave(file.path(inst_dir, "NatCommun_Figure3j_Receptor_MFI.png"),
       p_fig3j, width = 8.8, height = 4.2, dpi = 300)
cat("Figure 3j generated.\n")

# ------------------------------------------------------------------------------
# 3. Figure 3l: Cytokine Production (CIT vs NP)
# ------------------------------------------------------------------------------
cat("Processing Figure 3l...\n")
raw_3l <- as.data.frame(read_excel(excel_path, sheet = "Figure 3l", col_names = FALSE))

cit_3l <- na.omit(as.numeric(as.character(unlist(raw_3l[3, 3:20]))))
np_3l  <- na.omit(as.numeric(as.character(unlist(raw_3l[3, 21:77]))))

df_3l <- data.frame(
  Tissue    = factor(c(rep("CIT", length(cit_3l)), rep("NP", length(np_3l))), levels = c("CIT", "NP")),
  Frequency = c(cit_3l, np_3l)
)

p_fig3l <- ggplot(df_3l, aes(x = Tissue, y = Frequency, fill = Tissue)) +
  geom_boxplot(width = 0.52, outlier.shape = NA, alpha = 0.75, color = "black", linewidth = 0.4) +
  geom_jitter(width = 0.15, size = 1.5, alpha = 0.85) +
  scale_fill_manual(values = c("CIT" = "#2CA02C", "NP" = "#9467BD")) +
  geom_signif(
    comparisons = list(c("CIT", "NP")),
    test        = "wilcox.test",
    map_signif_level = FALSE,
    textsize    = 2.8,
    tip_length  = 0,
    color       = "black"
  ) +
  labs(
    tag  = "l",
    x    = NULL,
    y    = expression("GZMK"^{"+"}*"CD8"^{"+"}~"cells (%)")
  ) +
  theme_natcommun(base_size = 9) +
  theme(legend.position = "none")

ggsave(file.path(out_dir, "NatCommun_Figure3l_Cytokines.png"),
       p_fig3l, width = 4.5, height = 4.2, dpi = 300)
ggsave(file.path(inst_dir, "NatCommun_Figure3l_Cytokines.png"),
       p_fig3l, width = 4.5, height = 4.2, dpi = 300)
cat("Figure 3l generated.\n")

# ------------------------------------------------------------------------------
# 4. Figure 3n: Target Cell Cytotoxicity Lysis (%)
# ------------------------------------------------------------------------------
cat("Processing Figure 3n...\n")
raw_3n <- as.data.frame(read_excel(excel_path, sheet = "Figure 3n", col_names = FALSE))

cit_3n <- na.omit(as.numeric(as.character(unlist(raw_3n[3, 3:13]))))
np_3n  <- na.omit(as.numeric(as.character(unlist(raw_3n[3, 14:23]))))

df_3n <- data.frame(
  Tissue = factor(c(rep("CIT", length(cit_3n)), rep("NP", length(np_3n))), levels = c("CIT", "NP")),
  Lysis  = c(cit_3n, np_3n)
)

p_fig3n <- ggplot(df_3n, aes(x = Tissue, y = Lysis, fill = Tissue)) +
  geom_boxplot(width = 0.52, outlier.shape = NA, alpha = 0.75, color = "black", linewidth = 0.4) +
  geom_jitter(width = 0.15, size = 1.5, alpha = 0.85) +
  scale_fill_manual(values = c("CIT" = "#2CA02C", "NP" = "#9467BD")) +
  geom_signif(
    comparisons = list(c("CIT", "NP")),
    test        = "wilcox.test",
    textsize    = 2.8,
    tip_length  = 0,
    color       = "black"
  ) +
  labs(
    tag  = "n",
    x    = NULL,
    y    = "Target cell lysis (%)"
  ) +
  theme_natcommun(base_size = 9) +
  theme(legend.position = "none")

ggsave(file.path(out_dir, "NatCommun_Figure3n_Cytotoxicity.png"),
       p_fig3n, width = 4.5, height = 4.2, dpi = 300)
ggsave(file.path(inst_dir, "NatCommun_Figure3n_Cytotoxicity.png"),
       p_fig3n, width = 4.5, height = 4.2, dpi = 300)
cat("Figure 3n generated.\n")

# ------------------------------------------------------------------------------
# 5. Figure 3 Multi-Panel Composite
# ------------------------------------------------------------------------------
p_fig3_comp <- (p_fig3g) / (p_fig3j) / (p_fig3l | p_fig3n) +
  plot_layout(heights = c(1.1, 1.0, 1.0)) +
  plot_annotation(
    title = "Nature Communications (2024) 15:10413 - Figure 3 Replication",
    subtitle = "Phenotypic characterization, surface receptor profile, and effector function of GZMK+CD8+ T cells",
    theme = theme(
      plot.title    = element_text(size = 12, face = "bold", family = "sans"),
      plot.subtitle = element_text(size = 9, color = "grey30", family = "sans")
    )
  )

ggsave(file.path(out_dir, "NatCommun_Figure3_Composite.png"),
       p_fig3_comp, width = 11.5, height = 11.0, dpi = 300)
ggsave(file.path(inst_dir, "NatCommun_Figure3_Composite.png"),
       p_fig3_comp, width = 11.5, height = 11.0, dpi = 300)
cat("NatCommun_Figure3_Composite.png generated.\n")

cat("\nALL FIGURE 3 FIGURES GENERATED SUCCESSFULLY!\n")
