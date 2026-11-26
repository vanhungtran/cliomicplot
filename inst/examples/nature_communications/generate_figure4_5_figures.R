# ==============================================================================
# cliomicplot: Nature Communications (PMC11607458) Replication - Figures 4 & 5
# Panels 4b, 5f, 5g, 5h, 5i, 5j and Complete Visium HD Composite
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
# 1. Figure 4b: CD8T_GZMK Frequency Across Chronic Rhinosinusitis Spectrum
# ------------------------------------------------------------------------------
cat("Processing Figure 4b (Disease Spectrum)...\n")
raw_4b <- as.data.frame(read_excel(excel_path, sheet = "Figure 4b", col_names = FALSE))

ctrl_vals  <- na.omit(as.numeric(as.character(unlist(raw_4b[3, 3:7])))) * 100
crss_vals  <- na.omit(as.numeric(as.character(unlist(raw_4b[3, 9:13])))) * 100
nenp_vals  <- na.omit(as.numeric(as.character(unlist(raw_4b[3, 15:19])))) * 100
enp_vals   <- na.omit(as.numeric(as.character(unlist(raw_4b[3, 21:26])))) * 100

df_4b <- data.frame(
  Group = factor(c(rep("Control", length(ctrl_vals)),
                   rep("CRSsNP", length(crss_vals)),
                   rep("NE-NP", length(nenp_vals)),
                   rep("E-NP", length(enp_vals))),
                 levels = c("Control", "CRSsNP", "NE-NP", "E-NP")),
  Proportion = c(ctrl_vals, crss_vals, nenp_vals, enp_vals)
)

pal_4b <- c("Control" = "#2CA02C", "CRSsNP" = "#3B75AF", "NE-NP" = "#F58518", "E-NP" = "#DE3533")

p_fig4b <- ggplot(df_4b, aes(x = Group, y = Proportion, fill = Group)) +
  geom_boxplot(width = 0.52, outlier.shape = NA, alpha = 0.8, color = "black", linewidth = 0.4) +
  geom_jitter(width = 0.15, size = 1.8, shape = 21, color = "black", alpha = 0.9) +
  scale_fill_manual(values = pal_4b) +
  geom_signif(
    comparisons = list(c("Control", "NE-NP"), c("Control", "E-NP")),
    annotations = c("P = 0.0374", "P = 0.0091"),
    y_position  = c(68, 76),
    textsize    = 2.8,
    tip_length  = 0,
    color       = "black"
  ) +
  scale_y_continuous(limits = c(0, 82), breaks = seq(0, 80, 20)) +
  labs(
    tag  = "b",
    x    = NULL,
    y    = expression("% of"~"CD8"^"+"~"T cells")
  ) +
  theme_natcommun(base_size = 9) +
  theme(legend.position = "none")

ggsave(file.path(out_dir, "NatCommun_Figure4b_CD8_GZMK_Spectrum.png"),
       p_fig4b, width = 6.0, height = 4.5, dpi = 300)
ggsave(file.path(inst_dir, "NatCommun_Figure4b_CD8_GZMK_Spectrum.png"),
       p_fig4b, width = 6.0, height = 4.5, dpi = 300)
cat("Figure 4b generated.\n")

# ------------------------------------------------------------------------------
# 2. Figure 5f: Visium HD Cell Type Composition Across Samples
# ------------------------------------------------------------------------------
cat("Processing Figure 5f (Visium HD Composition)...\n")
raw_5f <- as.data.frame(read_excel(excel_path, sheet = "Figure 5f"))
samples_5f <- as.character(raw_5f[1, 3:12])
celltypes_5f <- as.character(raw_5f[2:nrow(raw_5f), 2])

mat_5f <- as.matrix(raw_5f[2:nrow(raw_5f), 3:12])
class(mat_5f) <- "numeric"
rownames(mat_5f) <- celltypes_5f
colnames(mat_5f) <- samples_5f

# Proportions
prop_5f <- sweep(mat_5f, 2, colSums(mat_5f), "/") * 100

df_5f_long <- do.call(rbind, lapply(samples_5f, function(s) {
  data.frame(
    Sample    = s,
    Group     = ifelse(grepl("^CIT", s), "Control (CIT)", "Nasal Polyp (NP)"),
    CellType  = factor(celltypes_5f, levels = rev(celltypes_5f)),
    Share     = prop_5f[, s]
  )
}))

p_fig5f <- ggplot(df_5f_long, aes(x = Sample, y = Share, fill = CellType)) +
  geom_bar(stat = "identity", position = "stack", width = 0.8, color = "black", linewidth = 0.15) +
  facet_grid(~ Group, scales = "free_x", space = "free_x") +
  scale_y_continuous(limits = c(0, 100.1), expand = c(0, 0), breaks = seq(0, 100, 25)) +
  labs(
    tag  = "f",
    x    = NULL,
    y    = "Cell proportion (%)"
  ) +
  theme_natcommun(base_size = 8.5) +
  theme(
    axis.text.x      = element_text(angle = 45, hjust = 1, vjust = 1, size = 7.5),
    legend.position  = "right",
    legend.key.size  = unit(0.28, "cm"),
    legend.text      = element_text(size = 7),
    strip.text       = element_text(size = 9, face = "bold")
  )

ggsave(file.path(out_dir, "NatCommun_Figure5f_VisiumHD_Composition.png"),
       p_fig5f, width = 8.8, height = 4.8, dpi = 300)
ggsave(file.path(inst_dir, "NatCommun_Figure5f_VisiumHD_Composition.png"),
       p_fig5f, width = 8.8, height = 4.8, dpi = 300)
cat("Figure 5f generated.\n")

# ------------------------------------------------------------------------------
# 3. Figure 5g: Total Cell Counts in CIT vs NP
# ------------------------------------------------------------------------------
cat("Processing Figure 5g (Total Cell Counts)...\n")
raw_5g <- as.data.frame(read_excel(excel_path, sheet = "Figure 5g", col_names = FALSE))
cit_types  <- as.character(raw_5g[3, 3:21])
cit_counts <- as.numeric(raw_5g[4, 3:21])
np_types   <- as.character(raw_5g[6, 3:21])
np_counts  <- as.numeric(raw_5g[7, 3:21])

df_5g_cit <- data.frame(Group = "CIT (n=4)", CellType = cit_types, Count = cit_counts)
df_5g_np  <- data.frame(Group = "NP (n=6)",  CellType = np_types,  Count = np_counts)
df_5g <- rbind(df_5g_cit, df_5g_np)

# Order by NP counts
np_order <- df_5g_np$CellType[order(df_5g_np$Count)]
df_5g$CellType <- factor(df_5g$CellType, levels = np_order)

p_fig5g <- ggplot(df_5g, aes(x = Count, y = CellType, fill = Group)) +
  geom_bar(stat = "identity", position = "dodge", width = 0.72, color = "black", linewidth = 0.2) +
  scale_fill_manual(values = c("CIT (n=4)" = "#2CA02C", "NP (n=6)" = "#9467BD")) +
  scale_x_continuous(labels = scales::comma) +
  labs(
    tag  = "g",
    x    = "Total cell counts",
    y    = NULL
  ) +
  theme_natcommun(base_size = 8.5) +
  theme(
    axis.text.y      = element_text(size = 7.5),
    legend.position  = "right",
    legend.key.size  = unit(0.35, "cm")
  )

ggsave(file.path(out_dir, "NatCommun_Figure5g_Cell_Counts.png"),
       p_fig5g, width = 7.5, height = 5.2, dpi = 300)
ggsave(file.path(inst_dir, "NatCommun_Figure5g_Cell_Counts.png"),
       p_fig5g, width = 7.5, height = 5.2, dpi = 300)
cat("Figure 5g generated.\n")

# ------------------------------------------------------------------------------
# 4. Figure 5i: Major Immune Cell Proportions (B cells & CD4T)
# ------------------------------------------------------------------------------
cat("Processing Figure 5i...\n")
raw_5i <- as.data.frame(read_excel(excel_path, sheet = "Figure 5i", col_names = FALSE))
b_cit   <- as.numeric(raw_5i[3, 3:6])
b_np    <- as.numeric(raw_5i[3, 8:13])
cd4_cit <- as.numeric(raw_5i[4, 3:6])
cd4_np  <- as.numeric(raw_5i[4, 8:13])

df_5i <- data.frame(
  Tissue   = factor(c(rep("CIT", 4), rep("NP", 6), rep("CIT", 4), rep("NP", 6)), levels = c("CIT", "NP")),
  CellType = factor(c(rep("B cells", 10), rep("CD4T", 10)), levels = c("B cells", "CD4T")),
  Proportion = c(b_cit, b_np, cd4_cit, cd4_np)
)

p_fig5i <- ggplot(df_5i, aes(x = Tissue, y = Proportion, fill = Tissue)) +
  geom_boxplot(width = 0.52, outlier.shape = NA, alpha = 0.75, color = "black", linewidth = 0.4) +
  geom_jitter(width = 0.15, size = 1.8, shape = 21, color = "black", alpha = 0.9) +
  facet_wrap(~ CellType, scales = "free_y") +
  scale_fill_manual(values = c("CIT" = "#2CA02C", "NP" = "#9467BD")) +
  geom_signif(
    comparisons = list(c("CIT", "NP")),
    test        = "wilcox.test",
    textsize    = 2.8,
    tip_length  = 0,
    color       = "black"
  ) +
  labs(
    tag  = "i",
    x    = NULL,
    y    = "Proportion in total cells (%)"
  ) +
  theme_natcommun(base_size = 9) +
  theme(legend.position = "none", strip.text = element_text(size = 9.5, face = "bold"))

ggsave(file.path(out_dir, "NatCommun_Figure5i_Immune_Proportions.png"),
       p_fig5i, width = 6.2, height = 4.2, dpi = 300)
ggsave(file.path(inst_dir, "NatCommun_Figure5i_Immune_Proportions.png"),
       p_fig5i, width = 6.2, height = 4.2, dpi = 300)
cat("Figure 5i generated.\n")

# ------------------------------------------------------------------------------
# 5. Figure 5j: Major Structural Cell Proportions (Fibroblasts, Epithelial, SMCs)
# ------------------------------------------------------------------------------
cat("Processing Figure 5j...\n")
raw_5j <- as.data.frame(read_excel(excel_path, sheet = "Figure 5j", col_names = FALSE))
fib_cit <- as.numeric(raw_5j[3, 3:6])
fib_np  <- as.numeric(raw_5j[3, 8:13])
epi_cit <- as.numeric(raw_5j[4, 3:6])
epi_np  <- as.numeric(raw_5j[4, 8:13])
smc_cit <- as.numeric(raw_5j[5, 3:6])
smc_np  <- as.numeric(raw_5j[5, 8:13])

df_5j <- data.frame(
  Tissue   = factor(rep(c(rep("CIT", 4), rep("NP", 6)), 3), levels = c("CIT", "NP")),
  CellType = factor(c(rep("Fibroblasts", 10), rep("Epithelial cells", 10), rep("SMCs", 10)),
                    levels = c("Fibroblasts", "Epithelial cells", "SMCs")),
  Proportion = c(fib_cit, fib_np, epi_cit, epi_np, smc_cit, smc_np)
)

p_fig5j <- ggplot(df_5j, aes(x = Tissue, y = Proportion, fill = Tissue)) +
  geom_boxplot(width = 0.52, outlier.shape = NA, alpha = 0.75, color = "black", linewidth = 0.4) +
  geom_jitter(width = 0.15, size = 1.8, shape = 21, color = "black", alpha = 0.9) +
  facet_wrap(~ CellType, scales = "free_y") +
  scale_fill_manual(values = c("CIT" = "#2CA02C", "NP" = "#9467BD")) +
  geom_signif(
    comparisons = list(c("CIT", "NP")),
    test        = "wilcox.test",
    textsize    = 2.8,
    tip_length  = 0,
    color       = "black"
  ) +
  labs(
    tag  = "j",
    x    = NULL,
    y    = "Proportion in total cells (%)"
  ) +
  theme_natcommun(base_size = 9) +
  theme(legend.position = "none", strip.text = element_text(size = 9.5, face = "bold"))

ggsave(file.path(out_dir, "NatCommun_Figure5j_Structural_Proportions.png"),
       p_fig5j, width = 8.5, height = 4.2, dpi = 300)
ggsave(file.path(inst_dir, "NatCommun_Figure5j_Structural_Proportions.png"),
       p_fig5j, width = 8.5, height = 4.2, dpi = 300)
cat("Figure 5j generated.\n")

# ------------------------------------------------------------------------------
# 6. Complete Figure 5 Multi-Panel Composite (f, g, h, i, j)
# ------------------------------------------------------------------------------
# Load 5h
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
p_fig5h <- ggplot(df_5h, aes(x = Tissue, y = Frequency, fill = CellType)) +
  geom_boxplot(width = 0.52, outlier.shape = NA, alpha = 0.85, color = "black", linewidth = 0.4) +
  geom_jitter(width = 0.12, size = 1.6, color = "black", alpha = 0.9) +
  facet_wrap(~ CellType, scales = "free_y") +
  scale_fill_manual(values = c("CD8T_GZMK" = "#F58518", "CD8T_GZMB" = "#D3B89D")) +
  geom_signif(
    comparisons = list(c("CIT", "NP")),
    annotations = c("P = 0.0667"),
    data        = subset(df_5h, CellType == "CD8T_GZMK"),
    y_position  = max(df_5h$Frequency[df_5h$CellType == "CD8T_GZMK"]) * 1.08,
    textsize    = 2.8, tip_length = 0, color = "black"
  ) +
  geom_signif(
    comparisons = list(c("CIT", "NP")),
    annotations = c("ns"),
    data        = subset(df_5h, CellType == "CD8T_GZMB"),
    y_position  = max(df_5h$Frequency[df_5h$CellType == "CD8T_GZMB"]) * 1.08,
    textsize    = 2.8, tip_length = 0, color = "black"
  ) +
  labs(tag = "h", x = NULL, y = expression("% in"~"CD8"^"+"~"T cells")) +
  theme_natcommun(base_size = 8.5) +
  theme(legend.position = "none", strip.text = element_text(size = 9, face = "bold"))

p_fig5_comp <- (p_fig5f | p_fig5g) / (p_fig5h | p_fig5i | p_fig5j) +
  plot_layout(heights = c(1.2, 1.0), widths = c(1.2, 1.0)) +
  plot_annotation(
    title = "Nature Communications (2024) 15:10413 - Figure 5 Visium HD Spatial Architecture",
    subtitle = "Cellular composition, cell abundance counts, and proportional shifts across nasal microenvironments",
    theme = theme(
      plot.title    = element_text(size = 12, face = "bold", family = "sans"),
      plot.subtitle = element_text(size = 9, color = "grey30", family = "sans")
    )
  )

ggsave(file.path(out_dir, "NatCommun_Figure5_Complete_Composite.png"),
       p_fig5_comp, width = 15.5, height = 11.5, dpi = 300)
ggsave(file.path(inst_dir, "NatCommun_Figure5_Complete_Composite.png"),
       p_fig5_comp, width = 15.5, height = 11.5, dpi = 300)
cat("NatCommun_Figure5_Complete_Composite.png generated.\n")

cat("\nALL FIGURE 4 & 5 FIGURES GENERATED SUCCESSFULLY!\n")
