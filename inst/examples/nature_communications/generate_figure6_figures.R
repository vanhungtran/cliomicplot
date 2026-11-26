# ==============================================================================
# Nature Communications (2024) 15:10413
# Granzyme K+ CD8+ T cells interact with fibroblasts to promote neutrophilic
# inflammation in nasal polyps
#
# Figure 6: Interaction between GZMK+CD8+ T cells and fibroblasts contributes
#           to neutrophilic inflammation in nasal polyps
# Panels: 6c, 6d, 6g, 6h, 6i, 6j, 6m, 6n, 6o and Complete Composite
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
      axis.text = element_text(size = rel(0.9), color = "black"),
      plot.title = element_text(size = rel(1.05), face = "bold", hjust = 0),
      plot.subtitle = element_text(size = rel(0.85), color = "grey30", hjust = 0),
      plot.tag = element_text(size = rel(1.4), face = "bold", hjust = 0, vjust = 1),
      legend.title = element_text(size = rel(0.9), face = "bold"),
      legend.text = element_text(size = rel(0.85)),
      legend.key.size = unit(0.35, "cm"),
      strip.background = element_blank(),
      strip.text = element_text(size = rel(0.95), face = "bold")
    )
}

# ------------------------------------------------------------------------------
# 1. Figure 6c: Average fibroblasts within 25 um
# ------------------------------------------------------------------------------
message("Processing Figure 6c (Spatial Density within 25 um)...")
raw_6c <- read_excel(excel_file, sheet = "Figure 6c", col_names = FALSE)

extract_spatial_df <- function(raw_sheet) {
  cell_types <- c("GZMK+CD8", "GZMB+CD8", "CD4+T", "CD19+B")
  start_cols <- c(4, 15, 26, 37)
  
  df_list <- list()
  for (i in seq_along(cell_types)) {
    ct <- cell_types[i]
    c_idx <- start_cols[i]
    
    cit_vals <- as.numeric(as.character(unlist(raw_sheet[3, c_idx:(c_idx + 9)])))
    np_vals  <- as.numeric(as.character(unlist(raw_sheet[4, c_idx:(c_idx + 9)])))
    
    df_list[[paste0(ct, "_CIT")]] <- data.frame(
      cell_type = ct,
      group = "CIT group (n = 10)",
      value = cit_vals[!is.na(cit_vals)]
    )
    df_list[[paste0(ct, "_NP")]] <- data.frame(
      cell_type = ct,
      group = "NP group (n = 10)",
      value = np_vals[!is.na(np_vals)]
    )
  }
  bind_rows(df_list)
}

df_6c <- extract_spatial_df(raw_6c)
df_6c$cell_type <- factor(df_6c$cell_type, levels = c("GZMK+CD8", "GZMB+CD8", "CD4+T", "CD19+B"))
df_6c$group <- factor(df_6c$group, levels = c("CIT group (n = 10)", "NP group (n = 10)"))

# Summary for mean +/- SD bars
sum_6c <- df_6c %>%
  group_by(group, cell_type) %>%
  summarize(mean = mean(value), sd = sd(value), .groups = "drop")

p_6c <- ggplot(sum_6c, aes(x = cell_type, y = mean, fill = cell_type)) +
  geom_col(color = "black", linewidth = 0.35, width = 0.65, alpha = 0.9) +
  geom_errorbar(aes(ymin = mean, ymax = mean + sd), width = 0.25, linewidth = 0.4) +
  geom_jitter(data = df_6c, aes(x = cell_type, y = value), width = 0.15, size = 1.1, shape = 21, fill = "white", stroke = 0.4, inherit.aes = FALSE) +
  facet_wrap(~ group, scales = "free_x") +
  scale_fill_manual(values = c("GZMK+CD8" = "#F2994A", "GZMB+CD8" = "#D9C8B8", "CD4+T" = "#66B266", "CD19+B" = "#A8C64B"), guide = "none") +
  labs(
    tag = "c",
    x = NULL,
    y = expression("Fibroblasts within 25 "*mu*"m of target")
  ) +
  theme_natcom(base_size = 7.5) +
  theme(axis.text.x = element_text(angle = 35, hjust = 1))

# ------------------------------------------------------------------------------
# 2. Figure 6d: Distance to closest COL1A1+ fibroblast (um)
# ------------------------------------------------------------------------------
message("Processing Figure 6d (Physical Distance to Fibroblasts)...")
raw_6d <- read_excel(excel_file, sheet = "Figure 6d", col_names = FALSE)
df_6d <- extract_spatial_df(raw_6d)
df_6d$cell_type <- factor(df_6d$cell_type, levels = c("GZMK+CD8", "GZMB+CD8", "CD4+T", "CD19+B"))
df_6d$group <- factor(df_6d$group, levels = c("CIT group (n = 10)", "NP group (n = 10)"))

sum_6d <- df_6d %>%
  group_by(group, cell_type) %>%
  summarize(mean = mean(value), sd = sd(value), .groups = "drop")

p_6d <- ggplot(sum_6d, aes(x = cell_type, y = mean, fill = cell_type)) +
  geom_col(color = "black", linewidth = 0.35, width = 0.65, alpha = 0.9) +
  geom_errorbar(aes(ymin = mean, ymax = mean + sd), width = 0.25, linewidth = 0.4) +
  geom_jitter(data = df_6d, aes(x = cell_type, y = value), width = 0.15, size = 1.1, shape = 21, fill = "white", stroke = 0.4, inherit.aes = FALSE) +
  facet_wrap(~ group, scales = "free_y") +
  scale_fill_manual(values = c("GZMK+CD8" = "#F2994A", "GZMB+CD8" = "#D9C8B8", "CD4+T" = "#66B266", "CD19+B" = "#A8C64B"), guide = "none") +
  labs(
    tag = "d",
    x = NULL,
    y = expression("Distance to closest fibroblasts ("*mu*"m)")
  ) +
  theme_natcom(base_size = 7.5) +
  theme(axis.text.x = element_text(angle = 35, hjust = 1))

# ------------------------------------------------------------------------------
# 3. Figure 6g: qPCR gene expression in stimulated NPDF (n=7)
# ------------------------------------------------------------------------------
message("Processing Figure 6g (NPDF qPCR Expression)...")
raw_6g <- read_excel(excel_file, sheet = "Figure 6g", col_names = FALSE)
genes_6g <- na.omit(as.character(raw_6g[3:nrow(raw_6g), 2][[1]]))

parse_stim_assay <- function(raw_sheet, genes, col_blocks) {
  df_list <- list()
  for (r in seq_along(genes)) {
    row_idx <- r + 2
    g <- genes[r]
    for (stim in names(col_blocks)) {
      c_range <- col_blocks[[stim]]
      vals <- as.numeric(as.character(unlist(raw_sheet[row_idx, c_range])))
      vals <- vals[!is.na(vals)]
      df_list[[paste(g, stim, sep = "_")]] <- data.frame(
        gene = g,
        treatment = stim,
        value = vals
      )
    }
  }
  bind_rows(df_list)
}

blocks_6g <- list("Medium" = 3:9, "GZMK" = 11:17, "GZMA" = 19:25, "GZMB" = 27:33)
df_6g <- parse_stim_assay(raw_6g, genes_6g, blocks_6g)
df_6g$treatment <- factor(df_6g$treatment, levels = c("Medium", "GZMK", "GZMA", "GZMB"))
df_6g$gene <- factor(df_6g$gene, levels = genes_6g)

sum_6g <- df_6g %>%
  group_by(gene, treatment) %>%
  summarize(mean = mean(value), sd = sd(value), .groups = "drop")

p_6g <- ggplot(sum_6g, aes(x = treatment, y = mean, fill = treatment)) +
  geom_col(color = "black", linewidth = 0.3, width = 0.65, alpha = 0.9) +
  geom_errorbar(aes(ymin = mean, ymax = mean + sd), width = 0.25, linewidth = 0.35) +
  geom_jitter(data = df_6g, aes(x = treatment, y = value), width = 0.15, size = 0.9, shape = 21, fill = "white", stroke = 0.35, inherit.aes = FALSE) +
  facet_wrap(~ gene, scales = "free_y", ncol = 4) +
  scale_fill_manual(values = c("Medium" = "#3A669C", "GZMK" = "#D96B14", "GZMA" = "#2B805E", "GZMB" = "#C9332B"), guide = "none") +
  labs(
    tag = "g",
    x = NULL,
    y = "Relative mRNA expression level"
  ) +
  theme_natcom(base_size = 7.0) +
  theme(axis.text.x = element_text(angle = 40, hjust = 1))

# ------------------------------------------------------------------------------
# 4. Figure 6h: Secreted chemokines/cytokines by NPDF (n=10)
# ------------------------------------------------------------------------------
message("Processing Figure 6h (NPDF Secreted Chemokines/Cytokines)...")
raw_6h <- read_excel(excel_file, sheet = "Figure 6h", col_names = FALSE)
genes_6h <- na.omit(as.character(raw_6h[3:nrow(raw_6h), 2][[1]]))
blocks_6h <- list("Medium" = 3:12, "GZMK" = 14:23, "GZMA" = 25:34, "GZMB" = 36:45)
df_6h <- parse_stim_assay(raw_6h, genes_6h, blocks_6h)
df_6h$treatment <- factor(df_6h$treatment, levels = c("Medium", "GZMK", "GZMA", "GZMB"))
df_6h$gene <- factor(df_6h$gene, levels = genes_6h)

sum_6h <- df_6h %>%
  group_by(gene, treatment) %>%
  summarize(mean = mean(value), sd = sd(value), .groups = "drop")

p_6h <- ggplot(sum_6h, aes(x = treatment, y = mean, fill = treatment)) +
  geom_col(color = "black", linewidth = 0.3, width = 0.65, alpha = 0.9) +
  geom_errorbar(aes(ymin = mean, ymax = mean + sd), width = 0.25, linewidth = 0.35) +
  geom_jitter(data = df_6h, aes(x = treatment, y = value), width = 0.15, size = 0.9, shape = 21, fill = "white", stroke = 0.35, inherit.aes = FALSE) +
  facet_wrap(~ gene, scales = "free_y", ncol = 4) +
  scale_fill_manual(values = c("Medium" = "#9BB4CE", "GZMK" = "#D99B66", "GZMA" = "#78B58F", "GZMB" = "#E08078"), guide = "none") +
  labs(
    tag = "h",
    x = NULL,
    y = "Protein concentration (pg/mL)"
  ) +
  theme_natcom(base_size = 7.0) +
  theme(axis.text.x = element_text(angle = 40, hjust = 1))

# ------------------------------------------------------------------------------
# 5. Figure 6i: Expression in NPDF under CD4T vs CD8T supernatant (n=8)
# ------------------------------------------------------------------------------
message("Processing Figure 6i (Supernatant Culture qPCR)...")
raw_6i <- read_excel(excel_file, sheet = "Figure 6i", col_names = FALSE)
genes_6i <- na.omit(as.character(raw_6i[3:nrow(raw_6i), 2][[1]]))
blocks_6i <- list("Medium" = 3:10, "CD8+T" = 12:19, "CD4+T" = 21:28)
df_6i <- parse_stim_assay(raw_6i, genes_6i, blocks_6i)
df_6i$treatment <- factor(df_6i$treatment, levels = c("Medium", "CD8+T", "CD4+T"))
df_6i$gene <- factor(df_6i$gene, levels = genes_6i)

sum_6i <- df_6i %>%
  group_by(gene, treatment) %>%
  summarize(mean = mean(value), sd = sd(value), .groups = "drop")

p_6i <- ggplot(sum_6i, aes(x = treatment, y = mean, fill = treatment)) +
  geom_col(color = "black", linewidth = 0.3, width = 0.65, alpha = 0.9) +
  geom_errorbar(aes(ymin = mean, ymax = mean + sd), width = 0.25, linewidth = 0.35) +
  geom_jitter(data = df_6i, aes(x = treatment, y = value), width = 0.15, size = 0.9, shape = 21, fill = "white", stroke = 0.35, inherit.aes = FALSE) +
  facet_wrap(~ gene, scales = "free_y", ncol = 4) +
  scale_fill_manual(values = c("Medium" = "#C8D9F0", "CD8+T" = "#F0C8D9", "CD4+T" = "#8CE68C"), guide = "none") +
  labs(
    tag = "i",
    x = NULL,
    y = "Relative mRNA expression level"
  ) +
  theme_natcom(base_size = 7.0) +
  theme(axis.text.x = element_text(angle = 35, hjust = 1))

# ------------------------------------------------------------------------------
# 6. Figure 6j: Chemotaxis migration index (+/- LIT-927)
# ------------------------------------------------------------------------------
message("Processing Figure 6j (Chemotaxis Migration Assay)...")
raw_6j <- read_excel(excel_file, sheet = "Figure 6j", col_names = FALSE)
blocks_6j <- list("Medium" = 4:10, "Fibroblasts" = 12:18, "Fibroblasts+LIT927" = 20:26, "LIT927" = 28:34)
df_6j_list <- list()
for (cond in names(blocks_6j)) {
  c_range <- blocks_6j[[cond]]
  vals <- as.numeric(as.character(unlist(raw_6j[3, c_range])))
  vals <- vals[!is.na(vals)]
  df_6j_list[[cond]] <- data.frame(condition = cond, value = vals)
}
df_6j <- bind_rows(df_6j_list)
df_6j$condition <- factor(df_6j$condition, levels = c("Medium", "Fibroblasts", "Fibroblasts+LIT927", "LIT927"))

sum_6j <- df_6j %>%
  group_by(condition) %>%
  summarize(mean = mean(value), sd = sd(value), .groups = "drop")

p_6j <- ggplot(sum_6j, aes(x = condition, y = mean, fill = condition)) +
  geom_col(color = "black", linewidth = 0.35, width = 0.65, alpha = 0.9) +
  geom_errorbar(aes(ymin = mean, ymax = mean + sd), width = 0.25, linewidth = 0.4) +
  geom_jitter(data = df_6j, aes(x = condition, y = value), width = 0.15, size = 1.2, shape = 21, fill = "white", stroke = 0.4, inherit.aes = FALSE) +
  scale_fill_manual(values = c("Medium" = "#1E2B85", "Fibroblasts" = "#7B3FA0", "Fibroblasts+LIT927" = "#B46BB8", "LIT927" = "#C94FA6"), guide = "none") +
  annotate("segment", x = 1, xend = 2, y = 7.5, yend = 7.5, linewidth = 0.35) +
  annotate("text", x = 1.5, y = 8.1, label = "P = 0.0012", size = 2.4, fontface = "italic") +
  annotate("segment", x = 2, xend = 3, y = 8.7, yend = 8.7, linewidth = 0.35) +
  annotate("text", x = 2.5, y = 9.3, label = "P = 0.0001", size = 2.4, fontface = "italic") +
  labs(
    tag = "j",
    title = expression("GZMK"^{"+"}~"GZMB"^{"-" }~"CD8"^{"+"}~"T"),
    x = NULL,
    y = "Chemotactic index (medium = 1)"
  ) +
  theme_natcom() +
  theme(axis.text.x = element_text(angle = 30, hjust = 1))

# ------------------------------------------------------------------------------
# 7. Figure 6m: MPO+ Neutrophil quantification in CIT vs NP
# ------------------------------------------------------------------------------
message("Processing Figure 6m (MPO+ Cell Counts)...")
raw_6m <- read_excel(excel_file, sheet = "Figure 6m", col_names = FALSE)
cit_mpo <- as.numeric(as.character(unlist(raw_6m[3, 3:19])))
np_mpo  <- as.numeric(as.character(unlist(raw_6m[3, 21:54])))
df_6m <- bind_rows(
  data.frame(group = "CIT (n = 17)", count = cit_mpo[!is.na(cit_mpo)]),
  data.frame(group = "NP (n = 34)",  count = np_mpo[!is.na(np_mpo)])
)
df_6m$group <- factor(df_6m$group, levels = c("CIT (n = 17)", "NP (n = 34)"))

sum_6m <- df_6m %>%
  group_by(group) %>%
  summarize(mean = mean(count), sd = sd(count), .groups = "drop")

p_6m <- ggplot(df_6m, aes(x = group, y = count, color = group)) +
  geom_jitter(width = 0.18, size = 1.6, shape = 1, stroke = 0.8) +
  geom_errorbar(data = sum_6m, aes(x = group, y = mean, ymin = mean - sd, ymax = mean + sd), width = 0.25, linewidth = 0.6, color = "black", inherit.aes = FALSE) +
  geom_segment(data = sum_6m, aes(x = as.numeric(group) - 0.2, xend = as.numeric(group) + 0.2, y = mean, yend = mean), linewidth = 0.7, color = "black", inherit.aes = FALSE) +
  scale_color_manual(values = c("CIT (n = 17)" = "#1B8A2B", "NP (n = 34)" = "#9B3FA0"), guide = "none") +
  annotate("segment", x = 1, xend = 2, y = 28, yend = 28, linewidth = 0.35) +
  annotate("text", x = 1.5, y = 29.5, label = "P < 0.0001", size = 2.6, fontface = "italic") +
  labs(
    tag = "m",
    title = expression("MPO"^{"+"}~"cell count"),
    x = NULL,
    y = "Count / HPF"
  ) +
  theme_natcom()

# ------------------------------------------------------------------------------
# 8. Figure 6n: Correlation between GZMK+CD8+ T cells and MPO+ Neutrophils
# ------------------------------------------------------------------------------
message("Processing Figure 6n (Correlation with Neutrophils)...")
raw_6n <- read_excel(excel_file, sheet = "Figure 6n", col_names = FALSE)
mpo_vals  <- as.numeric(as.character(unlist(raw_6n[2, 3:36])))
gzmk_vals <- as.numeric(as.character(unlist(raw_6n[3, 3:36])))
df_6n <- data.frame(
  mpo = mpo_vals[!is.na(mpo_vals)],
  gzmk = gzmk_vals[!is.na(gzmk_vals)]
)

p_6n <- ggplot(df_6n, aes(x = gzmk, y = mpo)) +
  geom_point(size = 1.6, color = "#1D35B8", alpha = 0.85) +
  geom_smooth(method = "lm", se = FALSE, color = "#1D35B8", linewidth = 0.65) +
  annotate("text", x = 0.2, y = 24, label = "R = 0.456\nP = 0.007", hjust = 0, size = 2.6, fontface = "italic") +
  labs(
    tag = "n",
    x = expression("GZMK"^{"+"}~"CD8"^{"+"}~"T cell count"),
    y = expression("MPO"^{"+"}~"cell count")
  ) +
  scale_x_continuous(breaks = seq(0, 8, 2), limits = c(0, 8.5)) +
  scale_y_continuous(breaks = seq(0, 30, 10), limits = c(0, 30)) +
  theme_natcom()

# ------------------------------------------------------------------------------
# 9. Figure 6o: Correlation between GZMK+CD8+ T cells and Eosinophils
# ------------------------------------------------------------------------------
message("Processing Figure 6o (Correlation with Eosinophils)...")
raw_6o <- read_excel(excel_file, sheet = "Figure 6o", col_names = FALSE)
eos_vals   <- as.numeric(as.character(unlist(raw_6o[2, 3:59])))
gzmk_vals2 <- as.numeric(as.character(unlist(raw_6o[3, 3:59])))
df_6o <- data.frame(
  eos = eos_vals[!is.na(eos_vals)],
  gzmk = gzmk_vals2[!is.na(gzmk_vals2)]
)

p_6o <- ggplot(df_6o, aes(x = gzmk, y = eos)) +
  geom_point(size = 1.6, color = "#EE1A1A", alpha = 0.85) +
  geom_smooth(method = "lm", se = FALSE, color = "#EE1A1A", linewidth = 0.65) +
  annotate("text", x = 0.2, y = 50, label = "R = 0.151\nP = 0.262", hjust = 0, size = 2.6, fontface = "italic") +
  labs(
    tag = "o",
    x = expression("GZMK"^{"+"}~"CD8"^{"+"}~"T cell count"),
    y = "Eosinophil cell count"
  ) +
  scale_x_continuous(breaks = seq(0, 8, 2), limits = c(0, 8.5)) +
  scale_y_continuous(breaks = seq(0, 60, 20), limits = c(0, 60)) +
  theme_natcom()

# ------------------------------------------------------------------------------
# Save Individual Panels
# ------------------------------------------------------------------------------
save_panel <- function(p, filename, width, height) {
  p_path <- file.path(out_dir, filename)
  b_path <- file.path(brain_dir, filename)
  ggsave(p_path, p, width = width, height = height, dpi = 300)
  ggsave(b_path, p, width = width, height = height, dpi = 300)
}

save_panel(p_6c, "NatCommun_Figure6c_Fibroblast_Density.png", 3.8, 3.4)
save_panel(p_6d, "NatCommun_Figure6d_Fibroblast_Distance.png", 3.8, 3.4)
save_panel(p_6g, "NatCommun_Figure6g_NPDF_qPCR.png", 6.8, 3.8)
save_panel(p_6h, "NatCommun_Figure6h_NPDF_Protein.png", 6.8, 3.8)
save_panel(p_6i, "NatCommun_Figure6i_Supernatant_qPCR.png", 6.8, 3.8)
save_panel(p_6j, "NatCommun_Figure6j_Chemotaxis.png", 3.4, 3.4)
save_panel(p_6m, "NatCommun_Figure6m_MPO_Quantification.png", 2.6, 3.4)
save_panel(p_6n, "NatCommun_Figure6n_Correlation_Neutrophils.png", 3.2, 3.4)
save_panel(p_6o, "NatCommun_Figure6o_Correlation_Eosinophils.png", 3.2, 3.4)

# ------------------------------------------------------------------------------
# Multi-Panel Composite Assembly
# ------------------------------------------------------------------------------
message("Assembling Complete Figure 6 Composite...")

top_row <- p_6c + p_6d + p_6j + plot_layout(widths = c(1.2, 1.2, 0.9))
mid_row1 <- p_6g
mid_row2 <- p_6h
bot_row <- p_6m + p_6n + p_6o + plot_layout(widths = c(0.8, 1.1, 1.1))

p_fig6_comp <- top_row / mid_row1 / mid_row2 / bot_row +
  plot_layout(heights = c(1.0, 1.1, 1.1, 1.0)) +
  plot_annotation(
    title = "Figure 6 | Interaction between GZMK+CD8+ T cells and fibroblasts contributes to neutrophilic inflammation in nasal polyps",
    theme = theme(
      plot.title = element_text(size = 11, face = "bold", hjust = 0),
      plot.margin = margin(8, 8, 8, 8)
    )
  )

save_panel(p_fig6_comp, "NatCommun_Figure6_Complete_Composite.png", 11.5, 13.5)

message("ALL FIGURE 6 PANELS GENERATED SUCCESSFULLY!")
