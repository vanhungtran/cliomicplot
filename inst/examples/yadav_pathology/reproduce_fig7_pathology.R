#!/usr/bin/env Rscript
# ==============================================================================
# Reproduce Fig 7 Pathology Boxplots (Yadav et al. 2026, Nature Aging)
# Using cliomicplot's type_pathology_boxplot() and cliplot()
# ==============================================================================

suppressPackageStartupMessages({
  library(cliomicplot)
  library(ggplot2)
})

ct_dir <- "C:/Users/lucas/OneDrive/Yadav2026/results/cross_tissue"
long_file <- file.path(ct_dir, "pathology_long_table.tsv")
summary_file <- file.path(ct_dir, "pathology_associations.tsv")

if (!file.exists(long_file) || !file.exists(summary_file)) {
  stop("Input files not found in ", ct_dir)
}

long_df    <- read.delim(long_file, sep = "\t", stringsAsFactors = FALSE)
summary_df <- read.delim(summary_file, sep = "\t", stringsAsFactors = FALSE)

keywords   <- c("calcif", "fibros", "atroph")
disp_names <- c(calcif = "Calcification", fibros = "Fibrosis", atroph = "Atrophy")

feature_levels <- sapply(keywords, function(kw) {
  n_kw <- summary_df$n_with[summary_df$keyword == kw]
  sprintf("%s\n(n = %d)", disp_names[[kw]], n_kw)
})

plot_list <- lapply(seq_along(keywords), function(i) {
  kw <- keywords[i]
  disp <- disp_names[[kw]]
  n_kw <- summary_df$n_with[summary_df$keyword == kw]
  f_name <- sprintf("%s\n(n = %d)", disp, n_kw)

  has_kw <- grepl(kw, long_df$SMPTHNTS, ignore.case = TRUE)
  sub_df <- data.frame(
    keyword = kw,
    feature = factor(f_name, levels = feature_levels),
    status  = factor(ifelse(has_kw, "Present", "Absent (~20k)"), levels = c("Absent (~20k)", "Present")),
    delta   = long_df$delta,
    stringsAsFactors = FALSE
  )
  sub_df[!is.na(sub_df$delta), ]
})

plot_df <- do.call(rbind, plot_list)

# Render figure with cliplot using type_pathology_boxplot
p <- cliplot(
  delta ~ feature | status,
  data = plot_df,
  type = type_pathology_boxplot(
    width            = 0.30,
    position_dodge   = 0.42,
    alpha            = 0.22,
    linewidth        = 0.40,
    jitter_subsample = 150,
    jitter_size      = 0.55,
    jitter_alpha     = 0.38,
    mean_size        = 1.3,
    mean_stroke      = 0.45,
    bold_median_lwd  = 0.80
  ),
  palette    = c("Absent (~20k)" = "#0F4D92", "Present" = "#D55E00"),
  theme      = "pathology",
  stat.test  = "t.test",
  stat.label = "p.nature",
  stat.y     = c(0.44, 0.46, 0.62),
  xlab       = "Microscopic pathology finding",
  ylab       = expression(paste("Structural aging ", Delta, " score"))
) +
  ggplot2::scale_y_continuous(breaks = seq(-0.2, 0.6, by = 0.2)) +
  ggplot2::coord_cartesian(ylim = c(-0.32, 0.76)) +
  ggplot2::labs(tag = "a") +
  ggplot2::theme(
    plot.tag         = ggplot2::element_text(face = "bold", size = 8.0, vjust = 1.5),
    legend.position  = c(0.24, 0.94),
    legend.direction = "horizontal"
  )

# Set precise Nature Aging single-column layout dimensions (89 mm x 66 mm)
out_dir <- "inst/examples/yadav_pathology"
if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)

out_png <- file.path(out_dir, "fig7_pathology_boxplots_cliomicplot.png")
ggsave(out_png, plot = p, width = 89, height = 66, units = "mm", dpi = 300)
message("Saved reproduced figure to: ", out_png)
