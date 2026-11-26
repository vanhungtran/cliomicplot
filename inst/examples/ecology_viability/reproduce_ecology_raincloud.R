#!/usr/bin/env Rscript
# ==============================================================================
# Reproduce Ecology / Demographic Viability Raincloud Plot
# Using cliomicplot's type_ecology_raincloud(), theme_cli_ecology(), and cliplot()
# ==============================================================================

suppressPackageStartupMessages({
  library(cliomicplot)
  library(ggplot2)
})

set.seed(42)
n <- 400
y_2yr  <- rnorm(n, mean = 0.869, sd = 0.055)
y_5yr  <- rnorm(n, mean = 0.713, sd = 0.042)
y_10yr <- rnorm(n, mean = 0.727, sd = 0.045)
# Add outlier replicates matching the open square glyphs
y_2yr[1:5]  <- c(1.02, 1.04, 1.05, 1.06, 0.71)
y_5yr[1:3]  <- c(0.85, 0.61, 0.59)
y_10yr[1:4] <- c(0.85, 0.84, 0.62, 0.60)

reseeding_df <- data.frame(
  years = factor(rep(c("2 yr", "5 yr", "10 yr"), each = n), levels = c("2 yr", "5 yr", "10 yr")),
  growth_rate = c(y_2yr, y_5yr, y_10yr)
)

# Render with cliplot
p <- cliplot(
  growth_rate ~ years,
  data = reseeding_df,
  type = type_ecology_raincloud(
    ref_line       = 1.0,
    show_stats     = TRUE,
    stat_param     = "\u03bb",
    stat_digits    = 3,
    stat_y_pos     = 0.52
  ),
  palette = "ecology",
  theme   = "ecology",
  xlab    = "Reseeding years",
  ylab    = expression(paste("population growth rate (", lambda, ")"))
) +
  ggplot2::scale_y_continuous(
    limits = c(0.48, 1.11),
    breaks = seq(0.5, 1.1, by = 0.1)
  )

out_dir <- "inst/examples/ecology_viability"
if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)

out_png <- file.path(out_dir, "ecology_reseeding_raincloud.png")
ggsave(out_png, plot = p, width = 7.0, height = 5.2, dpi = 300)
message("Saved reproduced ecology raincloud figure to: ", out_png)
