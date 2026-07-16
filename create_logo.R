# ===========================================================================
# cliomicplot hex logo — stylized volcano plot
# Clinical / multi-omics identity: significance + fold-change
# ===========================================================================
library(hexSticker)
library(ggplot2)
library(showtext)

font_add_google("Righteous", "righteous")
showtext_auto()

set.seed(2026)

# Generate a compact volcano scatter for the logo
n <- 400
logo_data <- data.frame(
  x = c(rnorm(n - 40, 0, 0.5),
        rnorm(25,  1.8, 0.3),
        rnorm(15, -1.8, 0.3)),
  y = c(runif(n - 40, 0, 2.5),
        10^runif(40, 0.3, 1.5))
)

# Classification
fc_thresh <- 0.6
sig_thresh <- 1.3
logo_data$class <- "ns"
logo_data$class[logo_data$y > sig_thresh & logo_data$x > fc_thresh]  <- "up"
logo_data$class[logo_data$y > sig_thresh & logo_data$x < -fc_thresh] <- "down"

p <- ggplot(logo_data, aes(x = x, y = y, color = class)) +
  # Horizontal significance threshold
  geom_hline(yintercept = sig_thresh, linetype = "dashed",
             linewidth = 0.3, color = "#8899AA") +
  # Vertical FC thresholds
  geom_vline(xintercept = c(-fc_thresh, fc_thresh), linetype = "dashed",
             linewidth = 0.25, color = "#8899AA") +
  # Points
  geom_point(alpha = 0.85, size = 1.4) +
  scale_color_manual(values = c(
    "up"   = "#D73027",
    "down" = "#2B6CB0",
    "ns"   = "#B0B8C1"
  )) +
  coord_cartesian(xlim = c(-2.5, 2.5), ylim = c(-0.2, 4.2)) +
  theme_void() +
  theme(
    legend.position = "none",
    plot.background = element_rect(fill = "transparent", color = NA),
    panel.background = element_rect(fill = "transparent", color = NA)
  )

sticker(p,
  package        = "cliomicplot",
  p_size         = 18,
  p_color        = "#0D3B66",
  p_family       = "righteous",
  p_y            = 1.45,
  s_x            = 1,
  s_y            = 0.72,
  s_width        = 1.25,
  s_height       = 1.0,
  h_fill         = "#F8F9FA",
  h_color        = "#0073C2",
  h_size         = 1.8,
  filename       = "man/figures/logo.png",
  dpi            = 300,
  white_around_sticker = FALSE
)

cat("Logo saved to man/figures/logo.png\n")
