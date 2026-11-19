# ==============================================================================
# build_microbiome_network_figures.R
# Production R script applying ggClusterNet 2.0 multi-network architectures
# to clinical metagenomics, multi-omics cohorts, and trial datasets
# ==============================================================================

suppressPackageStartupMessages({
  library(cliomicplot)
  library(ggplot2)
  library(patchwork)
})

set.seed(42)

# ==============================================================================
# 1. INPUT DATA FORMATS
# ==============================================================================
# nodes: data.frame with columns: id, group, module, degree (optional)
# edges: data.frame with columns: from, to, group
# cross_edges (optional): data.frame with columns: from, to, group1, group2
# env_cors (optional): named list of correlation matrices for the 4 corners

demo_data <- generate_demo_cross_network()

# ==============================================================================
# 2. PANEL (A): MULTI-GROUP NODE TOPOLOGY
# ==============================================================================
# Visualizes full network topology across conditions with modular clusters,
# shaded module hulls, intra-group edges, and conserved cross-group links.

p_a <- cliplot_cross_network_graph(
  nodes = demo_data$nodes,
  edges = demo_data$edges,
  cross_edges = demo_data$cross_edges,
  level = "node",
  title = "(a) Multi-Group Cross-Network Microbial Architecture"
)

# ==============================================================================
# 3. PANEL (B): MODULE-LEVEL PRESERVATION RINGS
# ==============================================================================
# Visualizes module-level preservation rings across conditions with nodes
# sized by module member count and cross-group preservation links.

mod_summary <- aggregate(degree ~ module + group, data = demo_data$nodes, FUN = length)
names(mod_summary)[3] <- "degree"
mod_summary$id <- paste0(mod_summary$group, "_", mod_summary$module)

p_b <- cliplot_cross_network_graph(
  nodes = mod_summary,
  level = "module",
  title = "(b) Module-Level Cross-Network Preservation"
)

# ==============================================================================
# 4. PANEL (C): CENTRAL MODULAR NETWORK COUPLED WITH CORNER CORRELATIONS
# ==============================================================================
# Couples a central circular modular network with peripheral triangular
# correlation heatmaps of environmental, clinical, or host indicators.

# Example 4-domain correlation matrices
env_domains <- list(
  Biophysical_Barrier = c("Skin_pH", "TEWL_Barrier", "Corneometry", "Sebum_Index", "Ceramide_NP"),
  Clinical_Atopy      = c("SCORAD_Total", "EASI_Score", "Pruritus_VAS", "Total_IgE", "Eosinophils"),
  Host_Biomarkers     = c("FLG_Null_Mutation", "TSLP_Serum", "IL4_IL13_Ratio", "IL31_Pruritus", "CCL17_TARC"),
  Exposome_Factors    = c("PM25_AirPollution", "Relative_Humidity", "Ambient_Temp", "UV_Index", "Hard_Water_Index")
)

make_mat <- function(vars) {
  p <- length(vars)
  m <- matrix(runif(p*p, -0.65, 0.75), nrow = p, ncol = p)
  m <- (m + t(m)) / 2
  diag(m) <- 1.0
  colnames(m) <- vars
  rownames(m) <- vars
  m
}

env_cors <- list(
  "Biophysical Barrier"    = make_mat(env_domains$Biophysical_Barrier),
  "Clinical Atopy"         = make_mat(env_domains$Clinical_Atopy),
  "Host Biomarkers"        = make_mat(env_domains$Host_Biomarkers),
  "Environmental Exposome" = make_mat(env_domains$Exposome_Factors)
)

p_c <- cliplot_env_network(
  env_cors    = env_cors,
  show_values = TRUE,  # Display compact, contrast-aware correlation coefficients
  value_digits = 2,    # Number of decimal places
  compact     = TRUE,  # Strip leading zero (.42 instead of 0.42) to fit snugly in tiles
  title       = "(c) Central Modular Network Coupled with Environmental Drivers"
)

# ==============================================================================
# 5. ASSEMBLE COMPOSITE FIGURE
# ==============================================================================
p_a_clean <- p_a + theme(legend.position = "none")
p_b_clean <- p_b + theme(legend.position = "none")

fig_composite <- (p_a_clean / p_b_clean) | p_c
fig_composite <- fig_composite + plot_layout(widths = c(1, 1.35))

# Export
# ggsave("figure3_composite.png", fig_composite, width = 16.5, height = 9.5, dpi = 300)
# ggsave("figure3_composite.pdf", fig_composite, width = 16.5, height = 9.5)
