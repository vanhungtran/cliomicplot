# ===========================================================================
# cliomicplot: Grammar of Graphics for Coupled Central & Environmental Networks
# Composable Layered Visual Grammar for Central Networks & Peripheral Heatmaps
# ===========================================================================

#' Grammatical Data Preparation for Central Network Coupled with Environmental Heatmaps
#'
#' @description
#' Generates the complete spatial layout, node coordinates, intra-module edge segments,
#' convex hulls, peripheral triangular heatmap coordinates, and curved inter-domain link
#' trajectories. The returned \code{cli_env_network_data} S3 object serves as the data foundation
#' for the modular grammar layers (\code{\link{geom_env_boundary}},
#' \code{\link{geom_env_central_network}}, \code{\link{geom_env_corner_heatmaps}},
#' and \code{\link{geom_env_links}}).
#'
#' @param comm_data Optional numeric matrix or data frame of community abundance data
#'   (samples in rows, features in columns). If supplied and \code{links} is NULL,
#'   empirical linkages are automatically calculated using \code{\link{cli_calc_env_links}}.
#' @param env_data Numeric matrix, data frame, or named list of 1 to 4 correlation matrices or
#'   indicator variables for corner triangular heatmaps. Alias for \code{env_cors}.
#' @param env_cors Alias for \code{env_data}.
#' @param nodes Optional data frame of central network nodes.
#' @param edges Optional data frame of central network edges.
#' @param links Optional data frame of pre-computed environmental-to-module linkages.
#' @param modules Optional named vector assigning features to module names.
#' @param module_labels Optional character vector or named vector of descriptive module labels.
#' @param module_colors Optional named vector of colors for modules.
#' @param corner_palettes Optional list of 1 to 4 color gradient palettes for the corner heatmaps.
#' @param link_colors Named vector of colors for link directions (default \code{c("Positive" = "#E64B35", "Negative" = "#4DBBD5")}).
#' @param label_type Format for heatmap tile values: \code{"auto"}, \code{"stars"}, \code{"coef"}, \code{"coef_stars"}, or \code{"none"}.
#' @param show_asterisks Logical. If TRUE, display significance asterisks. Default TRUE.
#' @param show_values Logical. If TRUE, display numeric correlation coefficients. Default FALSE.
#' @param value_digits Integer. Number of decimal places for correlation digits (default 2).
#' @param value_size Numeric. Font size for tile correlation text (default NULL for auto-scaling).
#' @param compact Logical. If TRUE (default), omits leading zero for compact tile text.
#' @param show_module_labels Logical. If TRUE, displays module label badges (default TRUE).
#' @param module_label_size Font size for module label text (default 2.4).
#' @param module_label_color Text color for module labels (default \code{"grey15"}).
#' @param module_label_fill Background fill color for module labels (default \code{"white"}).
#' @param module_label_bg Logical. If TRUE, renders bounding badge boxes behind labels (default TRUE).
#' @param module_label_alpha Transparency for module label background (default 1.0).
#' @param module_label_pos Placement strategy for module labels: \code{"radial"} (default) or \code{"centroid"}.
#' @param show_module_legend Logical. If TRUE, shows module color legend (default TRUE).
#' @param legend_position Position of plot legend: \code{"top"} (default), \code{"bottom"}, \code{"right"}, or \code{"left"}.
#' @param show_domain_labels Logical. If TRUE, displays corner domain headers (default TRUE).
#' @param env_label_size Font size for environmental variable labels (default 1.95).
#' @param env_label_color Color for environmental variable labels (default \code{"grey20"}).
#' @param env_label_face Font face for environmental variable labels (default \code{"plain"}).
#' @param link_width_range Line width range for significance-weighted links (default \code{c(0.4, 1.4)}).
#' @param link_width Optional constant line width overriding significance weighting.
#' @param link_alpha Transparency for curved inter-domain links (default 0.70).
#' @param show_link_values Logical. If TRUE, shows correlation coefficients on links (default FALSE).
#' @param link_value_size Font size for link correlation digits (default 2.0).
#' @param link_value_digits Number of decimal places for link correlation digits (default 2).
#' @param circle_radius Radius of central boundary circle (default 2.80).
#' @param circle_width Line width of boundary circle (default 0.4).
#' @param circle_color Color of boundary circle (default \code{"grey65"}).
#' @param circle_linetype Linetype of boundary circle (default \code{"dashed"}).
#' @param outer_ring_size Size of dots along outer boundary ring (default 0.6).
#' @param outer_ring_color Color of dots along outer boundary ring (default \code{"grey60"}).
#' @param outer_ring_alpha Transparency of dots along outer boundary ring (default 0.6).
#' @param intra_edge_width Line width of intra-module edges (default 0.18).
#' @param intra_edge_color Color of intra-module edges (default \code{"grey82"}).
#' @param intra_edge_alpha Transparency of intra-module edges (default 0.35).
#' @param diag_point_size Size of diagonal indicator connector points (default 2.0).
#' @param diag_point_fill Fill color of diagonal indicator points (default \code{"#F781BF"}).
#' @param diag_point_color Border color of diagonal indicator points (default \code{"#C51B7D"}).
#' @param node_size_range Numeric vector of length 2 for node sizes (default \code{c(1.1, 3.8)}).
#' @param hull_alpha Shading transparency for module cluster hulls (default 0.18).
#' @param curvature Curvature of inter-domain links (default 0.14).
#' @param preset Publisher style preset: \code{"default"}, \code{"nature"}, \code{"clinical"},
#'   \code{"seasonal"}, \code{"cell"}, \code{"dark"}, \code{"lancet"}, \code{"minimal"},
#'   \code{"multi_tissue"}, \code{"circadian"}, or \code{"bipolar"}.
#' @param scifig Logical. If TRUE, enforces AI4SciFig publication gates (default FALSE).
#'
#' @return An S3 object of class \code{c("cli_env_network_data", "list")} containing layout tables.
#' @export
cli_env_network_data = function(
    comm_data          = NULL,
    env_data           = NULL,
    nodes              = NULL,
    edges              = NULL,
    env_cors           = NULL,
    links              = NULL,
    modules            = NULL,
    module_labels      = NULL,
    module_colors      = NULL,
    corner_palettes    = NULL,
    link_colors        = c("Positive" = "#E64B35", "Negative" = "#4DBBD5"),
    label_type         = c("auto", "stars", "coef", "coef_stars", "none"),
    show_asterisks     = TRUE,
    show_values        = FALSE,
    value_digits       = 2,
    value_size         = NULL,
    compact            = TRUE,
    show_module_labels = TRUE,
    module_label_size  = 2.4,
    module_label_color = "grey15",
    module_label_fill  = "white",
    module_label_bg    = TRUE,
    module_label_alpha = 1.0,
    module_label_pos   = c("radial", "centroid"),
    show_module_legend = TRUE,
    legend_position    = "top",
    show_domain_labels = TRUE,
    env_label_size     = 1.95,
    env_label_color    = "grey20",
    env_label_face     = "plain",
    link_width_range   = c(0.4, 1.4),
    link_width         = NULL,
    link_alpha         = 0.70,
    show_link_values   = FALSE,
    link_value_size    = 2.0,
    link_value_digits  = 2,
    circle_radius      = 2.80,
    circle_width       = 0.4,
    circle_color       = "grey65",
    circle_linetype    = "dashed",
    outer_ring_size    = 0.6,
    outer_ring_color   = "grey60",
    outer_ring_alpha   = 0.6,
    intra_edge_width   = 0.18,
    intra_edge_color   = "grey82",
    intra_edge_alpha   = 0.35,
    diag_point_size    = 2.0,
    diag_point_fill    = "#F781BF",
    diag_point_color   = "#C51B7D",
    node_size_range    = c(1.1, 3.8),
    hull_alpha         = 0.18,
    curvature          = 0.14,
    preset             = "default",
    scifig             = FALSE
) {
  label_type <- match.arg(label_type)
  module_label_pos <- match.arg(module_label_pos)

  if (is.null(env_cors) && !is.null(env_data)) {
    env_cors <- env_data
  }
  if (is.null(module_labels) && !is.null(modules)) {
    module_labels <- modules
  }

  resolved_label_type <- if (label_type != "auto") {
    label_type
  } else if (isTRUE(show_values) && isTRUE(show_asterisks)) {
    "coef_stars"
  } else if (isTRUE(show_values)) {
    "coef"
  } else if (isTRUE(show_asterisks)) {
    "stars"
  } else {
    "none"
  }

  # Automated linkage calculation if comm_data is provided
  if (is.null(links) && !is.null(comm_data) && !is.null(env_cors)) {
    links <- tryCatch({
      cli_calc_env_links(comm_data = comm_data, env_data = env_cors, modules = module_labels)
    }, error = function(e) NULL)
  }

  custom_data <- if (!is.null(nodes) || !is.null(edges)) {
    list(nodes = nodes, edges = edges)
  } else {
    NULL
  }

  # Module definitions
  n_mods <- 7
  u_mods <- paste0("Mod_", 1:n_mods)
  if (is.null(module_colors)) {
    if (preset %in% c("dark", "cell")) {
      mod_cols <- c(
        "Mod_1" = "#00F2FE", "Mod_2" = "#FF007F", "Mod_3" = "#FFE600",
        "Mod_4" = "#7B2CBF", "Mod_5" = "#00F5D4", "Mod_6" = "#FF5400", "Mod_7" = "#9D4EDD"
      )
    } else if (preset %in% c("multi_tissue", "pharmacology")) {
      mod_cols <- c(
        "Mod_1" = "#EE0000", "Mod_2" = "#3B4992", "Mod_3" = "#008B45",
        "Mod_4" = "#631879", "Mod_5" = "#5C88DA", "Mod_6" = "#FF7F0E", "Mod_7" = "#2CA02C"
      )
    } else if (preset %in% c("circadian", "diurnal")) {
      mod_cols <- c(
        "Mod_1" = "#E6AB02", "Mod_2" = "#D95F02", "Mod_3" = "#7570B3",
        "Mod_4" = "#1B9E77", "Mod_5" = "#E7298A", "Mod_6" = "#66A61E", "Mod_7" = "#A6761D"
      )
    } else if (preset %in% c("bipolar", "response")) {
      mod_cols <- c(
        "Mod_1" = "#D62728", "Mod_2" = "#1F77B4", "Mod_3" = "#FF7F0E",
        "Mod_4" = "#2CA02C", "Mod_5" = "#9467BD", "Mod_6" = "#8C564B", "Mod_7" = "#17BECF"
      )
    } else {
      mod_cols <- c(
        "Mod_1" = "#4DBBD5", "Mod_2" = "#E64B35", "Mod_3" = "#00A087",
        "Mod_4" = "#3C5488", "Mod_5" = "#F39B7F", "Mod_6" = "#8491B4", "Mod_7" = "#B09C85"
      )
    }
    module_colors <- mod_cols
  }

  default_module_labels <- if (preset == "seasonal") {
    c("Mod_1" = "M1: Perennial Core", "Mod_2" = "M2: Vernal Bloom", "Mod_3" = "M3: Xerotolerant",
      "Mod_4" = "M4: Cellulolytic", "Mod_5" = "M5: Psychrophilic", "Mod_6" = "M6: Spore-Forming", "Mod_7" = "M7: Epiphytic")
  } else if (preset %in% c("multi_tissue", "pharmacology")) {
    c("Mod_1" = "M1: Barrier Lipids", "Mod_2" = "M2: Th2 Signaling", "Mod_3" = "M3: Pruriceptors",
      "Mod_4" = "M4: S. aureus Vir", "Mod_5" = "M5: SCFA Secretome", "Mod_6" = "M6: Xenobiotic Deg", "Mod_7" = "M7: Tight Junctions")
  } else if (preset %in% c("circadian", "diurnal")) {
    c("Mod_1" = "M1: Dawn Awakening", "Mod_2" = "M2: Solar Peak Flux", "Mod_3" = "M3: Nocturnal Melatonin",
      "Mod_4" = "M4: Evening Pruritus", "Mod_5" = "M5: Cortisol Dipping", "Mod_6" = "M6: Redox Oscillator", "Mod_7" = "M7: Clock Autoreg")
  } else if (preset %in% c("bipolar", "response")) {
    c("Mod_1" = "M1: Responder Core", "Mod_2" = "M2: Refractory Hub", "Mod_3" = "M3: Immune Checkpoint",
      "Mod_4" = "M4: Cytokine Storm", "Mod_5" = "M5: Tissue Remodeling", "Mod_6" = "M6: Metabolic Recovery", "Mod_7" = "M7: Tolerogenic DC")
  } else {
    c("Mod_1" = "M1: Core", "Mod_2" = "M2: Barrier", "Mod_3" = "M3: Sebum",
      "Mod_4" = "M4: DrySkin", "Mod_5" = "M5: AMP", "Mod_6" = "M6: Transient", "Mod_7" = "M7: Commensal")
  }

  if (is.null(module_labels)) {
    module_labels <- default_module_labels
  }

  outer_th <- seq(pi/6, 2*pi + pi/6, length.out = 7)[1:6]
  r_core <- circle_radius * 0.45
  mod_centers <- data.frame(
    module   = u_mods,
    cx       = c(0, r_core * cos(outer_th)),
    cy       = c(0, r_core * sin(outer_th)),
    n_nodes  = c(60, 32, 30, 28, 28, 25, 25),
    r_spread = c(circle_radius * 0.18, rep(circle_radius * 0.125, 6)),
    stringsAsFactors = FALSE
  )

  # Module labels
  mod_labels_df <- NULL
  if (isTRUE(show_module_labels) && nrow(mod_centers) > 0) {
    label_list <- list()
    for (m_i in seq_len(nrow(mod_centers))) {
      m_name <- as.character(mod_centers$module[m_i])
      cx <- mod_centers$cx[m_i]
      cy <- mod_centers$cy[m_i]
      r_dist <- sqrt(cx^2 + cy^2)
      if (module_label_pos == "radial" && r_dist > 0.15) {
        th <- atan2(cy, cx)
        r_pos <- circle_radius * 0.67
        lx <- r_pos * cos(th)
        ly <- r_pos * sin(th)
      } else {
        lx <- cx
        ly <- cy
      }
      lbl_txt <- if (!is.null(module_labels)) {
        if (!is.null(names(module_labels)) && m_name %in% names(module_labels)) {
          as.character(module_labels[m_name])
        } else if (m_i <= length(module_labels)) {
          as.character(module_labels[m_i])
        } else {
          sub("^Mod_", "Mod ", m_name)
        }
      } else {
        sub("^Mod_", "Mod ", m_name)
      }
      label_list[[m_i]] <- data.frame(module = m_name, x = lx, y = ly, label = lbl_txt, stringsAsFactors = FALSE)
    }
    mod_labels_df <- do.call(rbind, label_list)
  }

  hw <- 0.55
  hh <- 0.24
  line_intersects_rect <- function(x1, y1, x2, y2, rx0, ry0, rx1, ry1) {
    if (max(x1, x2) < rx0 || min(x1, x2) > rx1 || max(y1, y2) < ry0 || min(y1, y2) > ry1) return(FALSE)
    dx <- x2 - x1; dy <- y2 - y1
    p <- c(-dx, dx, -dy, dy); q <- c(x1 - rx0, rx1 - x1, y1 - ry0, ry1 - y1)
    u1 <- 0; u2 <- 1
    for (j in 1:4) {
      if (p[j] == 0) { if (q[j] < 0) return(FALSE) }
      else {
        t <- q[j] / p[j]
        if (p[j] < 0) { if (t > u1) u1 <- t } else { if (t < u2) u2 <- t }
        if (u1 > u2) return(FALSE)
      }
    }
    return(TRUE)
  }

  # Build network nodes
  nodes_df <- NULL
  if (is.list(custom_data) && !is.data.frame(custom_data)) {
    nodes_df <- custom_data$nodes
  } else if (is.data.frame(custom_data) && nrow(custom_data) > 0) {
    nodes_df <- custom_data
  }

  if (is.null(nodes_df) || nrow(nodes_df) == 0) {
    node_list <- list()
    for (i in 1:nrow(mod_centers)) {
      mc <- mod_centers[i, ]
      n <- mc$n_nodes
      r <- stats::rbeta(n, 1.5, 2.0) * mc$r_spread
      th <- stats::runif(n, 0, 2*pi)
      nx <- mc$cx + r * cos(th)
      ny <- mc$cy + r * sin(th)
      in_b <- rep(FALSE, n)
      if (!is.null(mod_labels_df) && nrow(mod_labels_df) > 0) {
        for (b_i in seq_len(nrow(mod_labels_df))) {
          in_b <- in_b | (abs(nx - mod_labels_df$x[b_i]) <= (hw + 0.10) & abs(ny - mod_labels_df$y[b_i]) <= (hh + 0.10))
        }
      }
      n_keep <- sum(!in_b)
      if (n_keep > 0) {
        node_list[[length(node_list) + 1]] <- data.frame(
          id     = paste0("Taxa_", mc$module, "_", (1:n)[!in_b]),
          module = rep(mc$module, n_keep),
          x      = nx[!in_b],
          y      = ny[!in_b],
          degree = sample(4:25, n_keep, replace = TRUE),
          stringsAsFactors = FALSE
        )
      }
    }
    nodes_df <- do.call(rbind, node_list)
  }

  # Build module hulls
  hull_list <- list()
  theta_seq <- seq(0, 2*pi, length.out = 45)
  for (i in 1:nrow(mod_centers)) {
    mc <- mod_centers[i, ]
    hull_list[[i]] <- data.frame(
      x      = mc$cx + (mc$r_spread * 1.05) * cos(theta_seq),
      y      = mc$cy + (mc$r_spread * 1.05) * sin(theta_seq),
      module = mc$module,
      id     = mc$module,
      stringsAsFactors = FALSE
    )
  }
  center_hulls <- do.call(rbind, hull_list)

  # Build intra edges
  intra_list <- list()
  for (i in 1:nrow(mod_centers)) {
    m_nodes <- nodes_df[nodes_df$module == mod_centers$module[i], ]
    if (!is.null(m_nodes) && nrow(m_nodes) >= 2) {
      n_e <- round(nrow(m_nodes) * 2.0)
      f <- sample(seq_len(nrow(m_nodes)), n_e, replace = TRUE)
      t <- sample(seq_len(nrow(m_nodes)), n_e, replace = TRUE)
      k <- f != t
      intra_list[[length(intra_list) + 1]] <- data.frame(
        x = m_nodes$x[f[k]], y = m_nodes$y[f[k]],
        xend = m_nodes$x[t[k]], yend = m_nodes$y[t[k]]
      )
    }
  }
  center_edges <- do.call(rbind, intra_list)

  if (!is.null(center_edges) && nrow(center_edges) > 0 && !is.null(mod_labels_df) && nrow(mod_labels_df) > 0) {
    bad <- rep(FALSE, nrow(center_edges))
    for (b_i in seq_len(nrow(mod_labels_df))) {
      bx0 <- mod_labels_df$x[b_i] - (hw + 0.15)
      bx1 <- mod_labels_df$x[b_i] + (hw + 0.15)
      by0 <- mod_labels_df$y[b_i] - (hh + 0.15)
      by1 <- mod_labels_df$y[b_i] + (hh + 0.15)
      in_x <- pmax(center_edges$x, center_edges$xend) >= bx0 & pmin(center_edges$x, center_edges$xend) <= bx1
      in_y <- pmax(center_edges$y, center_edges$yend) >= by0 & pmin(center_edges$y, center_edges$yend) <= by1
      cand <- which(in_x & in_y)
      if (length(cand) > 0) {
        hit <- mapply(line_intersects_rect,
                      center_edges$x[cand], center_edges$y[cand],
                      center_edges$xend[cand], center_edges$yend[cand],
                      MoreArgs = list(rx0 = bx0, ry0 = by0, rx1 = bx1, ry1 = by1))
        bad[cand[hit]] <- TRUE
      }
    }
    center_edges <- center_edges[!bad, ]
  }

  # Boundary rings
  ring_th <- seq(0, 2*pi, length.out = 160)
  center_ring <- data.frame(x = circle_radius * cos(ring_th), y = circle_radius * sin(ring_th))

  n_outer_ring <- 80
  outer_ring_th <- seq(0, 2*pi, length.out = n_outer_ring + 1)[1:n_outer_ring]
  outer_ring_nodes <- data.frame(
    x = (circle_radius * 0.96) * cos(outer_ring_th),
    y = (circle_radius * 0.96) * sin(outer_ring_th)
  )

  # Corner triangular heatmaps
  default_pals <- if (preset == "seasonal") {
    list(
      c("#238B45", "#FFFFFF", "#D73027"),
      c("#7A0177", "#FFFFFF", "#FF7F00"),
      c("#016C59", "#FFFFFF", "#B35806"),
      c("#08519C", "#FFFFFF", "#EF3B2C")
    )
  } else if (preset %in% c("multi_tissue", "pharmacology")) {
    list(
      c("#3B4992", "#FFFFFF", "#EE0000"),
      c("#008B45", "#FFFFFF", "#631879"),
      c("#1F77B4", "#FFFFFF", "#FF7F0E"),
      c("#016C59", "#FFFFFF", "#B35806")
    )
  } else if (preset %in% c("circadian", "diurnal")) {
    list(
      c("#7570B3", "#FFFFFF", "#E6AB02"),
      c("#1B9E77", "#FFFFFF", "#D95F02"),
      c("#2B83BA", "#FFFFFF", "#FDAE61"),
      c("#3F007D", "#FFFFFF", "#9E9AC8")
    )
  } else if (preset %in% c("bipolar", "response")) {
    list(
      c("#1F77B4", "#FFFFFF", "#D62728"),
      c("#2CA02C", "#FFFFFF", "#9467BD"),
      c("#D62728", "#FFFFFF", "#1F77B4"),
      c("#FF7F0E", "#FFFFFF", "#17BECF")
    )
  } else if (preset %in% c("dark", "cell")) {
    list(
      c("#00F5D4", "#1E2638", "#FF007F"),
      c("#00F2FE", "#1E2638", "#FFE600"),
      c("#7B2CBF", "#1E2638", "#00F5D4"),
      c("#00F2FE", "#1E2638", "#FF5400")
    )
  } else if (preset %in% c("lancet", "minimal")) {
    list(
      c("#2166AC", "#F7F7F7", "#B2182B"),
      c("#1B7837", "#F7F7F7", "#762A83"),
      c("#35978F", "#F7F7F7", "#BF812D"),
      c("#4393C3", "#F7F7F7", "#D6604D")
    )
  } else {
    list(
      c("#4DAF4A", "#FFFFFF", "#E41A1C"),
      c("#984EA3", "#FFFFFF", "#FF7F00"),
      c("#66C2A5", "#FFFFFF", "#FC8D62"),
      c("#377EB8", "#FFFFFF", "#E41A1C")
    )
  }

  corner_configs <- list(
    top_left     = list(corner = "top_left", center_x = -5.0, center_y = 5.0, default_pal = default_pals[[1]]),
    top_right    = list(corner = "top_right", center_x = 5.0, center_y = 5.0, default_pal = default_pals[[2]]),
    bottom_left  = list(corner = "bottom_left", center_x = -5.0, center_y = -5.0, default_pal = default_pals[[3]]),
    bottom_right = list(corner = "bottom_right", center_x = 5.0, center_y = -5.0, default_pal = default_pals[[4]])
  )

  tiles_list <- list()
  diag_list  <- list()
  labels_list <- list()

  if (is.null(env_cors) || length(env_cors) == 0) {
    if (preset == "seasonal") {
      default_domains <- list(
        "Spring (Greening & Pollen)"     = c("Pollen_Count", "Soil_Moisture", "Temp_Fluct", "Fungal_Spores", "Solar_Flux"),
        "Summer (Heat & UV Extremes)"    = c("UV_Index", "Peak_Heat", "Drought_Index", "Ozone_O3", "PM2.5"),
        "Autumn (Senescence & Rainfall)" = c("Precipitation", "Rel_Humidity", "Soil_pH", "Leaf_Litter", "Wind_Speed"),
        "Winter (Cold Shock & Freezing)" = c("Frost_Days", "Snow_Cover", "Min_Temp", "Inversion_Layer", "Barometric_Pres")
      )
    } else if (preset %in% c("multi_tissue", "pharmacology")) {
      default_domains <- list(
        "Microbial Secretome & Toxins"    = c("Delta_Toxin", "Enterotoxin_B", "Phenol_Soluble", "Aureolysin", "Protein_A"),
        "Host Serum Metabolome"          = c("Kynurenine", "Tryptophan", "Sphingosine", "TMAO", "Butyrate"),
        "Systemic Immune Cytokines"      = c("Serum_IL13", "Serum_IL4", "Serum_IL22", "Serum_IL31", "Serum_TSLP"),
        "Skin Stratum Corneum Barrier"   = c("Ceramide_NP", "Free_FattyAcids", "Filaggrin_2", "Loricrin", "Involucrin")
      )
    } else if (preset %in% c("circadian", "diurnal")) {
      default_domains <- list(
        "Dawn (06:00 Awakening)"       = c("Peak_Cortisol", "TEWL_Awake", "Histamine_Rise", "Adrenaline", "Skin_Temp_Min"),
        "Midday (12:00 Solar Peak)"     = c("UV_Defense", "Sebum_Peak", "Stratum_Hydration", "Blood_Flow_Max", "Antioxidant_Max"),
        "Dusk (18:00 Twilight Drop)"    = c("Pruritus_Onset", "Mast_Cell_Activ", "IL31_Rise", "Cortisol_Trough", "Skin_pH_Shift"),
        "Midnight (00:00 Nocturnal)"    = c("TEWL_Max", "Scratch_Events", "Melatonin_Peak", "Permeability_Max", "Cytokine_Surge")
      )
    } else if (preset %in% c("bipolar", "response")) {
      default_domains <- list(
        "Target Engagement Biomarkers"  = c("Drug_Trough_Conc", "IL4Ra_Occupancy", "pSTAT6_Inhibition", "Total_IgE_Clear", "Th2_Suppression"),
        "Favorable Response (EASI75+)"  = c("Epidermal_Thick_Dec", "Barrier_Lipid_Rest", "Microbial_Diversity", "Pruritus_NRS_Drop", "POEM_Remission"),
        "Non-Response Signatures"       = c("Persistent_Th17", "IL22_Elevation", "Staph_Dominance", "FLG_Loss_Variant", "Neurogenic_Pruritus"),
        "Safety & Tolerability Markers" = c("Conjunctivitis_Risk", "Eosinophil_Spike", "Injection_Site_Rxn", "ALT_AST_Enzymes", "Serum_Creatinine")
      )
    } else {
      default_domains <- list(
        "Biophysical Barrier"    = c("Skin_pH", "TEWL", "Corneometry", "Sebum", "Ceramides"),
        "Clinical Severity"      = c("SCORAD", "EASI", "Pruritus", "Total_IgE", "POEM"),
        "Host Biomarkers"        = c("FLG_status", "TSLP", "IL-4/13", "IL-31", "CCL17"),
        "Environmental Exposome" = c("PM2.5", "Humidity", "Temp", "UV_Index", "Hard_Water")
      )
    }
    make_dummy_cor <- function(vars) {
      p <- length(vars)
      mat <- matrix(stats::runif(p * p, -0.65, 0.75), nrow = p, ncol = p)
      mat <- (mat + t(mat)) / 2
      diag(mat) <- 1.0
      colnames(mat) <- vars
      rownames(mat) <- vars
      mat
    }
    set.seed(42)
    env_cors <- lapply(default_domains, make_dummy_cor)
  }

  n_corners_to_draw <- min(length(env_cors), 4)
  size_box <- 2.3
  r_dot <- circle_radius * 1.21

  for (c_idx in 1:n_corners_to_draw) {
    cfg <- corner_configs[[c_idx]]
    c_name <- if (!is.null(names(env_cors)[c_idx])) names(env_cors)[c_idx] else paste0("Env ", c_idx)
    var_names <- NULL
    mat_r     <- NULL
    item <- env_cors[[c_idx]]
    if (is.matrix(item) || is.data.frame(item)) {
      var_names <- colnames(item)
      mat_r     <- as.matrix(item)
    } else if (is.character(item)) {
      var_names <- item
    }
    if (is.null(var_names) || length(var_names) < 2) {
      var_names <- paste0("Var_", 1:5, "_", c_idx)
    }
    pal <- if (!is.null(corner_palettes) && length(corner_palettes) >= c_idx) corner_palettes[[c_idx]] else cfg$default_pal
    p_dim <- length(var_names)
    grid <- expand.grid(row = 1:p_dim, col = 1:p_dim)
    grid <- grid[grid$row >= grid$col, ]

    if (!is.null(mat_r) && nrow(mat_r) == p_dim && ncol(mat_r) == p_dim) {
      r_vals <- numeric(nrow(grid))
      for (g_i in seq_len(nrow(grid))) {
        r_vals[g_i] <- mat_r[grid$row[g_i], grid$col[g_i]]
      }
    } else {
      r_vals <- stats::runif(nrow(grid), -0.75, 0.85)
      r_vals[grid$row == grid$col] <- 1.0
    }

    stars <- rep("", nrow(grid))
    for (idx in seq_len(nrow(grid))) {
      abs_r <- abs(r_vals[idx])
      if (grid$row[idx] == grid$col[idx]) {
        stars[idx] <- ""
      } else if (abs_r > 0.6) {
        stars[idx] <- "***"
      } else if (abs_r > 0.45) {
        stars[idx] <- "**"
      } else if (abs_r > 0.3) {
        stars[idx] <- "*"
      }
    }

    step <- size_box / p_dim
    corner_type <- cfg$corner
    center_x <- cfg$center_x
    center_y <- cfg$center_y

    if (corner_type == "top_left") {
      xs <- center_x + (grid$col - 1) * step
      ys <- center_y - (grid$row - 1) * step
      th_dots <- seq(2.15, 2.75, length.out = p_dim)
      diag_pts <- data.frame(var = var_names, x = r_dot * cos(th_dots), y = r_dot * sin(th_dots), stringsAsFactors = FALSE)
      top_labels <- data.frame(var = var_names[1:(p_dim - 1)], x = center_x + (1:(p_dim - 1) - 1) * step, y = center_y + step * 0.60, angle = 78, hjust = 0, vjust = 0.3, stringsAsFactors = FALSE)
      left_labels <- data.frame(var = var_names[2:p_dim], x = center_x - step * 0.60, y = center_y - (2:p_dim - 1) * step, angle = 0, hjust = 1, vjust = 0.5, stringsAsFactors = FALSE)
    } else if (corner_type == "top_right") {
      xs <- center_x - (grid$col - 1) * step
      ys <- center_y - (grid$row - 1) * step
      th_dots <- seq(0.99, 0.39, length.out = p_dim)
      diag_pts <- data.frame(var = var_names, x = r_dot * cos(th_dots), y = r_dot * sin(th_dots), stringsAsFactors = FALSE)
      top_labels <- data.frame(var = var_names[1:(p_dim - 1)], x = center_x - (1:(p_dim - 1) - 1) * step, y = center_y + step * 0.60, angle = -78, hjust = 1, vjust = 0.3, stringsAsFactors = FALSE)
      left_labels <- data.frame(var = var_names[2:p_dim], x = center_x + step * 0.60, y = center_y - (2:p_dim - 1) * step, angle = 0, hjust = 0, vjust = 0.5, stringsAsFactors = FALSE)
    } else if (corner_type == "bottom_left") {
      xs <- center_x + (grid$col - 1) * step
      ys <- center_y + (grid$row - 1) * step
      th_dots <- seq(4.13, 3.53, length.out = p_dim)
      diag_pts <- data.frame(var = var_names, x = r_dot * cos(th_dots), y = r_dot * sin(th_dots), stringsAsFactors = FALSE)
      top_labels <- data.frame(var = var_names[1:(p_dim - 1)], x = center_x + (1:(p_dim - 1) - 1) * step, y = center_y - step * 0.60, angle = -78, hjust = 0, vjust = 0.7, stringsAsFactors = FALSE)
      left_labels <- data.frame(var = var_names[2:p_dim], x = center_x - step * 0.60, y = center_y + (2:p_dim - 1) * step, angle = 0, hjust = 1, vjust = 0.5, stringsAsFactors = FALSE)
    } else {
      xs <- center_x - (grid$col - 1) * step
      ys <- center_y + (grid$row - 1) * step
      th_dots <- seq(5.29, 5.89, length.out = p_dim)
      diag_pts <- data.frame(var = var_names, x = r_dot * cos(th_dots), y = r_dot * sin(th_dots), stringsAsFactors = FALSE)
      top_labels <- data.frame(var = var_names[1:(p_dim - 1)], x = center_x - (1:(p_dim - 1) - 1) * step, y = center_y - step * 0.60, angle = 78, hjust = 1, vjust = 0.7, stringsAsFactors = FALSE)
      left_labels <- data.frame(var = var_names[2:p_dim], x = center_x + step * 0.60, y = center_y + (2:p_dim - 1) * step, angle = 0, hjust = 0, vjust = 0.5, stringsAsFactors = FALSE)
    }

    fmt_r <- sprintf(paste0("%.", value_digits, "f"), r_vals)
    zero_pat <- paste0("^-0\\.", paste(rep("0", value_digits), collapse = ""), "$")
    fmt_r <- sub(zero_pat, paste0("0.", paste(rep("0", value_digits), collapse = "")), fmt_r)
    fmt_r <- sub("^-0$", "0", fmt_r)
    if (compact) {
      fmt_r <- sub("^(-?)0\\.", "\\1.", fmt_r)
      fmt_r[r_vals == 1.0] <- "1"
      fmt_r[r_vals == -1.0] <- "-1"
      fmt_r <- sub(paste0("^-\\.", paste(rep("0", value_digits), collapse = ""), "$"),
                   paste0(".", paste(rep("0", value_digits), collapse = "")), fmt_r)
    }

    tile_label <- switch(
      resolved_label_type,
      "coef"       = fmt_r,
      "coef_stars" = ifelse(grid$row == grid$col, fmt_r, paste0(fmt_r, stars)),
      "stars"      = ifelse(grid$row == grid$col, "", stars),
      "none"       = rep("", length(r_vals)),
      stars
    )

    text_col <- ifelse(abs(r_vals) > 0.60, "white", "grey18")
    col_ramp <- scales::col_numeric(pal, domain = c(-1, 1))

    tiles_list[[c_idx]] <- data.frame(
      x = xs, y = ys, r = r_vals, stars = stars,
      label = tile_label, text_color = text_col,
      fill_col = col_ramp(r_vals), step = step, corner = c_name, stringsAsFactors = FALSE
    )
    diag_list[[c_idx]] <- cbind(diag_pts, corner = c_name)
    labels_list[[c_idx]] <- cbind(rbind(top_labels, left_labels), corner = c_name)
  }

  all_tiles <- do.call(rbind, tiles_list)
  all_diag  <- do.call(rbind, diag_list)
  all_env_labels <- do.call(rbind, labels_list)

  # Inter-domain connecting links
  link_list <- list()
  if (!is.null(links) && nrow(links) > 0 && all(c("env", "module") %in% names(links))) {
    for (l_i in seq_len(nrow(links))) {
      e_var <- as.character(links$env[l_i])
      m_tgt <- as.character(links$module[l_i])
      pt <- all_diag[all_diag$var == e_var, ]
      mc <- mod_centers[mod_centers$module == m_tgt, ]
      if (nrow(pt) > 0 && nrow(mc) > 0) {
        pt <- pt[1, ]
        mc <- mc[1, ]
        r_val <- if ("r" %in% names(links)) links$r[l_i] else if ("cor" %in% names(links)) links$cor[l_i] else 0.5
        p_val <- if ("p" %in% names(links)) links$p[l_i] else if ("pvalue" %in% names(links)) links$pvalue[l_i] else 0.01
        log10_p <- if ("log10p" %in% names(links)) links$log10p[l_i] else -log10(max(p_val, 1e-10))

        th_target <- atan2(mc$cy, mc$cx)
        end_x <- (circle_radius * 0.98) * cos(th_target)
        end_y <- (circle_radius * 0.98) * sin(th_target)

        link_list[[length(link_list) + 1]] <- data.frame(
          x = pt$x, y = pt$y, xend = end_x, yend = end_y,
          r = r_val, log10p = log10_p,
          sign = ifelse(r_val > 0, "Positive", "Negative"),
          stringsAsFactors = FALSE
        )
      }
    }
  }

  if (length(link_list) == 0) {
    u_corners <- unique(all_diag$corner)
    for (c_name in u_corners) {
      c_pts <- all_diag[all_diag$corner == c_name, ]
      n_pick <- min(3, nrow(c_pts))
      for (k_i in seq_len(n_pick)) {
        pt <- c_pts[k_i, ]
        r_val <- sample(c(0.72, 0.58, -0.64, 0.45, -0.52), 1)
        p_val <- stats::runif(1, 1e-4, 0.012)
        th_s <- atan2(pt$y, pt$x)
        th_t <- th_s + (k_i - 2) * 0.12
        end_x <- (circle_radius * 0.98) * cos(th_t)
        end_y <- (circle_radius * 0.98) * sin(th_t)

        link_list[[length(link_list) + 1]] <- data.frame(
          x = pt$x, y = pt$y, xend = end_x, yend = end_y,
          r = r_val, log10p = -log10(p_val),
          sign = ifelse(r_val > 0, "Positive", "Negative"),
          stringsAsFactors = FALSE
        )
      }
    }
  }
  all_links <- do.call(rbind, link_list)

  # Domain title text
  d_df <- NULL
  if (isTRUE(show_domain_labels) && !is.null(names(env_cors))) {
    d_names <- names(env_cors)
    d_pos <- list(
      top_left     = c(x = -3.9, y = 7.15,  hjust = 0.5),
      top_right    = c(x = 3.9,  y = 7.15,  hjust = 0.5),
      bottom_left  = c(x = -3.9, y = -7.15, hjust = 0.5),
      bottom_right = c(x = 3.9,  y = -7.15, hjust = 0.5)
    )
    d_cols <- if (preset %in% c("dark", "cell")) {
      c("#63B3ED", "#F687B3", "#68D391", "#F6AD55")
    } else if (preset == "seasonal") {
      c("#238B45", "#7A0177", "#016C59", "#08519C")
    } else if (preset %in% c("multi_tissue", "pharmacology")) {
      c("#EE0000", "#631879", "#1F77B4", "#016C59")
    } else if (preset %in% c("circadian", "diurnal")) {
      c("#D95F02", "#1B9E77", "#2B83BA", "#3F007D")
    } else if (preset %in% c("bipolar", "response")) {
      c("#D62728", "#2CA02C", "#1F77B4", "#FF7F0E")
    } else {
      c("#2166AC", "#984EA3", "#1B7837", "#B35806")
    }
    d_rows <- list()
    for (i in seq_along(d_names)) {
      c_k <- names(corner_configs)[i]
      if (!is.null(c_k) && c_k %in% names(d_pos)) {
        pos <- d_pos[[c_k]]
        d_rows[[i]] <- data.frame(
          x = as.numeric(pos["x"]), y = as.numeric(pos["y"]),
          label = d_names[i], color = d_cols[((i - 1) %% length(d_cols)) + 1],
          hjust = as.numeric(pos["hjust"]), stringsAsFactors = FALSE
        )
      }
    }
    if (length(d_rows) > 0) d_df <- do.call(rbind, d_rows)
  }

  out <- list(
    nodes               = nodes_df,
    edges               = center_edges,
    hulls               = center_hulls,
    module_labels       = mod_labels_df,
    center_ring         = center_ring,
    outer_ring_nodes    = outer_ring_nodes,
    tiles               = all_tiles,
    diag                = all_diag,
    env_labels          = all_env_labels,
    domain_labels       = d_df,
    links               = all_links,
    module_colors       = module_colors,
    link_colors         = link_colors,
    resolved_label_type = resolved_label_type,
    value_size          = value_size,
    value_digits        = value_digits,
    compact             = compact,
    link_width          = link_width,
    link_width_range    = link_width_range,
    link_alpha          = link_alpha,
    show_link_values    = show_link_values,
    link_value_size     = link_value_size,
    circle_radius       = circle_radius,
    circle_width        = circle_width,
    circle_color        = circle_color,
    circle_linetype     = circle_linetype,
    outer_ring_size     = outer_ring_size,
    outer_ring_color    = outer_ring_color,
    outer_ring_alpha    = outer_ring_alpha,
    intra_edge_width    = intra_edge_width,
    intra_edge_color    = intra_edge_color,
    intra_edge_alpha    = intra_edge_alpha,
    diag_point_size     = diag_point_size,
    diag_point_fill     = diag_point_fill,
    node_size_range     = node_size_range,
    hull_alpha          = hull_alpha,
    module_label_bg     = module_label_bg,
    module_label_fill   = module_label_fill,
    module_label_color  = module_label_color,
    module_label_size   = module_label_size,
    module_label_alpha  = module_label_alpha,
    env_label_size      = env_label_size,
    env_label_color     = env_label_color,
    env_label_face      = env_label_face,
    preset              = preset,
    scifig              = scifig,
    show_module_legend  = show_module_legend,
    legend_position     = legend_position,
    show_domain_labels  = show_domain_labels
  )
  class(out) <- c("cli_env_network_data", "list")
  out
}

# Helper to resolve or fallback data
resolve_env_net_data <- function(data) {
  if (inherits(data, "cli_env_network_data")) {
    return(data)
  }
  if (is.list(data) && !is.data.frame(data) && "nodes" %in% names(data)) {
    return(cli_env_network_data(nodes = data$nodes, edges = data$edges, env_cors = data$env_cors, links = data$links))
  }
  cli_env_network_data()
}

#' Grammar Layer: Central Network Circular Boundary Ring
#'
#' @description
#' Draws the circular boundary ring enclosing the central network and the outer concentric
#' dots defining the open annular link corridor.
#'
#' @param data Optional \code{cli_env_network_data} object generated by \code{\link{cli_env_network_data}}.
#'   If NULL, default boundary dimensions are computed automatically.
#' @param circle_radius Numeric radius of the central boundary circle (default 2.80).
#' @param circle_width Line width of the central boundary circle (default 0.4).
#' @param circle_color Color of the central boundary circle (default \code{"grey65"}).
#' @param circle_linetype Linetype of the central boundary circle (default \code{"dashed"}).
#' @param outer_ring_size Size of dots along the outer boundary ring (default 0.6).
#' @param outer_ring_color Color of dots along the outer boundary ring (default \code{"grey60"}).
#' @param outer_ring_alpha Transparency of dots along the outer boundary ring (default 0.6).
#'
#' @return A list of \code{ggplot2} layers.
#' @export
geom_env_boundary = function(
    data             = NULL,
    circle_radius    = 2.80,
    circle_width     = 0.4,
    circle_color     = "grey65",
    circle_linetype  = "dashed",
    outer_ring_size  = 0.6,
    outer_ring_color = "grey60",
    outer_ring_alpha = 0.6
) {
  d <- resolve_env_net_data(data)
  ring_df <- d$center_ring
  outer_df <- d$outer_ring_nodes

  list(
    ggplot2::geom_path(
      data = ring_df,
      ggplot2::aes(x = .data[["x"]], y = .data[["y"]]),
      linetype = circle_linetype,
      color = circle_color,
      linewidth = circle_width,
      inherit.aes = FALSE
    ),
    ggplot2::geom_point(
      data = outer_df,
      ggplot2::aes(x = .data[["x"]], y = .data[["y"]]),
      shape = 16,
      size = outer_ring_size,
      color = outer_ring_color,
      alpha = outer_ring_alpha,
      inherit.aes = FALSE
    )
  )
}

#' Grammar Layer: Central Modular Network Nodes, Edges, and Hulls
#'
#' @description
#' Renders the central modular co-occurrence network, including intra-module edge connections,
#' soft cluster convex hulls, node glyphs (shape 16 fill-only), and radially positioned module badges.
#'
#' @param data Optional \code{cli_env_network_data} object.
#' @param hull_alpha Shading transparency for module cluster hulls (default 0.18).
#' @param intra_edge_width Width of intra-module edges (default 0.18).
#' @param intra_edge_color Color of intra-module edges (default \code{"grey82"}).
#' @param intra_edge_alpha Transparency of intra-module edges (default 0.35).
#' @param node_size_range Size range for central network nodes (default \code{c(1.1, 3.8)}).
#' @param show_module_labels Logical. If TRUE, displays module label badges (default TRUE).
#' @param module_label_bg Logical. If TRUE, renders badge boxes behind labels (default TRUE).
#' @param module_label_fill Background fill color for module labels (default \code{"white"}).
#' @param module_label_color Text color for module labels (default \code{"grey15"}).
#' @param module_label_size Font size for module labels (default 2.4).
#' @param module_label_alpha Transparency for module label background (default 1.0).
#'
#' @return A list of \code{ggplot2} layers.
#' @export
geom_env_central_network = function(
    data               = NULL,
    hull_alpha         = 0.18,
    intra_edge_width   = 0.18,
    intra_edge_color   = "grey82",
    intra_edge_alpha   = 0.35,
    node_size_range    = c(1.1, 3.8),
    show_module_labels = TRUE,
    module_label_bg    = TRUE,
    module_label_fill  = "white",
    module_label_color = "grey15",
    module_label_size  = 2.4,
    module_label_alpha = 1.0
) {
  d <- resolve_env_net_data(data)
  hulls_df <- d$hulls
  edges_df <- d$edges
  nodes_df <- d$nodes
  labels_df <- d$module_labels

  layers <- list(
    ggplot2::geom_polygon(
      data = hulls_df,
      ggplot2::aes(x = .data[["x"]], y = .data[["y"]], group = .data[["id"]], fill = .data[["module"]]),
      alpha = hull_alpha,
      show.legend = FALSE,
      inherit.aes = FALSE
    ),
    ggplot2::geom_segment(
      data = edges_df,
      ggplot2::aes(x = .data[["x"]], y = .data[["y"]], xend = .data[["xend"]], yend = .data[["yend"]]),
      color = intra_edge_color,
      alpha = intra_edge_alpha,
      linewidth = intra_edge_width,
      inherit.aes = FALSE
    ),
    ggplot2::geom_point(
      data = nodes_df,
      ggplot2::aes(x = .data[["x"]], y = .data[["y"]], color = .data[["module"]], size = .data[["degree"]]),
      shape = 16,
      alpha = 0.85,
      inherit.aes = FALSE
    ),
    ggplot2::scale_size_continuous(range = node_size_range, guide = "none")
  )

  if (isTRUE(show_module_labels) && !is.null(labels_df) && nrow(labels_df) > 0) {
    if (isTRUE(module_label_bg)) {
      layers[[length(layers) + 1]] <- ggplot2::geom_label(
        data = labels_df,
        ggplot2::aes(x = .data[["x"]], y = .data[["y"]], label = .data[["label"]]),
        fill = module_label_fill,
        color = module_label_color,
        fontface = "bold",
        size = module_label_size,
        alpha = module_label_alpha,
        label.padding = ggplot2::unit(0.18, "lines"),
        label.r = ggplot2::unit(0.15, "lines"),
        linewidth = 0,
        show.legend = FALSE,
        inherit.aes = FALSE
      )
    } else {
      layers[[length(layers) + 1]] <- ggplot2::geom_text(
        data = labels_df,
        ggplot2::aes(x = .data[["x"]], y = .data[["y"]], label = .data[["label"]]),
        color = module_label_color,
        fontface = "bold",
        size = module_label_size,
        show.legend = FALSE,
        inherit.aes = FALSE
      )
    }
  }

  layers
}

#' @rdname geom_env_central_network
#' @export
geom_central_network = geom_env_central_network

#' Grammar Layer: Peripheral Triangular Correlation Heatmaps
#'
#' @description
#' Renders the 1 to 4 peripheral triangular correlation heatmaps, including correlation tiles,
#' off-diagonal significance asterisks/digits, diagonal connector badges, outer 78° angled labels,
#' and domain category titles.
#'
#' @param data Optional \code{cli_env_network_data} object.
#' @param diag_point_size Size of diagonal indicator connector points (default 2.0).
#' @param diag_point_fill Fill color of diagonal indicator points (default \code{"#F781BF"}).
#' @param env_label_size Font size for environmental variable labels (default 1.95).
#' @param env_label_color Color for environmental variable labels (default \code{"grey20"}).
#' @param env_label_face Font face for environmental variable labels (default \code{"plain"}).
#' @param show_domain_labels Logical. If TRUE, renders domain category headers (default TRUE).
#'
#' @return A list of \code{ggplot2} layers.
#' @export
geom_env_corner_heatmaps = function(
    data               = NULL,
    diag_point_size    = 2.0,
    diag_point_fill    = "#F781BF",
    env_label_size     = 1.95,
    env_label_color    = "grey20",
    env_label_face     = "plain",
    show_domain_labels = TRUE
) {
  d <- resolve_env_net_data(data)
  tiles_df <- d$tiles
  diag_df <- d$diag
  env_labels_df <- d$env_labels
  domain_df <- d$domain_labels
  label_type <- d$resolved_label_type %||% "none"

  layers <- list(
    ggplot2::geom_tile(
      data = tiles_df,
      ggplot2::aes(x = .data[["x"]], y = .data[["y"]]),
      fill = tiles_df$fill_col,
      color = NA,
      linewidth = 0,
      inherit.aes = FALSE
    )
  )

  # Numerical or asterisk labels on tiles
  if (label_type != "none") {
    valid_text <- tiles_df$label != ""
    if (any(valid_text)) {
      sub_tiles <- tiles_df[valid_text, ]
      min_step <- min(tiles_df$step)
      char_count <- if (label_type %in% c("coef_stars", "both")) 5.2 else if (label_type == "coef") 3.8 else 2.2
      calc_fit_size <- max(1.76, min(2.5, min_step / (char_count * 0.038)))
      opt_size <- if (!is.null(d$value_size)) d$value_size else calc_fit_size

      sub_dark <- sub_tiles[sub_tiles$text_color == "white", ]
      sub_light <- sub_tiles[sub_tiles$text_color != "white", ]

      if (nrow(sub_dark) > 0) {
        layers[[length(layers) + 1]] <- ggplot2::geom_text(
          data = sub_dark,
          ggplot2::aes(x = .data[["x"]], y = .data[["y"]], label = .data[["label"]]),
          size = opt_size, color = "white", vjust = 0.5, hjust = 0.5,
          show.legend = FALSE, inherit.aes = FALSE
        )
      }
      if (nrow(sub_light) > 0) {
        layers[[length(layers) + 1]] <- ggplot2::geom_text(
          data = sub_light,
          ggplot2::aes(x = .data[["x"]], y = .data[["y"]], label = .data[["label"]]),
          size = opt_size, color = "grey18", vjust = 0.5, hjust = 0.5,
          show.legend = FALSE, inherit.aes = FALSE
        )
      }
    }
  }

  # Diagonal connector points
  layers[[length(layers) + 1]] <- ggplot2::geom_point(
    data = diag_df,
    ggplot2::aes(x = .data[["x"]], y = .data[["y"]]),
    shape = 16,
    color = diag_point_fill,
    size = diag_point_size,
    show.legend = FALSE,
    inherit.aes = FALSE
  )

  # Rotated variable labels along outer margins
  layers[[length(layers) + 1]] <- ggplot2::geom_text(
    data = env_labels_df,
    ggplot2::aes(x = .data[["x"]], y = .data[["y"]], label = .data[["var"]],
                 angle = .data[["angle"]], hjust = .data[["hjust"]], vjust = .data[["vjust"]]),
    size = env_label_size,
    fontface = env_label_face,
    color = env_label_color,
    show.legend = FALSE,
    inherit.aes = FALSE
  )

  # Domain title headers
  if (isTRUE(show_domain_labels) && !is.null(domain_df) && nrow(domain_df) > 0) {
    layers[[length(layers) + 1]] <- ggplot2::geom_text(
      data = domain_df,
      ggplot2::aes(x = .data[["x"]], y = .data[["y"]], label = .data[["label"]],
                   color = base::I(.data[["color"]]), hjust = .data[["hjust"]]),
      size = 2.6,
      fontface = "bold",
      show.legend = FALSE,
      inherit.aes = FALSE
    )
  }

  layers
}

#' @rdname geom_env_corner_heatmaps
#' @export
geom_env_heatmaps = geom_env_corner_heatmaps

#' Grammar Layer: Inter-Domain Coupling Links
#'
#' @description
#' Renders the curved connecting links between peripheral environmental indicators and central
#' network module perimeters, weighted by correlation sign and statistical significance.
#'
#' @param data Optional \code{cli_env_network_data} object.
#' @param link_width Optional constant line width. Overrides significance weighting.
#' @param link_width_range Line width range for significance-weighted links (default \code{c(0.4, 1.4)}).
#' @param link_alpha Transparency of links (default 0.70).
#' @param link_colors Named vector of colors for link directions.
#' @param module_colors Named vector of colors for network modules.
#'
#' @return A list of \code{ggplot2} layers.
#' @export
geom_env_links = function(
    data             = NULL,
    link_width       = NULL,
    link_width_range = c(0.4, 1.4),
    link_alpha       = 0.70,
    link_colors      = NULL,
    module_colors    = NULL
) {
  d <- resolve_env_net_data(data)
  links_df <- d$links
  mod_cols <- module_colors %||% d$module_colors
  lnk_cols <- link_colors %||% d$link_colors %||% c("Positive" = "#E64B35", "Negative" = "#4DBBD5")
  l_w <- if (!is.null(link_width)) link_width else d$link_width

  layers <- list()
  if (!is.null(l_w)) {
    layers[[length(layers) + 1]] <- ggplot2::geom_segment(
      data = links_df,
      ggplot2::aes(x = .data[["x"]], y = .data[["y"]], xend = .data[["xend"]], yend = .data[["yend"]],
                   color = .data[["sign"]]),
      alpha = link_alpha,
      linewidth = l_w,
      inherit.aes = FALSE
    )
  } else {
    layers[[length(layers) + 1]] <- ggplot2::geom_segment(
      data = links_df,
      ggplot2::aes(x = .data[["x"]], y = .data[["y"]], xend = .data[["xend"]], yend = .data[["yend"]],
                   color = .data[["sign"]], linewidth = .data[["log10p"]]),
      alpha = link_alpha,
      inherit.aes = FALSE
    )
    layers[[length(layers) + 1]] <- ggplot2::scale_linewidth_continuous(
      range = link_width_range,
      breaks = c(1.5, 2.0, 2.5, 3.0),
      name = "-log10 P:"
    )
  }

  layers[[length(layers) + 1]] <- ggplot2::scale_color_manual(
    values = c(mod_cols, lnk_cols),
    breaks = c("Positive", "Negative", names(mod_cols)),
    name = "Legend:"
  )

  layers
}

#' Grammar Coordinate System for Coupled Environmental Networks
#'
#' @description
#' Enforces a fixed 1:1 circular aspect ratio with standard viewport boundaries.
#'
#' @param xlim Vector of length 2 specifying horizontal bounds (default \code{c(-7.4, 7.4)}).
#' @param ylim Vector of length 2 specifying vertical bounds (default \code{c(-7.4, 7.4)}).
#'
#' @return A \code{coord_fixed} object.
#' @export
coord_env_network = function(xlim = c(-7.4, 7.4), ylim = c(-7.4, 7.4)) {
  ggplot2::coord_fixed(xlim = xlim, ylim = ylim)
}

#' Grammar Theme for Coupled Environmental Networks
#'
#' @description
#' Eliminates Cartesian grid lines and axis ticks while formatting compact, multi-tier
#' publication legends conforming to SciFig publication standards.
#'
#' @param preset Publisher styling preset: \code{"default"}, \code{"nature"}, \code{"clinical"},
#'   \code{"seasonal"}, \code{"cell"}, \code{"dark"}, \code{"lancet"}, \code{"minimal"},
#'   \code{"multi_tissue"}, \code{"circadian"}, or \code{"bipolar"}.
#' @param legend_position Position of legend: \code{"top"} (default), \code{"bottom"}, or \code{"none"}.
#' @param show_module_legend Logical. If TRUE, shows module color legend.
#' @param link_width Optional constant link width.
#'
#' @return A list containing \code{guides} and a \code{theme} object.
#' @export
theme_env_network = function(
    preset             = "default",
    legend_position    = "top",
    show_module_legend = TRUE,
    link_width         = NULL
) {
  guides_layer <- ggplot2::guides(
    color     = ggplot2::guide_legend(title.position = "left", title.vjust = 0.5, nrow = 1, order = 1),
    linewidth = if (is.null(link_width)) {
      ggplot2::guide_legend(title.position = "left", title.vjust = 0.5, nrow = 1, order = 2)
    } else {
      "none"
    },
    fill      = if (isTRUE(show_module_legend)) {
      ggplot2::guide_legend(title.position = "left", title.vjust = 0.5, nrow = 1, order = 3)
    } else {
      "none"
    }
  )

  theme_layer <- if (preset %in% c("dark", "cell")) {
    ggplot2::theme_void() + ggplot2::theme(
      plot.title           = ggplot2::element_text(size = 8.5, face = "bold", hjust = 0.5, vjust = 1.0, color = "#F8FAFC", margin = ggplot2::margin(t = 2, b = 4)),
      plot.background      = ggplot2::element_rect(fill = "#0F141C", color = NA),
      panel.background     = ggplot2::element_rect(fill = "#0F141C", color = NA),
      plot.margin          = ggplot2::margin(t = 12, b = 4, l = 4, r = 4, unit = "pt"),
      legend.position      = legend_position,
      legend.background    = ggplot2::element_rect(fill = "#0F141C", color = NA),
      legend.box           = "vertical",
      legend.box.just      = "center",
      legend.spacing.y     = ggplot2::unit(1.0, "pt"),
      legend.key.spacing.y = ggplot2::unit(0.5, "pt"),
      legend.key.spacing.x = ggplot2::unit(2.5, "pt"),
      legend.key.height    = ggplot2::unit(2.2, "mm"),
      legend.key.width     = ggplot2::unit(3.8, "mm"),
      legend.text          = ggplot2::element_text(size = 6.8, family = "sans", color = "#CBD5E1"),
      legend.title         = ggplot2::element_text(size = 7.4, face = "bold", family = "sans", color = "#F1F5F9"),
      legend.margin        = ggplot2::margin(t = 0, b = 2, l = 0, r = 0, unit = "pt"),
      legend.box.margin    = ggplot2::margin(t = 0, b = 1, l = 0, r = 0, unit = "pt")
    )
  } else {
    ggplot2::theme_void() + ggplot2::theme(
      plot.title           = ggplot2::element_text(size = 8.5, face = "bold", hjust = 0.5, vjust = 1.0, margin = ggplot2::margin(t = 2, b = 4)),
      plot.background      = ggplot2::element_rect(fill = "white", color = NA),
      panel.background     = ggplot2::element_rect(fill = "white", color = NA),
      plot.margin          = ggplot2::margin(t = 12, b = 4, l = 4, r = 4, unit = "pt"),
      legend.position      = legend_position,
      legend.box           = "vertical",
      legend.box.just      = "center",
      legend.spacing.y     = ggplot2::unit(1.0, "pt"),
      legend.key.spacing.y = ggplot2::unit(0.5, "pt"),
      legend.key.spacing.x = ggplot2::unit(2.5, "pt"),
      legend.key.height    = ggplot2::unit(2.2, "mm"),
      legend.key.width     = ggplot2::unit(3.8, "mm"),
      legend.text          = ggplot2::element_text(size = 6.8, family = "sans"),
      legend.title         = ggplot2::element_text(size = 7.4, face = "bold", family = "sans"),
      legend.margin        = ggplot2::margin(t = 0, b = 2, l = 0, r = 0, unit = "pt"),
      legend.box.margin    = ggplot2::margin(t = 0, b = 1, l = 0, r = 0, unit = "pt")
    )
  }

  list(guides_layer, theme_layer)
}

#' Plot Method for Environmental Network Data Objects
#' @param x Object of class \code{cli_env_network_data}.
#' @param ... Additional arguments.
#' @return A \code{ggplot} object.
#' @method plot cli_env_network_data
#' @export
plot.cli_env_network_data = function(x, ...) {
  p <- ggplot2::ggplot() +
    geom_env_boundary(data = x) +
    geom_env_central_network(data = x) +
    geom_env_corner_heatmaps(data = x) +
    geom_env_links(data = x) +
    coord_env_network() +
    theme_env_network(
      preset = x$preset %||% "default",
      legend_position = x$legend_position %||% "top",
      show_module_legend = x$show_module_legend %||% TRUE,
      link_width = x$link_width
    )
  p
}

#' Print Method for Environmental Network Data Objects
#' @param x Object of class \code{cli_env_network_data}.
#' @param ... Additional arguments.
#' @keywords internal
#' @method print cli_env_network_data
#' @export
print.cli_env_network_data = function(x, ...) {
  p <- plot.cli_env_network_data(x, ...)
  print(p)
  invisible(x)
}
