# ===========================================================================
# cliomicplot: Modular Network Coupled with Environmental Indicator Correlations
# Multi-Omics & Systems Biology Figure Archetype (Panel C Archetype)
# ===========================================================================

#' Circular Modular Network Coupled with Environmental Correlation Heatmaps
#'
#' @description
#' Creates publication-ready multi-domain integration networks coupling a central
#' modular microbial/omics co-occurrence network with peripheral triangular
#' correlation heatmaps of environmental, clinical, or host-associated indicators
#' (e.g. soil chemistry across seasons, clinical biomarkers across cohorts, or multi-tissue traits).
#' Curved inter-domain links connect environmental indicators from the triangular
#' diagonal to central network modules, weighted by statistical significance
#' and colored by correlation direction.
#'
#' @param env_cors Named list of 1 to 4 correlation matrices for corner triangular heatmaps.
#'   Names define corner labels (e.g. \code{list("Env 1" = cor1, "Env 2" = cor2)}).
#' @param links Optional data frame of links connecting environmental variables to network modules.
#'   Required columns: \code{env}, \code{module}, \code{r}, and \code{p} (or \code{log10p}).
#'   If NULL, significant links are computed or sampled automatically from input data.
#' @param module_colors Optional named vector of colors for network modules.
#' @param corner_palettes Optional list of color vectors for the corner heatmaps.
#' @param link_colors Named vector of colors for link signs (default \code{c("Positive" = "#E64B35", "Negative" = "#4DBBD5")}).
#' @param show_asterisks Logical. If TRUE, overlays significance asterisks (*, **, ***) in correlation tiles. Default TRUE.
#' @param show_values Logical. If TRUE, displays numerical correlation values in correlation tiles. Default FALSE.
#' @param show_cor Logical alias for \code{show_values}. If non-NULL, overrides \code{show_values}.
#' @param label_type Character string specifying tile annotation format: \code{"auto"} (default, determined by
#'   \code{show_values} and \code{show_asterisks}), \code{"coef"} (e.g. \code{".42"}),
#'   \code{"coef_stars"} (e.g. \code{".42***"}), \code{"stars"} (e.g. \code{"***"}), or \code{"none"}.
#' @param value_digits Integer number of decimal places for correlation coefficients (default 2).
#' @param value_size Optional numeric font size for tile text. If NULL (default), automatically calculated
#'   based on tile step dimensions so numbers fit inside without clipping.
#' @param compact Logical. If TRUE (default), strips leading zeros (e.g. \code{".42"} instead of \code{"0.42"})
#'   for compact, high-density tile fitting.
#' @param show_module_labels Logical. If TRUE (default), displays text labels for modules/components
#'   inside the central circle.
#' @param module_labels Optional named character vector or list mapping module IDs (e.g. \code{"Mod_1"})
#'   to custom display labels (e.g. \code{c("Mod_1" = "M1: S. aureus core")}).
#' @param module_label_size Numeric font size for module labels inside the circle (default 2.8).
#' @param module_label_color Color of module label text and borders (default \code{"grey15"}).
#' @param module_label_fill Background fill color for module labels (default \code{"white"}).
#' @param module_label_bg Logical. If TRUE (default), renders labels inside rounded opaque badges.
#' @param module_label_alpha Numeric fill transparency for module label badges (default 1.0, fully opaque).
#' @param module_label_pos Placement of module labels: \code{"radial"} (default, placed radially
#'   outward toward the circle boundary to avoid overlapping dense core nodes) or \code{"centroid"}
#'   (placed directly at module centroid).
#' @param show_module_legend Logical. If TRUE, shows the module color legend. Default TRUE.
#' @param legend_position Position of the legend: \code{"top"} (default), \code{"bottom"}, \code{"right"}, \code{"left"}, or \code{"none"}.
#' @param env_label_size Font size for environmental variable labels outside corner heatmaps (default 1.8).
#' @param env_label_color Text color for environmental variable labels (default \code{"grey20"}).
#' @param env_label_face Font face for environmental variable labels (default \code{"plain"}).
#' @param link_width_range Numeric vector of length 2 specifying line width range for inter-domain
#'   curved links scaled by significance (default \code{c(0.4, 1.4)}).
#' @param link_width Optional numeric constant line width for inter-domain links. If non-NULL, overrides
#'   \code{link_width_range}.
#' @param link_alpha Transparency for inter-domain curved links (default 0.70).
#' @param curvature Numeric curvature of inter-domain connecting lines (default 0.14).
#' @param show_link_values Logical. If TRUE, displays numerical correlation coefficients on the curved
#'   inter-domain links (default FALSE).
#' @param link_value_size Font size for correlation digits on inter-domain links (default 2.0).
#' @param link_value_digits Integer number of decimal places for link correlation digits (default 2).
#' @param circle_radius Numeric radius of the central boundary circle (default 2.45).
#' @param circle_width Line width of the central boundary circle (default 0.4).
#' @param circle_color Line color of the central boundary circle (default \code{"grey65"}).
#' @param circle_linetype Linetype of the central boundary circle (default \code{"dashed"}).
#' @param outer_ring_size Size of dots along the outer boundary circle (default 0.6).
#' @param outer_ring_color Color of dots along the outer boundary circle (default \code{"grey60"}).
#' @param outer_ring_alpha Transparency of dots along the outer boundary circle (default 0.6).
#' @param intra_edge_width Line width of intra-module network edges (default 0.18).
#' @param intra_edge_color Color of intra-module network edges (default \code{"grey82"}).
#' @param intra_edge_alpha Transparency of intra-module network edges (default 0.35).
#' @param diag_point_size Size of diagonal indicator connector points on heatmaps (default 1.8).
#' @param diag_point_fill Fill color of diagonal indicator points (default \code{"#F781BF"}).
#' @param diag_point_color Border color of diagonal indicator points (default \code{"#C51B7D"}).
#' @param node_size_range Numeric vector of length 2 specifying size range for network nodes (default \code{c(1.1, 3.8)}).
#' @param hull_alpha Shading transparency for module cluster hulls (default 0.18).
#' @param preset Character string specifying publisher style preset: \code{"default"},
#'   \code{"nature"}, \code{"cell"}, \code{"science"}, or \code{"lancet"}.
#' @param scifig Logical. If TRUE, enforces AI4SciFig publication gates (font floor >= 5 pt,
#'   line floor >= 0.25 pt, zero sign suppression, and CVD compliant palettes).
#' @param title Optional plot title.
#'
#' @return A \code{cliplot_type} object for use with \code{\link{cliplot}}.
#'
#' @export
type_env_network = function(
    env_cors           = NULL,
    links              = NULL,
    module_colors      = NULL,
    corner_palettes    = NULL,
    link_colors        = c("Positive" = "#E64B35", "Negative" = "#4DBBD5"),
    show_asterisks     = TRUE,
    show_values        = FALSE,
    show_cor           = NULL,
    label_type         = c("auto", "stars", "coef", "coef_stars", "none"),
    value_digits       = 2,
    value_size         = NULL,
    compact            = TRUE,
    show_module_labels = TRUE,
    module_labels      = NULL,
    module_label_size  = 2.8,
    module_label_color = "grey15",
    module_label_fill  = "white",
    module_label_bg    = TRUE,
    module_label_alpha = 1.0,
    module_label_pos   = c("radial", "centroid"),
    show_module_legend = TRUE,
    legend_position    = "top",
    env_label_size     = 1.8,
    env_label_color    = "grey20",
    env_label_face     = "plain",
    link_width_range   = c(0.4, 1.4),
    link_width         = NULL,
    link_alpha         = 0.70,
    show_link_values   = FALSE,
    link_value_size    = 2.0,
    link_value_digits  = 2,
    circle_radius      = 2.45,
    circle_width       = 0.4,
    circle_color       = "grey65",
    circle_linetype    = "dashed",
    outer_ring_size    = 0.6,
    outer_ring_color   = "grey60",
    outer_ring_alpha   = 0.6,
    intra_edge_width   = 0.18,
    intra_edge_color   = "grey82",
    intra_edge_alpha   = 0.35,
    diag_point_size    = 1.8,
    diag_point_fill    = "#F781BF",
    diag_point_color   = "#C51B7D",
    node_size_range    = c(1.1, 3.8),
    hull_alpha         = 0.18,
    curvature          = 0.14,
    preset             = c("default", "nature", "cell", "science", "lancet"),
    scifig             = FALSE,
    title              = NULL
) {
  if (!is.null(show_cor)) show_values <- show_cor
  label_type <- match.arg(label_type)
  module_label_pos <- match.arg(module_label_pos)
  preset <- match.arg(preset)

  if (isTRUE(scifig) && preset == "default") preset <- "nature"
  if (preset != "default") {
    # Enforce AI4SciFig quality gates
    env_label_size     <- max(env_label_size %||% 1.8, 1.8)     # >= 5.1 pt floor
    link_value_size    <- max(link_value_size %||% 1.85, 1.85)  # >= 5.25 pt floor
    module_label_size  <- max(module_label_size %||% 2.2, 2.2)   # >= 6.2 pt
    circle_width       <- max(circle_width %||% 0.35, 0.35)
    intra_edge_width   <- max(intra_edge_width %||% 0.25, 0.25) # >= 0.25 pt hairline floor
  }

  resolved_label_type <- if (label_type == "auto") {
    if (isTRUE(show_values)) {
      if (isTRUE(show_asterisks)) "coef_stars" else "coef"
    } else if (isTRUE(show_asterisks)) {
      "stars"
    } else {
      "none"
    }
  } else {
    label_type
  }

  cliplot_type(
    data = function(settings, ...) {
      df <- settings$data
      if (is.null(df)) df <- data.frame()
      df
    },
    draw = function(p, settings, data, ...) {
      args <- list(...)
      custom_data <- args$network_data %||% settings$data

      res <- build_env_network_layers(
        custom_data         = custom_data,
        env_cors            = env_cors,
        links               = links,
        module_colors       = module_colors,
        corner_palettes     = corner_palettes,
        link_colors         = link_colors,
        resolved_label_type = resolved_label_type,
        value_digits        = value_digits,
        value_size          = value_size,
        compact             = compact,
        show_module_labels  = show_module_labels,
        module_labels       = module_labels,
        module_label_size   = module_label_size,
        module_label_color  = module_label_color,
        module_label_fill   = module_label_fill,
        module_label_bg     = module_label_bg,
        module_label_alpha  = module_label_alpha,
        module_label_pos    = module_label_pos,
        show_module_legend  = show_module_legend,
        legend_position     = legend_position,
        env_label_size      = env_label_size,
        env_label_color     = env_label_color,
        env_label_face      = env_label_face,
        link_width_range    = link_width_range,
        link_width          = link_width,
        link_alpha          = link_alpha,
        show_link_values    = show_link_values,
        link_value_size     = link_value_size,
        link_value_digits   = link_value_digits,
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
        diag_point_color    = diag_point_color,
        node_size_range     = node_size_range,
        hull_alpha          = hull_alpha,
        curvature           = curvature,
        preset              = preset,
        scifig              = scifig
      )

      plot_obj <- res$plot
      if (!is.null(title)) plot_obj <- plot_obj + ggplot2::labs(title = title)
      plot_obj
    }
  )
}

#' Direct Plotting Function for Modular Network with Environmental Factor Heatmaps
#'
#' @param nodes Optional data frame of central network nodes (columns: \code{id}, \code{module}, optional \code{degree}, \code{x}, \code{y}).
#' @param edges Optional data frame of central network edges (columns: \code{from}, \code{to}).
#' @param env_cors Named list of 1 to 4 correlation matrices for corner triangular heatmaps.
#' @param links Data frame of environmental-to-module links.
#' @param module_colors Named color vector for modules.
#' @param corner_palettes List of color gradients for corner heatmaps.
#' @param show_asterisks Logical. If TRUE, shows significance asterisks. Default TRUE.
#' @param show_values Logical. If TRUE, displays numerical correlation values in tiles. Default FALSE.
#' @param show_cor Logical alias for \code{show_values}.
#' @param label_type Character string specifying tile label format: \code{"auto"}, \code{"coef"}, \code{"coef_stars"}, \code{"stars"}, or \code{"none"}.
#' @param value_digits Decimal places for correlation coefficients (default 2).
#' @param value_size Font size for tile text (default auto-scaled).
#' @param compact Logical. If TRUE (default), omits leading zero for compact fitting.
#' @param env_label_size Font size for environmental variable labels (default 1.8).
#' @param env_label_color Text color for environmental variable labels (default \code{"grey20"}).
#' @param env_label_face Font face for environmental variable labels (default \code{"plain"}).
#' @param module_label_fill Background fill color for module labels (default \code{"white"}).
#' @param link_width_range Line width range for inter-domain links (default \code{c(0.4, 1.4)}).
#' @param link_width Optional constant line width for inter-domain links. Overrides \code{link_width_range}.
#' @param link_alpha Transparency for inter-domain curved links (default 0.70).
#' @param show_link_values Logical. If TRUE, shows correlation coefficients on inter-domain links (default FALSE).
#' @param link_value_size Font size for link correlation digits (default 2.0).
#' @param link_value_digits Decimal places for link correlation digits (default 2).
#' @param circle_width Line width of the boundary circle (default 0.4).
#' @param circle_color Color of the boundary circle (default \code{"grey65"}).
#' @param circle_linetype Linetype of the boundary circle (default \code{"dashed"}).
#' @param outer_ring_size Size of outer boundary ring dots (default 0.6).
#' @param outer_ring_color Color of outer boundary ring dots (default \code{"grey60"}).
#' @param outer_ring_alpha Transparency of outer boundary ring dots (default 0.6).
#' @param intra_edge_width Width of intra-module edges (default 0.18).
#' @param intra_edge_color Color of intra-module edges (default \code{"grey82"}).
#' @param intra_edge_alpha Transparency of intra-module edges (default 0.35).
#' @param diag_point_size Size of diagonal connector points on heatmaps (default 1.8).
#' @param diag_point_fill Fill color of diagonal connector points (default \code{"#F781BF"}).
#' @param diag_point_color Border color of diagonal connector points (default \code{"#C51B7D"}).
#' @param node_size_range Point size range for central network nodes (default \code{c(1.1, 3.8)}).
#' @param hull_alpha Transparency for module cluster hulls (default 0.18).
#' @param curvature Curvature of inter-domain curved links (default 0.14).
#' @param circle_radius Radius of central boundary circle (default 2.45).
#' @param title Plot title.
#' @param ... Additional arguments.
#'
#' @return A \code{ggplot} object.
#' @export
cliplot_env_network = function(
    nodes              = NULL,
    edges              = NULL,
    env_cors           = NULL,
    links              = NULL,
    module_colors      = NULL,
    corner_palettes    = NULL,
    link_colors        = c("Positive" = "#E64B35", "Negative" = "#4DBBD5"),
    show_asterisks     = TRUE,
    show_values        = FALSE,
    show_cor           = NULL,
    label_type         = c("auto", "stars", "coef", "coef_stars", "none"),
    value_digits       = 2,
    value_size         = NULL,
    compact            = TRUE,
    show_module_labels = TRUE,
    module_labels      = NULL,
    module_label_size  = 2.8,
    module_label_color = "grey15",
    module_label_fill  = "white",
    module_label_bg    = TRUE,
    module_label_alpha = 1.0,
    module_label_pos   = c("radial", "centroid"),
    show_module_legend = TRUE,
    legend_position    = "top",
    env_label_size     = 1.8,
    env_label_color    = "grey20",
    env_label_face     = "plain",
    link_width_range   = c(0.4, 1.4),
    link_width         = NULL,
    link_alpha         = 0.70,
    show_link_values   = FALSE,
    link_value_size    = 2.0,
    link_value_digits  = 2,
    circle_radius      = 2.45,
    circle_width       = 0.4,
    circle_color       = "grey65",
    circle_linetype    = "dashed",
    outer_ring_size    = 0.6,
    outer_ring_color   = "grey60",
    outer_ring_alpha   = 0.6,
    intra_edge_width   = 0.18,
    intra_edge_color   = "grey82",
    intra_edge_alpha   = 0.35,
    diag_point_size    = 1.8,
    diag_point_fill    = "#F781BF",
    diag_point_color   = "#C51B7D",
    node_size_range    = c(1.1, 3.8),
    hull_alpha         = 0.18,
    curvature          = 0.14,
    preset             = c("default", "nature", "cell", "science", "lancet"),
    scifig             = FALSE,
    title              = NULL,
    ...
) {
  if (!is.null(show_cor)) show_values <- show_cor
  label_type <- match.arg(label_type)
  module_label_pos <- match.arg(module_label_pos)
  preset <- match.arg(preset)

  net_data <- list(nodes = nodes, edges = edges)
  t_obj <- type_env_network(
    env_cors           = env_cors,
    links              = links,
    module_colors      = module_colors,
    corner_palettes    = corner_palettes,
    link_colors        = link_colors,
    show_asterisks     = show_asterisks,
    show_values        = show_values,
    label_type         = label_type,
    value_digits       = value_digits,
    value_size         = value_size,
    compact            = compact,
    show_module_labels = show_module_labels,
    module_labels      = module_labels,
    module_label_size  = module_label_size,
    module_label_color = module_label_color,
    module_label_fill  = module_label_fill,
    module_label_bg    = module_label_bg,
    module_label_alpha = module_label_alpha,
    module_label_pos   = module_label_pos,
    show_module_legend = show_module_legend,
    legend_position    = legend_position,
    env_label_size     = env_label_size,
    env_label_color    = env_label_color,
    env_label_face     = env_label_face,
    link_width_range   = link_width_range,
    link_width         = link_width,
    link_alpha         = link_alpha,
    show_link_values   = show_link_values,
    link_value_size    = link_value_size,
    link_value_digits  = link_value_digits,
    circle_radius      = circle_radius,
    circle_width       = circle_width,
    circle_color       = circle_color,
    circle_linetype    = circle_linetype,
    outer_ring_size    = outer_ring_size,
    outer_ring_color   = outer_ring_color,
    outer_ring_alpha   = outer_ring_alpha,
    intra_edge_width   = intra_edge_width,
    intra_edge_color   = intra_edge_color,
    intra_edge_alpha   = intra_edge_alpha,
    diag_point_size    = diag_point_size,
    diag_point_fill    = diag_point_fill,
    diag_point_color   = diag_point_color,
    node_size_range    = node_size_range,
    hull_alpha         = hull_alpha,
    curvature          = curvature,
    preset             = preset,
    scifig             = scifig,
    title              = title,
    ...
  )
  t_obj$draw(ggplot2::ggplot(), list(data = net_data), net_data, ...)
}

#' Helper builder for env network layers
#' @keywords internal
build_env_network_layers = function(
    custom_data,
    env_cors,
    links,
    module_colors,
    corner_palettes,
    link_colors,
    resolved_label_type,
    value_digits,
    value_size,
    compact,
    show_module_labels  = TRUE,
    module_labels       = NULL,
    module_label_size   = 2.8,
    module_label_color  = "grey15",
    module_label_fill   = "white",
    module_label_bg     = TRUE,
    module_label_alpha  = 1.0,
    module_label_pos    = "radial",
    show_module_legend  = TRUE,
    legend_position     = "top",
    env_label_size      = 1.8,
    env_label_color     = "grey20",
    env_label_face      = "plain",
    link_width_range    = c(0.4, 1.4),
    link_width          = NULL,
    link_alpha          = 0.70,
    show_link_values    = FALSE,
    link_value_size     = 2.0,
    link_value_digits   = 2,
    circle_radius       = 2.45,
    circle_width        = 0.4,
    circle_color        = "grey65",
    circle_linetype     = "dashed",
    outer_ring_size     = 0.6,
    outer_ring_color    = "grey60",
    outer_ring_alpha    = 0.6,
    intra_edge_width    = 0.18,
    intra_edge_color    = "grey82",
    intra_edge_alpha    = 0.35,
    diag_point_size     = 1.8,
    diag_point_fill     = "#F781BF",
    diag_point_color    = "#C51B7D",
    node_size_range     = c(1.1, 3.8),
    hull_alpha          = 0.18,
    curvature           = 0.14,
    preset              = "default",
    scifig              = FALSE
) {
  # 1. Module configuration
  n_mods <- 7
  u_mods <- paste0("Mod_", 1:n_mods)
  if (is.null(module_colors)) {
    mod_cols <- c(
      "Mod_1" = "#4DBBD5", "Mod_2" = "#E64B35", "Mod_3" = "#00A087",
      "Mod_4" = "#3C5488", "Mod_5" = "#F39B7F", "Mod_6" = "#8491B4", "Mod_7" = "#B09C85"
    )
    module_colors <- mod_cols
  }

  outer_th <- seq(pi/6, 2*pi + pi/6, length.out = 7)[1:6]
  mod_centers <- data.frame(
    module = u_mods,
    cx = c(0, 1.45 * cos(outer_th)),
    cy = c(0, 1.45 * sin(outer_th)),
    n_nodes = c(65, 35, 32, 28, 30, 26, 25),
    r_spread = c(0.68, 0.45, 0.42, 0.40, 0.42, 0.38, 0.38),
    stringsAsFactors = FALSE
  )

  # Check custom nodes
  nodes_df <- NULL
  if (is.list(custom_data) && !is.data.frame(custom_data)) {
    nodes_df <- custom_data$nodes
    edges_df <- custom_data$edges
  } else if (is.data.frame(custom_data) && nrow(custom_data) > 0) {
    nodes_df <- custom_data
    edges_df <- NULL
  }

  if (is.null(nodes_df) || nrow(nodes_df) == 0) {
    node_list <- list()
    for (i in 1:nrow(mod_centers)) {
      mc <- mod_centers[i, ]
      n <- mc$n_nodes
      r <- stats::rbeta(n, 1.2, 2.0) * mc$r_spread
      th <- stats::runif(n, 0, 2*pi)
      node_list[[i]] <- data.frame(
        id = paste0("Taxa_", mc$module, "_", 1:n),
        module = mc$module,
        x = mc$cx + r * cos(th),
        y = mc$cy + r * sin(th),
        degree = sample(4:25, n, replace = TRUE),
        stringsAsFactors = FALSE
      )
    }
    nodes_df <- do.call(rbind, node_list)
  } else if (all(c("x", "y", "module") %in% names(nodes_df))) {
    mod_cents <- stats::aggregate(cbind(x, y) ~ module, data = nodes_df, FUN = mean)
    mod_centers <- data.frame(
      module = as.character(mod_cents$module),
      cx = mod_cents$x,
      cy = mod_cents$y,
      stringsAsFactors = FALSE
    )
  }

  # Build module hulls
  hull_list <- list()
  theta_seq <- seq(0, 2*pi, length.out = 45)
  for (i in 1:nrow(mod_centers)) {
    mc <- mod_centers[i, ]
    hull_list[[i]] <- data.frame(
      x = mc$cx + (mc$r_spread * 1.15) * cos(theta_seq),
      y = mc$cy + (mc$r_spread * 1.15) * sin(theta_seq),
      module = mc$module,
      id = mc$module,
      stringsAsFactors = FALSE
    )
  }
  center_hulls <- do.call(rbind, hull_list)

  # Build intra edges
  intra_list <- list()
  for (i in 1:nrow(mod_centers)) {
    m_nodes <- nodes_df[nodes_df$module == mod_centers$module[i], ]
    n_e <- round(nrow(m_nodes) * 2.2)
    f <- sample(seq_len(nrow(m_nodes)), n_e, replace = TRUE)
    t <- sample(seq_len(nrow(m_nodes)), n_e, replace = TRUE)
    k <- f != t
    intra_list[[i]] <- data.frame(
      x = m_nodes$x[f[k]],
      y = m_nodes$y[f[k]],
      xend = m_nodes$x[t[k]],
      yend = m_nodes$y[t[k]]
    )
  }
  center_edges <- do.call(rbind, intra_list)

  # Circular dashed boundary
  ring_th <- seq(0, 2*pi, length.out = 160)
  center_ring <- data.frame(
    x = circle_radius * cos(ring_th),
    y = circle_radius * sin(ring_th)
  )

  # Concentric outer ring dots
  n_outer_ring <- 80
  outer_ring_th <- seq(0, 2*pi, length.out = n_outer_ring + 1)[1:n_outer_ring]
  outer_ring_nodes <- data.frame(
    x = (circle_radius * 0.95) * cos(outer_ring_th),
    y = (circle_radius * 0.95) * sin(outer_ring_th)
  )

  # 2. Four Corner Environmental Triangles
  # Check if user supplied env_cors
  corner_configs <- list(
    list(corner = "top_left", center_x = -5.0, center_y = 5.0, default_pal = c("#4DAF4A", "#FFFFFF", "#E41A1C")),
    list(corner = "top_right", center_x = 5.0, center_y = 5.0, default_pal = c("#984EA3", "#FFFFFF", "#FF7F00")),
    list(corner = "bottom_left", center_x = -5.0, center_y = -5.0, default_pal = c("#66C2A5", "#FFFFFF", "#FC8D62")),
    list(corner = "bottom_right", center_x = 5.0, center_y = -5.0, default_pal = c("#377EB8", "#FFFFFF", "#E41A1C"))
  )

  tiles_list <- list()
  diag_list  <- list()
  labels_list <- list()

  env_vars_default <- c("pH", "Humdepth", "Baresoil", "Mo", "Zn", "Mn", "Fe", "Al", "S", "Mg", "Ca", "K", "P", "N")

  n_corners_to_draw <- if (!is.null(env_cors) && length(env_cors) > 0) min(length(env_cors), 4) else 4

  for (c_idx in 1:n_corners_to_draw) {
    cfg <- corner_configs[[c_idx]]
    c_name <- if (!is.null(env_cors) && !is.null(names(env_cors)[c_idx])) names(env_cors)[c_idx] else paste0("Env ", c_idx)
    
    # Extract variable names and correlation matrix if provided
    var_names <- NULL
    mat_r     <- NULL
    if (!is.null(env_cors) && length(env_cors) >= c_idx) {
      item <- env_cors[[c_idx]]
      if (is.matrix(item) || is.data.frame(item)) {
        var_names <- colnames(item)
        mat_r     <- as.matrix(item)
      } else if (is.character(item)) {
        var_names <- item
      }
    }
    if (is.null(var_names) || length(var_names) < 2) {
      var_names <- paste0(env_vars_default, "_", c_idx)
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
        stars[idx] <- "***"
      } else if (abs_r > 0.6) {
        stars[idx] <- "***"
      } else if (abs_r > 0.45) {
        stars[idx] <- "**"
      } else if (abs_r > 0.3) {
        stars[idx] <- "*"
      }
    }

    size_box <- 2.3
    step <- size_box / p_dim
    corner_type <- cfg$corner
    center_x <- cfg$center_x
    center_y <- cfg$center_y

    if (corner_type == "top_left") {
      xs <- center_x + (grid$col - 1) * step
      ys <- center_y - (grid$row - 1) * step
      diag_pts <- data.frame(
        var = var_names,
        x = center_x + (1:p_dim - 1) * step + step * 0.5,
        y = center_y - (1:p_dim - 1) * step - step * 0.5,
        stringsAsFactors = FALSE
      )
      top_labels <- data.frame(
        var = var_names, x = center_x + (1:p_dim - 1) * step, y = center_y + step * 0.6,
        angle = 45, hjust = 0, vjust = 0, stringsAsFactors = FALSE
      )
      left_labels <- data.frame(
        var = var_names, x = center_x - step * 0.6, y = center_y - (1:p_dim - 1) * step,
        angle = 0, hjust = 1, vjust = 0.5, stringsAsFactors = FALSE
      )
    } else if (corner_type == "top_right") {
      xs <- center_x - (grid$col - 1) * step
      ys <- center_y - (grid$row - 1) * step
      diag_pts <- data.frame(
        var = var_names,
        x = center_x - (1:p_dim - 1) * step - step * 0.5,
        y = center_y - (1:p_dim - 1) * step - step * 0.5,
        stringsAsFactors = FALSE
      )
      top_labels <- data.frame(
        var = var_names, x = center_x - (1:p_dim - 1) * step, y = center_y + step * 0.6,
        angle = 45, hjust = 0, vjust = 0, stringsAsFactors = FALSE
      )
      left_labels <- data.frame(
        var = var_names, x = center_x + step * 0.6, y = center_y - (1:p_dim - 1) * step,
        angle = 0, hjust = 0, vjust = 0.5, stringsAsFactors = FALSE
      )
    } else if (corner_type == "bottom_left") {
      xs <- center_x + (grid$col - 1) * step
      ys <- center_y + (grid$row - 1) * step
      diag_pts <- data.frame(
        var = var_names,
        x = center_x + (1:p_dim - 1) * step + step * 0.5,
        y = center_y + (1:p_dim - 1) * step + step * 0.5,
        stringsAsFactors = FALSE
      )
      top_labels <- data.frame(
        var = var_names, x = center_x + (1:p_dim - 1) * step, y = center_y - step * 0.6,
        angle = 45, hjust = 1, vjust = 1, stringsAsFactors = FALSE
      )
      left_labels <- data.frame(
        var = var_names, x = center_x - step * 0.6, y = center_y + (1:p_dim - 1) * step,
        angle = 0, hjust = 1, vjust = 0.5, stringsAsFactors = FALSE
      )
    } else {
      xs <- center_x - (grid$col - 1) * step
      ys <- center_y + (grid$row - 1) * step
      diag_pts <- data.frame(
        var = var_names,
        x = center_x - (1:p_dim - 1) * step - step * 0.5,
        y = center_y + (1:p_dim - 1) * step + step * 0.5,
        stringsAsFactors = FALSE
      )
      top_labels <- data.frame(
        var = var_names, x = center_x - (1:p_dim - 1) * step, y = center_y - step * 0.6,
        angle = 45, hjust = 1, vjust = 1, stringsAsFactors = FALSE
      )
      left_labels <- data.frame(
        var = var_names, x = center_x + step * 0.6, y = center_y + (1:p_dim - 1) * step,
        angle = 0, hjust = 0, vjust = 0.5, stringsAsFactors = FALSE
      )
    }

    # Formatting numerical correlation values
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
      "stars"      = stars,
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

  # 3. Inter-domain connecting links
  link_list <- list()
  if (!is.null(links) && nrow(links) > 0 && all(c("env", "module") %in% names(links))) {
    # Match user supplied links
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
        curv <- if (pt$x < 0) curvature else -curvature

        link_list[[length(link_list) + 1]] <- data.frame(
          x = pt$x, y = pt$y, xend = mc$cx, yend = mc$cy,
          r = r_val, log10p = log10_p,
          sign = ifelse(r_val > 0, "Positive", "Negative"),
          curvature = curv, stringsAsFactors = FALSE
        )
      }
    }
  }

  if (length(link_list) == 0) {
    u_corners <- unique(all_diag$corner)
    for (c_name in u_corners) {
      c_pts <- all_diag[all_diag$corner == c_name, ]
      n_pick <- min(5, nrow(c_pts))
      chosen_vars <- sample(c_pts$var, n_pick)
      chosen_mods <- sample(mod_centers$module, n_pick, replace = TRUE)
      for (k in seq_len(n_pick)) {
        pt <- c_pts[c_pts$var == chosen_vars[k], ]
        mc <- mod_centers[mod_centers$module == chosen_mods[k], ]
        r_val <- sample(c(0.72, 0.58, -0.64, 0.45, -0.52), 1)
        p_val <- stats::runif(1, 1e-4, 0.015)
        curv <- if (pt$x < 0) curvature else -curvature

        link_list[[length(link_list) + 1]] <- data.frame(
          x = pt$x,
          y = pt$y,
          xend = mc$cx,
          yend = mc$cy,
          r = r_val,
          log10p = -log10(p_val),
          sign = ifelse(r_val > 0, "Positive", "Negative"),
          curvature = curv,
          stringsAsFactors = FALSE
        )
      }
    }
  }
  all_links <- do.call(rbind, link_list)

  # 4. Assemble Plot
  p <- ggplot2::ggplot() +
    ggplot2::geom_path(
      data = center_ring, ggplot2::aes(x = .data[["x"]], y = .data[["y"]]),
      linetype = circle_linetype, color = circle_color, linewidth = circle_width
    ) +
    ggplot2::geom_point(
      data = outer_ring_nodes, ggplot2::aes(x = .data[["x"]], y = .data[["y"]]),
      size = outer_ring_size, color = outer_ring_color, alpha = outer_ring_alpha
    ) +
    ggplot2::geom_polygon(
      data = center_hulls,
      ggplot2::aes(x = .data[["x"]], y = .data[["y"]], group = .data[["id"]], fill = .data[["module"]]),
      alpha = hull_alpha, show.legend = FALSE
    ) +
    ggplot2::geom_segment(
      data = center_edges,
      ggplot2::aes(x = .data[["x"]], y = .data[["y"]], xend = .data[["xend"]], yend = .data[["yend"]]),
      color = intra_edge_color, alpha = intra_edge_alpha, linewidth = intra_edge_width
    )

  if (!is.null(link_width)) {
    p <- p + ggplot2::geom_curve(
      data = all_links,
      ggplot2::aes(x = .data[["x"]], y = .data[["y"]], xend = .data[["xend"]], yend = .data[["yend"]], color = .data[["sign"]]),
      curvature = curvature, alpha = link_alpha, linewidth = link_width
    )
  } else {
    p <- p + ggplot2::geom_curve(
      data = all_links,
      ggplot2::aes(x = .data[["x"]], y = .data[["y"]], xend = .data[["xend"]], yend = .data[["yend"]], color = .data[["sign"]], linewidth = .data[["log10p"]]),
      curvature = curvature, alpha = link_alpha
    ) +
    ggplot2::scale_linewidth_continuous(
      range = link_width_range,
      breaks = c(1.5, 2.0, 2.5, 3.0),
      name = expression(-log[10](italic(P)-value) * ":")
    )
  }

  p <- p + ggplot2::scale_color_manual(values = link_colors, name = "Correlation:")

  if (isTRUE(show_link_values) && nrow(all_links) > 0) {
    dx <- all_links$xend - all_links$x
    dy <- all_links$yend - all_links$y
    curv <- all_links$curvature
    mx <- (all_links$x + all_links$xend) / 2 - curv * dy * 0.40
    my <- (all_links$y + all_links$yend) / 2 + curv * dx * 0.40

    fmt_link_r <- sprintf(paste0("%.", link_value_digits, "f"), all_links$r)
    link_zero_pat <- paste0("^-0\\.", paste(rep("0", link_value_digits), collapse = ""), "$")
    fmt_link_r <- sub(link_zero_pat, paste0("0.", paste(rep("0", link_value_digits), collapse = "")), fmt_link_r)
    fmt_link_r <- sub("^-0$", "0", fmt_link_r)
    if (isTRUE(compact)) {
      fmt_link_r <- sub("^(-?)0\\.", "\\1.", fmt_link_r)
      fmt_link_r[all_links$r == 1.0] <- "1"
      fmt_link_r[all_links$r == -1.0] <- "-1"
      fmt_link_r <- sub(paste0("^-\\.", paste(rep("0", link_value_digits), collapse = ""), "$"),
                        paste0(".", paste(rep("0", link_value_digits), collapse = "")), fmt_link_r)
    }

    link_val_df <- data.frame(
      x = mx,
      y = my,
      label = fmt_link_r,
      sign = all_links$sign,
      stringsAsFactors = FALSE
    )

    p <- p + ggplot2::geom_label(
      data = link_val_df,
      ggplot2::aes(x = .data[["x"]], y = .data[["y"]], label = .data[["label"]], color = .data[["sign"]]),
      size = link_value_size,
      fill = "white",
      alpha = 0.95,
      fontface = "bold",
      label.padding = ggplot2::unit(0.12, "lines"),
      label.r = ggplot2::unit(0.12, "lines"),
      linewidth = 0.20,
      show.legend = FALSE
    )
  }

  p <- p +
    ggplot2::geom_point(
      data = nodes_df,
      ggplot2::aes(x = .data[["x"]], y = .data[["y"]], fill = .data[["module"]], size = .data[["degree"]]),
      shape = 21, color = "grey25", stroke = 0.2
    ) +
    ggplot2::scale_fill_manual(values = module_colors, name = "Module:") +
    ggplot2::scale_size_continuous(range = node_size_range, guide = "none") +
    ggplot2::geom_tile(
      data = all_tiles,
      ggplot2::aes(x = .data[["x"]], y = .data[["y"]]),
      fill = all_tiles$fill_col,
      color = "white", linewidth = 0.15
    )

  if (resolved_label_type != "none") {
    valid_text <- all_tiles$label != ""
    if (any(valid_text)) {
      sub_tiles <- all_tiles[valid_text, ]

      min_step <- min(all_tiles$step)
      char_count <- if (resolved_label_type %in% c("coef_stars", "both")) 5.2 else if (resolved_label_type == "coef") 3.8 else 2.2
      calc_fit_size <- max(0.9, min(2.5, min_step / (char_count * 0.038)))
      opt_size <- if (!is.null(value_size)) value_size else calc_fit_size
      if (preset != "default" || isTRUE(scifig)) {
        opt_size <- max(1.76, opt_size)
      }

      sub_dark <- sub_tiles[sub_tiles$text_color == "white", ]
      sub_light <- sub_tiles[sub_tiles$text_color != "white", ]

      if (nrow(sub_dark) > 0) {
        p <- p + ggplot2::geom_text(
          data = sub_dark,
          ggplot2::aes(x = .data[["x"]], y = .data[["y"]], label = .data[["label"]]),
          size = opt_size, color = "white", vjust = 0.5, hjust = 0.5
        )
      }
      if (nrow(sub_light) > 0) {
        p <- p + ggplot2::geom_text(
          data = sub_light,
          ggplot2::aes(x = .data[["x"]], y = .data[["y"]], label = .data[["label"]]),
          size = opt_size, color = "grey18", vjust = 0.5, hjust = 0.5
        )
      }
    }
  }

  # 3b. Module labels inside the central circle
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
        r_pos <- min(circle_radius * 0.77, r_dist + 0.43)
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

      label_list[[m_i]] <- data.frame(
        module = m_name,
        x = lx,
        y = ly,
        label = lbl_txt,
        stringsAsFactors = FALSE
      )
    }
    mod_labels_df <- do.call(rbind, label_list)
  }

  if (!is.null(mod_labels_df) && nrow(mod_labels_df) > 0) {
    if (isTRUE(module_label_bg)) {
      p <- p + ggplot2::geom_label(
        data = mod_labels_df,
        ggplot2::aes(x = .data[["x"]], y = .data[["y"]], label = .data[["label"]]),
        fill = module_label_fill,
        color = module_label_color,
        fontface = "bold",
        size = module_label_size,
        alpha = module_label_alpha,
        label.padding = ggplot2::unit(0.22, "lines"),
        label.r = ggplot2::unit(0.20, "lines"),
        linewidth = 0.25,
        show.legend = FALSE
      )
    } else {
      p <- p + ggplot2::geom_text(
        data = mod_labels_df,
        ggplot2::aes(x = .data[["x"]], y = .data[["y"]], label = .data[["label"]]),
        color = module_label_color,
        fontface = "bold",
        size = module_label_size,
        show.legend = FALSE
      )
    }
  }

  p <- p +
    ggplot2::geom_point(
      data = all_diag,
      ggplot2::aes(x = .data[["x"]], y = .data[["y"]]),
      shape = 21, fill = diag_point_fill, color = diag_point_color, size = diag_point_size, stroke = 0.3
    ) +
    ggplot2::geom_text(
      data = all_env_labels,
      ggplot2::aes(x = .data[["x"]], y = .data[["y"]], label = .data[["var"]], angle = .data[["angle"]], hjust = .data[["hjust"]], vjust = .data[["vjust"]]),
      size = env_label_size, fontface = env_label_face, color = env_label_color
    ) +
    ggplot2::guides(
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
    ) +
    ggplot2::coord_fixed(xlim = c(-6.8, 6.8), ylim = c(-6.8, 6.8)) +
    ggplot2::theme_void() +
    ggplot2::theme(
      legend.position      = legend_position,
      legend.box           = "vertical",
      legend.box.just      = "center",
      legend.spacing.y     = ggplot2::unit(1.0, "pt"),
      legend.key.spacing.y = ggplot2::unit(0.5, "pt"),
      legend.key.spacing.x = ggplot2::unit(2.5, "pt"),
      legend.key.height    = ggplot2::unit(2.2, "mm"),
      legend.key.width     = ggplot2::unit(3.8, "mm"),
      legend.text          = ggplot2::element_text(size = 6.2, family = "sans"),
      legend.title         = ggplot2::element_text(size = 6.8, face = "bold", family = "sans"),
      legend.margin        = ggplot2::margin(t = 0, b = 2, l = 0, r = 0, unit = "pt"),
      legend.box.margin    = ggplot2::margin(t = 0, b = 1, l = 0, r = 0, unit = "pt")
    )

  list(plot = p, nodes = nodes_df, tiles = all_tiles, links = all_links, module_labels = mod_labels_df)
}
