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
#' @param module_label_size Numeric font size for module labels inside the circle (default 2.4).
#' @param module_label_color Color of module label text and borders (default \code{"grey15"}).
#' @param module_label_fill Background fill color for module labels (default \code{"white"}).
#' @param module_label_bg Logical. If TRUE (default), renders labels inside rounded opaque badges.
#' @param module_label_alpha Numeric fill transparency for module label badges (default 1.0, fully opaque).
#' @param module_label_pos Placement of module labels: \code{"radial"} (default, placed radially
#'   outward toward the circle boundary to avoid overlapping dense core nodes) or \code{"centroid"}
#'   (placed directly at module centroid).
#' @param show_module_legend Logical. If TRUE, shows the module color legend. Default TRUE.
#' @param legend_position Position of the legend: \code{"top"} (default), \code{"bottom"}, \code{"right"}, \code{"left"}, or \code{"none"}.
#' @param show_domain_labels Logical. If TRUE, displays domain headers for the corner heatmaps. Default TRUE.
#' @param env_label_size Font size for environmental variable labels outside corner heatmaps (default 1.95).
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
#' @param circle_radius Numeric radius of the central boundary circle (default 2.80).
#' @param circle_width Line width of the central boundary circle (default 0.4).
#' @param circle_color Line color of the central boundary circle (default \code{"grey65"}).
#' @param circle_linetype Linetype of the central boundary circle (default \code{"dashed"}).
#' @param outer_ring_size Size of dots along the outer boundary circle (default 0.6).
#' @param outer_ring_color Color of dots along the outer boundary circle (default \code{"grey60"}).
#' @param outer_ring_alpha Transparency of dots along the outer boundary circle (default 0.6).
#' @param intra_edge_width Line width of intra-module network edges (default 0.18).
#' @param intra_edge_color Color of intra-module network edges (default \code{"grey82"}).
#' @param intra_edge_alpha Transparency of intra-module network edges (default 0.35).
#' @param diag_point_size Size of diagonal indicator connector points on heatmaps (default 2.0).
#' @param diag_point_fill Fill color of diagonal indicator points (default \code{"#F781BF"}).
#' @param diag_point_color Border color of diagonal indicator points (default \code{"#C51B7D"}).
#' @param node_size_range Numeric vector of length 2 specifying size range for network nodes (default \code{c(1.1, 3.8)}).
#' @param hull_alpha Shading transparency for module cluster hulls (default 0.18).
#' @param preset Character string specifying publisher style preset: \code{"default"},
#'   \code{"nature"}, \code{"clinical"}, \code{"seasonal"}, \code{"cell"}, \code{"dark"},
#'   \code{"lancet"}, \code{"minimal"}, \code{"multi_tissue"} (or \code{"pharmacology"}),
#'   \code{"circadian"} (or \code{"diurnal"}), or \code{"bipolar"} (or \code{"response"}).
#' @param style Character string alias for \code{preset}. If supplied, overrides \code{preset}.
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
    comm_data          = NULL,
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
    preset             = c("default", "nature", "clinical", "seasonal", "cell", "dark", "lancet", "minimal",
                           "multi_tissue", "pharmacology", "circadian", "diurnal", "bipolar", "response"),
    style              = NULL,
    scifig             = FALSE,
    title              = NULL
) {
  if (!is.null(show_cor)) show_values <- show_cor
  if (!is.null(style)) preset <- style
  label_type <- match.arg(label_type)
  module_label_pos <- match.arg(module_label_pos)
  preset <- match.arg(preset)

  if (is.null(links) && !is.null(comm_data) && !is.null(env_cors)) {
    links <- tryCatch({
      cli_calc_env_links(comm_data = comm_data, env_data = env_cors, modules = module_labels)
    }, error = function(e) NULL)
  }

  if (isTRUE(scifig) && preset == "default") preset <- "nature"
  if (preset != "default") {
    # Enforce AI4SciFig quality gates
    env_label_size     <- max(env_label_size %||% 1.8, 1.8)     # >= 5.1 pt floor
    link_value_size    <- max(link_value_size %||% 1.85, 1.85)  # >= 5.25 pt floor
    module_label_size  <- max(module_label_size %||% 2.2, 2.2)   # >= 6.2 pt
    circle_width       <- max(circle_width %||% 0.35, 0.35)
    intra_edge_width   <- max(intra_edge_width %||% 0.25, 0.25) # >= 0.25 pt hairline floor
  }

  if (preset %in% c("dark", "cell")) {
    if (identical(circle_color, "grey65")) circle_color <- "#334155"
    if (identical(outer_ring_color, "grey60")) outer_ring_color <- "#475569"
    if (identical(intra_edge_color, "grey82")) intra_edge_color <- "#242F42"
    intra_edge_alpha <- max(intra_edge_alpha, 0.45)
    if (identical(module_label_fill, "white")) module_label_fill <- "#1E2638"
    if (identical(module_label_color, "grey15")) module_label_color <- "#F8FAFC"
    if (identical(env_label_color, "grey20")) env_label_color <- "#CBD5E1"
    if (identical(diag_point_fill, "#F781BF")) diag_point_fill <- "#00F2FE"
    if (identical(link_colors, c("Positive" = "#E64B35", "Negative" = "#4DBBD5"))) {
      link_colors <- c("Positive" = "#00F2FE", "Negative" = "#FF007F")
    }
  } else if (preset %in% c("lancet", "minimal")) {
    if (identical(circle_linetype, "dashed")) circle_linetype <- "solid"
    if (identical(circle_color, "grey65")) circle_color <- "grey80"
    if (identical(link_colors, c("Positive" = "#E64B35", "Negative" = "#4DBBD5"))) {
      link_colors <- c("Positive" = "#B2182B", "Negative" = "#2166AC")
    }
    if (label_type == "auto" && isTRUE(show_values)) label_type <- "coef"
  } else if (preset %in% c("multi_tissue", "pharmacology")) {
    if (identical(link_colors, c("Positive" = "#E64B35", "Negative" = "#4DBBD5"))) {
      link_colors <- c("Positive" = "#EE0000", "Negative" = "#3B4992")
    }
    if (identical(diag_point_fill, "#F781BF")) diag_point_fill <- "#008B45"
    if (identical(diag_point_color, "#C51B7D")) diag_point_color <- "#006400"
  } else if (preset %in% c("circadian", "diurnal")) {
    if (identical(link_colors, c("Positive" = "#E64B35", "Negative" = "#4DBBD5"))) {
      link_colors <- c("Positive" = "#D95F02", "Negative" = "#7570B3")
    }
    if (identical(diag_point_fill, "#F781BF")) diag_point_fill <- "#E6AB02"
    if (identical(diag_point_color, "#C51B7D")) diag_point_color <- "#D95F02"
  } else if (preset %in% c("bipolar", "response")) {
    if (identical(link_colors, c("Positive" = "#E64B35", "Negative" = "#4DBBD5"))) {
      link_colors <- c("Positive" = "#D62728", "Negative" = "#1F77B4")
    }
    if (identical(diag_point_fill, "#F781BF")) diag_point_fill <- "#1F77B4"
    if (identical(diag_point_color, "#C51B7D")) diag_point_color <- "#08519C"
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

      # Formula grammar resolution
      use_env_cors <- env_cors
      if (is.null(use_env_cors) && !is.null(settings$y)) {
        use_env_cors <- settings$y
      }
      use_comm_data <- comm_data
      if (is.null(use_comm_data) && !is.null(settings$x)) {
        use_comm_data <- settings$x
      }

      res <- build_env_network_layers(
        custom_data         = custom_data,
        env_cors            = use_env_cors,
        comm_data           = use_comm_data,
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
        show_domain_labels  = show_domain_labels,
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
#' @param comm_data Optional numeric matrix or data frame of community abundance data. If supplied
#'   and \code{links} is NULL, empirical linkages to \code{env_cors} are automatically calculated
#'   using \code{\link{cli_calc_env_links}}.
#' @param module_colors Named color vector for modules.
#' @param corner_palettes List of color gradients for corner heatmaps.
#' @param show_asterisks Logical. If TRUE, shows significance asterisks. Default TRUE.
#' @param show_values Logical. If TRUE, displays numerical correlation values in tiles. Default FALSE.
#' @param show_cor Logical alias for \code{show_values}.
#' @param label_type Character string specifying tile label format: \code{"auto"}, \code{"coef"}, \code{"coef_stars"}, \code{"stars"}, or \code{"none"}.
#' @param value_digits Decimal places for correlation coefficients (default 2).
#' @param value_size Font size for tile text (default auto-scaled).
#' @param compact Logical. If TRUE (default), omits leading zero for compact fitting.
#' @param env_label_size Font size for environmental variable labels (default 1.95).
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
#' @param diag_point_size Size of diagonal connector points on heatmaps (default 2.0).
#' @param diag_point_fill Fill color of diagonal connector points (default \code{"#F781BF"}).
#' @param diag_point_color Border color of diagonal connector points (default \code{"#C51B7D"}).
#' @param node_size_range Point size range for central network nodes (default \code{c(1.1, 3.8)}).
#' @param hull_alpha Transparency for module cluster hulls (default 0.18).
#' @param curvature Curvature of inter-domain curved links (default 0.14).
#' @param circle_radius Radius of central boundary circle (default 2.80).
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
    comm_data          = NULL,
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
    preset             = c("default", "nature", "clinical", "seasonal", "cell", "dark", "lancet", "minimal",
                           "multi_tissue", "pharmacology", "circadian", "diurnal", "bipolar", "response"),
    style              = NULL,
    scifig             = FALSE,
    title              = NULL,
    ...
) {
  if (!is.null(show_cor)) show_values <- show_cor
  if (!is.null(style)) preset <- style
  label_type <- match.arg(label_type)
  module_label_pos <- match.arg(module_label_pos)
  preset <- match.arg(preset)

  if (is.null(links) && !is.null(comm_data) && !is.null(env_cors)) {
    links <- tryCatch({
      cli_calc_env_links(comm_data = comm_data, env_data = env_cors, modules = module_labels)
    }, error = function(e) NULL)
  }

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
    show_domain_labels = show_domain_labels,
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
    style              = style,
    scifig             = scifig,
    title              = title
  )

  settings <- list(data = net_data)
  t_obj$draw(NULL, settings, net_data, ...)
}

# Internal layer builder: delegates to Grammar of Graphics engine
build_env_network_layers <- function(
    custom_data         = NULL,
    env_cors            = NULL,
    comm_data           = NULL,
    links               = NULL,
    module_colors       = NULL,
    corner_palettes     = NULL,
    link_colors         = c("Positive" = "#E64B35", "Negative" = "#4DBBD5"),
    resolved_label_type = "none",
    value_digits        = 2,
    value_size          = NULL,
    compact             = TRUE,
    show_module_labels  = TRUE,
    module_labels       = NULL,
    module_label_size   = 2.4,
    module_label_color  = "grey15",
    module_label_fill   = "white",
    module_label_bg     = TRUE,
    module_label_alpha  = 1.0,
    module_label_pos    = "radial",
    show_module_legend  = TRUE,
    legend_position     = "top",
    show_domain_labels  = TRUE,
    env_label_size      = 1.95,
    env_label_color     = "grey20",
    env_label_face      = "plain",
    link_width_range    = c(0.4, 1.4),
    link_width          = NULL,
    link_alpha          = 0.70,
    show_link_values    = FALSE,
    link_value_size     = 2.0,
    link_value_digits   = 2,
    circle_radius       = 2.80,
    circle_width        = 0.4,
    circle_color        = "grey65",
    circle_linetype     = "dashed",
    outer_ring_size     = 0.6,
    outer_ring_color    = "grey60",
    outer_ring_alpha    = 0.6,
    intra_edge_width    = 0.18,
    intra_edge_color    = "grey82",
    intra_edge_alpha    = 0.35,
    diag_point_size     = 2.0,
    diag_point_fill     = "#F781BF",
    diag_point_color    = "#C51B7D",
    node_size_range     = c(1.1, 3.8),
    hull_alpha          = 0.18,
    curvature           = 0.14,
    preset              = "default",
    scifig              = FALSE
) {
  nodes_arg <- if (!is.null(custom_data) && is.list(custom_data) && "nodes" %in% names(custom_data)) custom_data$nodes else NULL
  edges_arg <- if (!is.null(custom_data) && is.list(custom_data) && "edges" %in% names(custom_data)) custom_data$edges else NULL

  d <- cli_env_network_data(
    comm_data           = comm_data,
    nodes               = nodes_arg,
    edges               = edges_arg,
    env_cors            = env_cors,
    links               = links,
    module_labels       = module_labels,
    module_colors       = module_colors,
    corner_palettes     = corner_palettes,
    link_colors         = link_colors,
    label_type          = resolved_label_type,
    value_digits        = value_digits,
    value_size          = value_size,
    compact             = compact,
    show_module_labels  = show_module_labels,
    module_label_size   = module_label_size,
    module_label_color  = module_label_color,
    module_label_fill   = module_label_fill,
    module_label_bg     = module_label_bg,
    module_label_alpha  = module_label_alpha,
    module_label_pos    = module_label_pos,
    show_module_legend  = show_module_legend,
    legend_position     = legend_position,
    show_domain_labels  = show_domain_labels,
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

  p <- ggplot2::ggplot() +
    geom_env_boundary(data = d) +
    geom_env_central_network(data = d) +
    geom_env_corner_heatmaps(data = d) +
    geom_env_links(data = d) +
    coord_env_network() +
    theme_env_network(
      preset = d$preset,
      legend_position = d$legend_position,
      show_module_legend = d$show_module_legend,
      link_width = d$link_width
    )

  list(plot = p, nodes = d$nodes, tiles = d$tiles, links = d$links, module_labels = d$module_labels)
}
