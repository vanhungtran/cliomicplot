# ===========================================================================
# cliomicplot: Multi-Group Cross-Network Architecture Graph
# Multi-Omics & Systems Biology Figure Archetype (Node & Module Levels)
# ===========================================================================

#' Multi-Group Cross-Network Architecture Graph
#'
#' @description
#' Creates publication-ready multi-group network comparison graphs to visualize
#' co-occurrence networks, module conservation, and cross-network shared links
#' across multiple experimental groups or conditions (e.g. KO vs WT vs OE,
#' Healthy vs Lesional vs Non-Lesional, or Multi-Cohort comparisons).
#' Supports two representations:
#' \itemize{
#'   \item \code{level = "node"}: Full network topology with modular clusters,
#'     shaded transparent module hulls, intra-group edges, and inter-group cross-links
#'     connecting shared or conserved features.
#'   \item \code{level = "module"}: Module-level circular ring architecture where
#'     each group is represented as a ring of module nodes sized by module member count,
#'     with inter-group links showing module-level preservation.
#' }
#'
#' @param level Character string specifying visualization level: \code{"node"} (default)
#'   for full node-edge topology, or \code{"module"} for circular module ring preservation.
#' @param groups Character vector specifying group names in desired layout order.
#' @param layout Character string specifying group layout: \code{"triangle"} (default for 3 groups),
#'   \code{"horizontal"}, \code{"vertical"}, or \code{"grid"}.
#' @param module_colors Optional named vector of colors for modules.
#' @param cross_colors Optional named vector of colors for inter-group cross links (e.g. \code{c("KO - WT" = "#E7298A")}).
#' @param show_hulls Logical. If TRUE, draws semi-transparent shaded hulls around modules (node level). Default TRUE.
#' @param hull_alpha Numeric transparency for module hulls (default 0.18).
#' @param show_meta Logical. If TRUE, prints group metadata (Node and Edge counts). Default TRUE.
#' @param node_size_range Numeric vector of length 2 specifying point size range (default \code{c(1.2, 4.2)}).
#' @param edge_color Color for intra-group edges (default \code{"grey85"}).
#' @param edge_alpha Transparency for intra-group edges (default \code{0.45}).
#' @param edge_width Width for intra-group edges (default \code{0.2}).
#' @param cross_width Width for cross-group links (default \code{0.6}).
#' @param cross_alpha Transparency for cross-group links (default \code{0.75}).
#' @param show_module_labels Logical. If TRUE, displays labels for module components in the circles.
#'   Defaults to TRUE when \code{level = "module"} and FALSE when \code{level = "node"}.
#' @param module_labels Optional named vector of custom labels for modules.
#' @param module_label_size Numeric font size for module labels (default 2.6).
#' @param module_label_color Color of module labels (default \code{"grey15"}).
#' @param module_label_bg Logical. If TRUE (default), renders labels in a semi-transparent white pill badge.
#'
#' @return A \code{cliplot_type} object for use with \code{\link{cliplot}}.
#'
#' @export
type_cross_network_graph = function(
    level              = c("node", "module"),
    groups             = NULL,
    layout             = c("triangle", "horizontal", "vertical", "grid"),
    module_colors      = NULL,
    cross_colors       = NULL,
    show_hulls         = TRUE,
    hull_alpha         = 0.18,
    show_meta          = TRUE,
    node_size_range    = c(1.2, 4.2),
    edge_color         = "grey85",
    edge_alpha         = 0.45,
    edge_width         = 0.2,
    cross_width        = 0.6,
    cross_alpha        = 0.75,
    show_module_labels = NULL,
    module_labels      = NULL,
    module_label_size  = 2.6,
    module_label_color = "grey15",
    module_label_bg    = TRUE
) {
  level <- match.arg(level)
  layout <- match.arg(layout)

  cliplot_type(
    data = function(settings, ...) {
      # Input data can be a data frame or list containing nodes, edges, cross_edges
      df <- settings$data
      if (is.null(df)) df <- data.frame()
      df
    },
    draw = function(p, settings, data, ...) {
      args <- list(...)
      custom_data <- args$network_data %||% settings$data

      res <- build_cross_network_layers(
        custom_data        = custom_data,
        level              = level,
        groups             = groups,
        layout             = layout,
        module_colors      = module_colors,
        cross_colors       = cross_colors,
        show_hulls         = show_hulls,
        hull_alpha         = hull_alpha,
        show_meta          = show_meta,
        node_size_range    = node_size_range,
        edge_color         = edge_color,
        edge_alpha         = edge_alpha,
        edge_width         = edge_width,
        cross_width        = cross_width,
        cross_alpha        = cross_alpha,
        show_module_labels = show_module_labels,
        module_labels      = module_labels,
        module_label_size  = module_label_size,
        module_label_color = module_label_color,
        module_label_bg    = module_label_bg
      )

      res$plot
    }
  )
}

#' Direct Cross-Network Architecture Plotting Function
#'
#' @param nodes Optional data frame of nodes containing columns: \code{id}, \code{group}, \code{module},
#'   and optional \code{degree}, \code{size}, \code{x}, \code{y}. If NULL, demonstration data are generated.
#' @param edges Data frame of intra-group edges containing columns: \code{from}, \code{to}, \code{group}.
#' @param cross_edges Data frame of cross-group edges containing columns: \code{from}, \code{to}, \code{group1}, \code{group2}.
#'   If NULL, conserved features with identical \code{id} across groups are linked automatically.
#' @param level Visualization level: \code{"node"} (default) or \code{"module"}.
#' @param groups Group order vector.
#' @param layout Group placement: \code{"triangle"}, \code{"horizontal"}, \code{"vertical"}, or \code{"grid"}.
#' @param module_colors Named color vector for modules.
#' @param cross_colors Named color vector for cross-group links.
#' @param show_module_labels Logical. If TRUE, displays labels for module components in the circles.
#'   Defaults to TRUE when \code{level = "module"} and FALSE when \code{level = "node"}.
#' @param module_labels Optional named vector of custom labels for modules.
#' @param module_label_size Numeric font size for module labels (default 2.6).
#' @param module_label_color Color of module labels (default \code{"grey15"}).
#' @param module_label_bg Logical. If TRUE (default), renders labels in a semi-transparent white pill badge.
#' @param title Optional plot title.
#' @param ... Additional graphic arguments.
#'
#' @return A \code{ggplot} object.
#' @export
cliplot_cross_network_graph = function(
    nodes              = NULL,
    edges              = NULL,
    cross_edges        = NULL,
    level              = c("node", "module"),
    groups             = NULL,
    layout             = c("triangle", "horizontal", "vertical", "grid"),
    module_colors      = NULL,
    cross_colors       = NULL,
    show_module_labels = NULL,
    module_labels      = NULL,
    module_label_size  = 2.6,
    module_label_color = "grey15",
    module_label_bg    = TRUE,
    title              = NULL,
    ...
) {
  level <- match.arg(level)
  layout <- match.arg(layout)

  network_data <- list(
    nodes       = nodes,
    edges       = edges,
    cross_edges = cross_edges
  )

  t_obj <- type_cross_network_graph(
    level              = level,
    groups             = groups,
    layout             = layout,
    module_colors      = module_colors,
    cross_colors       = cross_colors,
    show_module_labels = show_module_labels,
    module_labels      = module_labels,
    module_label_size  = module_label_size,
    module_label_color = module_label_color,
    module_label_bg    = module_label_bg,
    ...
  )

  p <- t_obj$draw(ggplot2::ggplot(), list(data = network_data), network_data, ...)
  if (!is.null(title)) p <- p + ggplot2::labs(title = title)
  p
}

#' Helper builder for cross-network layers
#' @keywords internal
build_cross_network_layers = function(
    custom_data,
    level,
    groups,
    layout,
    module_colors,
    cross_colors,
    show_hulls,
    hull_alpha,
    show_meta,
    node_size_range,
    edge_color,
    edge_alpha,
    edge_width,
    cross_width,
    cross_alpha,
    show_module_labels = NULL,
    module_labels      = NULL,
    module_label_size  = 2.6,
    module_label_color = "grey15",
    module_label_bg    = TRUE
) {
  # Extract components
  if (is.list(custom_data) && !is.data.frame(custom_data)) {
    nodes_df    <- custom_data$nodes
    edges_df    <- custom_data$edges
    cross_df    <- custom_data$cross_edges
  } else if (is.data.frame(custom_data) && nrow(custom_data) > 0) {
    nodes_df    <- custom_data
    edges_df    <- NULL
    cross_df    <- NULL
  } else {
    # Generate default demonstration dataset (KO, WT, OE)
    demo <- generate_demo_cross_network()
    nodes_df    <- demo$nodes
    edges_df    <- demo$edges
    cross_df    <- demo$cross_edges
  }

  grp_names <- if (!is.null(groups)) groups else unique(as.character(nodes_df$group))
  n_grp <- length(grp_names)

  # Group coordinates
  grp_centers <- list()
  if (layout == "triangle" && n_grp == 3) {
    grp_centers[[grp_names[1]]] <- c(x = 0, y = 3.6)
    grp_centers[[grp_names[2]]] <- c(x = -3.5, y = -1.6)
    grp_centers[[grp_names[3]]] <- c(x = 3.5, y = -1.6)
  } else if (layout == "horizontal") {
    xs <- seq(-3.5 * (n_grp - 1) / 2, 3.5 * (n_grp - 1) / 2, length.out = n_grp)
    for (i in seq_along(grp_names)) grp_centers[[grp_names[i]]] <- c(x = xs[i], y = 0)
  } else if (layout == "vertical") {
    ys <- seq(3.5 * (n_grp - 1) / 2, -3.5 * (n_grp - 1) / 2, length.out = n_grp)
    for (i in seq_along(grp_names)) grp_centers[[grp_names[i]]] <- c(x = 0, y = ys[i])
  } else {
    # Default circle/polygon layout
    angles <- seq(pi/2, 2*pi + pi/2, length.out = n_grp + 1)[1:n_grp]
    for (i in seq_along(grp_names)) grp_centers[[grp_names[i]]] <- c(x = 3.2 * cos(angles[i]), y = 3.2 * sin(angles[i]))
  }

  # Build layout coordinates if x, y missing
  if (!all(c("x", "y") %in% names(nodes_df))) {
    nodes_df$x <- 0
    nodes_df$y <- 0
    for (g in grp_names) {
      idx <- which(nodes_df$group == g)
      n_g <- length(idx)
      if (n_g == 0) next
      c_xy <- grp_centers[[g]]
      if (level == "module") {
        angles <- seq(0, 2*pi, length.out = n_g + 1)[1:n_g]
        nodes_df$x[idx] <- c_xy[1] + 1.05 * cos(angles)
        nodes_df$y[idx] <- c_xy[2] + 1.05 * sin(angles)
      } else {
        mods <- unique(nodes_df$module[idx])
        n_m <- length(mods)
        m_ang <- seq(0, 2*pi, length.out = n_m + 1)[1:n_m]
        for (m_i in seq_along(mods)) {
          m_idx <- idx[nodes_df$module[idx] == mods[m_i]]
          n_sub <- length(m_idx)
          sub_cx <- c_xy[1] + 0.45 * cos(m_ang[m_i])
          sub_cy <- c_xy[2] + 0.45 * sin(m_ang[m_i])
          sub_r <- stats::rbeta(n_sub, 1.2, 2.5) * 0.50
          sub_th <- stats::runif(n_sub, 0, 2*pi)
          nodes_df$x[m_idx] <- sub_cx + sub_r * cos(sub_th)
          nodes_df$y[m_idx] <- sub_cy + sub_r * sin(sub_th)
        }
      }
    }
  }

  if (!"degree" %in% names(nodes_df)) nodes_df$degree <- 10

  # Palette
  u_mods <- unique(as.character(nodes_df$module))
  if (is.null(module_colors)) {
    pal <- c("#3B4992", "#EE0000", "#008B45", "#E69F00", "#631879", "#008280", "#BB0021", "#5F559B")
    module_colors <- stats::setNames(pal[((seq_along(u_mods) - 1) %% length(pal)) + 1], u_mods)
  }

  # Build Intra-edges
  intra_coords <- NULL
  if (!is.null(edges_df) && nrow(edges_df) > 0 && all(c("from", "to", "group") %in% names(edges_df))) {
    node_key <- paste(nodes_df$group, nodes_df$id)
    m_from <- match(paste(edges_df$group, edges_df$from), node_key)
    m_to   <- match(paste(edges_df$group, edges_df$to), node_key)
    valid <- !is.na(m_from) & !is.na(m_to)
    if (any(valid)) {
      intra_coords <- data.frame(
        x = nodes_df$x[m_from[valid]],
        y = nodes_df$y[m_from[valid]],
        xend = nodes_df$x[m_to[valid]],
        yend = nodes_df$y[m_to[valid]],
        group = edges_df$group[valid],
        stringsAsFactors = FALSE
      )
    }
  }

  # Build Cross-edges
  cross_coords <- NULL
  if (is.null(cross_df) || nrow(cross_df) == 0) {
    # Detect shared IDs or modules across groups
    shared_pairs <- list()
    for (i in 1:(n_grp - 1)) {
      for (j in (i + 1):n_grp) {
        g1 <- grp_names[i]; g2 <- grp_names[j]
        if (level == "module") {
          mod1 <- nodes_df$module[nodes_df$group == g1]
          mod2 <- nodes_df$module[nodes_df$group == g2]
          common_mod <- intersect(mod1, mod2)
          for (cm in common_mod) {
            id1 <- nodes_df$id[nodes_df$group == g1 & nodes_df$module == cm][1]
            id2 <- nodes_df$id[nodes_df$group == g2 & nodes_df$module == cm][1]
            shared_pairs[[length(shared_pairs) + 1]] <- data.frame(
              from = id1, to = id2,
              group1 = g1, group2 = g2,
              type = paste0(g1, " - ", g2),
              stringsAsFactors = FALSE
            )
          }
        } else {
          id1 <- nodes_df$id[nodes_df$group == g1]
          id2 <- nodes_df$id[nodes_df$group == g2]
          common <- intersect(id1, id2)
          if (length(common) > 0) {
            shared_pairs[[length(shared_pairs) + 1]] <- data.frame(
              from = common, to = common,
              group1 = g1, group2 = g2,
              type = paste0(g1, " - ", g2),
              stringsAsFactors = FALSE
            )
          }
        }
      }
    }
    if (length(shared_pairs) > 0) cross_df <- do.call(rbind, shared_pairs)
  }

  if (!is.null(cross_df) && nrow(cross_df) > 0) {
    node_key <- paste(nodes_df$group, nodes_df$id)
    m_cfrom <- match(paste(cross_df$group1, cross_df$from), node_key)
    m_cto   <- match(paste(cross_df$group2, cross_df$to), node_key)
    valid_c <- !is.na(m_cfrom) & !is.na(m_cto)
    if (any(valid_c)) {
      cross_coords <- data.frame(
        x = nodes_df$x[m_cfrom[valid_c]],
        y = nodes_df$y[m_cfrom[valid_c]],
        xend = nodes_df$x[m_cto[valid_c]],
        yend = nodes_df$y[m_cto[valid_c]],
        type = if ("type" %in% names(cross_df)) cross_df$type[valid_c] else paste0(cross_df$group1[valid_c], " - ", cross_df$group2[valid_c]),
        stringsAsFactors = FALSE
      )
    }
  }

  u_cross_types <- if (!is.null(cross_coords)) unique(cross_coords$type) else character(0)
  if (is.null(cross_colors) && length(u_cross_types) > 0) {
    c_pal <- c("#E7298A", "#1F78B4", "#33A02C", "#FF7F00", "#6A3D9A")
    cross_colors <- stats::setNames(c_pal[((seq_along(u_cross_types) - 1) %% length(c_pal)) + 1], u_cross_types)
  }

  # Build Hulls if node level
  hull_df <- NULL
  if (level == "node" && show_hulls) {
    hull_list <- list()
    theta_seq <- seq(0, 2*pi, length.out = 36)
    for (g in grp_names) {
      g_nodes <- nodes_df[nodes_df$group == g, ]
      for (m in unique(g_nodes$module)) {
        m_pts <- g_nodes[g_nodes$module == m, ]
        if (nrow(m_pts) >= 3) {
          cx <- mean(m_pts$x); cy <- mean(m_pts$y)
          r_max <- max(stats::dist(rbind(c(cx, cy), as.matrix(m_pts[, c("x", "y")]))))[1] * 1.15
          hull_list[[length(hull_list) + 1]] <- data.frame(
            x = cx + r_max * cos(theta_seq),
            y = cy + r_max * sin(theta_seq),
            id = paste0(g, "_", m),
            module = m,
            stringsAsFactors = FALSE
          )
        }
      }
    }
    if (length(hull_list) > 0) hull_df <- do.call(rbind, hull_list)
  }

  # Group Metadata
  meta_df <- NULL
  if (show_meta) {
    meta_list <- list()
    for (g in grp_names) {
      c_xy <- grp_centers[[g]]
      n_n <- sum(nodes_df$group == g)
      n_e <- if (!is.null(edges_df)) sum(edges_df$group == g) else 0
      meta_list[[length(meta_list) + 1]] <- data.frame(
        group = g,
        x = c_xy[1],
        y = if (c_xy[2] > 0) c_xy[2] + 2.0 else c_xy[2] - 1.7,
        label = paste0("Group = ", g, "\nNode = ", n_n, if (n_e > 0) paste0("\nEdge = ", n_e) else ""),
        stringsAsFactors = FALSE
      )
    }
    meta_df <- do.call(rbind, meta_list)
  }

  # Assemble Plot
  p <- ggplot2::ggplot()

  if (!is.null(hull_df)) {
    p <- p + ggplot2::geom_polygon(
      data = hull_df,
      ggplot2::aes(x = .data[["x"]], y = .data[["y"]], group = .data[["id"]], fill = .data[["module"]]),
      alpha = hull_alpha, show.legend = FALSE
    )
  }

  if (!is.null(intra_coords) && nrow(intra_coords) > 0) {
    p <- p + ggplot2::geom_segment(
      data = intra_coords,
      ggplot2::aes(x = .data[["x"]], y = .data[["y"]], xend = .data[["xend"]], yend = .data[["yend"]]),
      color = edge_color, alpha = edge_alpha, linewidth = edge_width
    )
  }

  if (!is.null(cross_coords) && nrow(cross_coords) > 0) {
    p <- p + ggplot2::geom_segment(
      data = cross_coords,
      ggplot2::aes(x = .data[["x"]], y = .data[["y"]], xend = .data[["xend"]], yend = .data[["yend"]], color = .data[["type"]]),
      linewidth = cross_width, alpha = cross_alpha
    ) +
    ggplot2::scale_color_manual(values = cross_colors, name = "Conserved Cross Links")
  }

  if (level == "module") {
    # Module circle layout
    p <- p + ggplot2::geom_point(
      data = nodes_df,
      ggplot2::aes(x = .data[["x"]], y = .data[["y"]], size = .data[["degree"]], fill = .data[["module"]]),
      shape = 21, color = "white", stroke = 0.5
    ) +
    ggplot2::scale_size_continuous(range = c(3.5, 8.5), name = "Module Size")
  } else {
    p <- p + ggplot2::geom_point(
      data = nodes_df,
      ggplot2::aes(x = .data[["x"]], y = .data[["y"]], fill = .data[["module"]], size = .data[["degree"]]),
      shape = 21, color = "grey25", stroke = 0.25
    ) +
    ggplot2::scale_size_continuous(range = node_size_range, guide = "none")
  }

  p <- p + ggplot2::scale_fill_manual(values = module_colors, name = "Network Module")

  resolved_show_labels <- if (!is.null(show_module_labels)) {
    isTRUE(show_module_labels)
  } else {
    level == "module"
  }

  if (resolved_show_labels) {
    if (level == "module") {
      mod_lbl_text <- if (!is.null(module_labels)) {
        if (!is.null(names(module_labels)) && all(nodes_df$module %in% names(module_labels))) {
          module_labels[as.character(nodes_df$module)]
        } else if (length(module_labels) == nrow(nodes_df)) {
          module_labels
        } else {
          sub("^Mod_", "M", as.character(nodes_df$module))
        }
      } else {
        sub("^Mod_", "M", as.character(nodes_df$module))
      }

      nodes_lbl_df <- nodes_df
      nodes_lbl_df$lbl <- mod_lbl_text

      if (isTRUE(module_label_bg)) {
        p <- p + ggplot2::geom_label(
          data = nodes_lbl_df,
          ggplot2::aes(x = .data[["x"]], y = .data[["y"]], label = .data[["lbl"]]),
          size = module_label_size,
          fill = "white",
          color = module_label_color,
          fontface = "bold",
          alpha = 0.88,
          label.padding = ggplot2::unit(0.18, "lines"),
          label.r = ggplot2::unit(0.16, "lines"),
          linewidth = 0.2,
          show.legend = FALSE
        )
      } else {
        p <- p + ggplot2::geom_text(
          data = nodes_lbl_df,
          ggplot2::aes(x = .data[["x"]], y = .data[["y"]], label = .data[["lbl"]]),
          size = module_label_size,
          color = module_label_color,
          fontface = "bold",
          show.legend = FALSE
        )
      }
    } else if (level == "node" && !is.null(hull_df)) {
      centroid_list <- list()
      for (g in grp_names) {
        g_nodes <- nodes_df[nodes_df$group == g, ]
        for (m in unique(g_nodes$module)) {
          m_pts <- g_nodes[g_nodes$module == m, ]
          if (nrow(m_pts) >= 3) {
            lbl <- if (!is.null(module_labels) && m %in% names(module_labels)) {
              as.character(module_labels[m])
            } else {
              sub("^Mod_", "Mod ", m)
            }
            centroid_list[[length(centroid_list) + 1]] <- data.frame(
              x = mean(m_pts$x),
              y = mean(m_pts$y),
              label = lbl,
              stringsAsFactors = FALSE
            )
          }
        }
      }
      if (length(centroid_list) > 0) {
        mod_cents <- do.call(rbind, centroid_list)
        if (isTRUE(module_label_bg)) {
          p <- p + ggplot2::geom_label(
            data = mod_cents,
            ggplot2::aes(x = .data[["x"]], y = .data[["y"]], label = .data[["label"]]),
            size = module_label_size,
            fill = "white",
            color = module_label_color,
            fontface = "bold",
            alpha = 0.88,
            label.padding = ggplot2::unit(0.20, "lines"),
            label.r = ggplot2::unit(0.18, "lines"),
            linewidth = 0.2,
            show.legend = FALSE
          )
        } else {
          p <- p + ggplot2::geom_text(
            data = mod_cents,
            ggplot2::aes(x = .data[["x"]], y = .data[["y"]], label = .data[["label"]]),
            size = module_label_size,
            color = module_label_color,
            fontface = "bold",
            show.legend = FALSE
          )
        }
      }
    }
  }

  if (!is.null(meta_df)) {
    p <- p + ggplot2::geom_text(
      data = meta_df,
      ggplot2::aes(x = .data[["x"]], y = .data[["y"]], label = .data[["label"]]),
      size = 3.2, fontface = "bold", lineheight = 0.95
    )
  }

  p <- p +
    ggplot2::coord_fixed(xlim = c(-5.5, 5.5), ylim = c(-3.8, 6.2)) +
    ggplot2::theme_void() +
    ggplot2::theme(
      legend.position = "right",
      legend.title = ggplot2::element_text(size = 9, face = "bold"),
      legend.text  = ggplot2::element_text(size = 8)
    )

  list(plot = p, nodes = nodes_df, edges = intra_coords, cross = cross_coords)
}

#' Generate Demonstration Dataset for Cross-Network Architecture
#'
#' @return A list with \code{nodes}, \code{edges}, and \code{cross_edges}.
#' @export
generate_demo_cross_network = function() {
  set.seed(42)
  groups <- c("KO", "WT", "OE")
  taxa_pool <- paste0("OTU_", 1:150)
  nodes_list <- list()
  edges_list <- list()

  for (g in groups) {
    n_t <- if (g == "KO") 85 else if (g == "WT") 65 else 50
    sampled <- sample(taxa_pool, n_t)
    mods <- paste0("Mod_", sample(1:4, n_t, replace = TRUE, prob = c(0.35, 0.25, 0.25, 0.15)))
    nodes_list[[g]] <- data.frame(
      id = sampled,
      group = g,
      module = mods,
      degree = sample(3:35, n_t, replace = TRUE),
      stringsAsFactors = FALSE
    )
    n_e <- round(n_t * 2.2)
    e_f <- sample(sampled, n_e, replace = TRUE)
    e_t <- sample(sampled, n_e, replace = TRUE)
    keep <- e_f != e_t
    edges_list[[g]] <- data.frame(
      from = e_f[keep], to = e_t[keep], group = g, stringsAsFactors = FALSE
    )
  }

  all_n <- do.call(rbind, nodes_list)
  all_e <- do.call(rbind, edges_list)

  # Conserved cross links
  s_kw <- intersect(nodes_list$KO$id, nodes_list$WT$id)[1:14]
  s_ko <- intersect(nodes_list$KO$id, nodes_list$OE$id)[1:12]
  s_wo <- intersect(nodes_list$WT$id, nodes_list$OE$id)[1:8]

  cross <- rbind(
    data.frame(from = s_kw, to = s_kw, group1 = "KO", group2 = "WT", type = "KO - WT", stringsAsFactors = FALSE),
    data.frame(from = s_ko, to = s_ko, group1 = "KO", group2 = "OE", type = "KO - OE", stringsAsFactors = FALSE),
    data.frame(from = s_wo, to = s_wo, group1 = "WT", group2 = "OE", type = "WT - OE", stringsAsFactors = FALSE)
  )

  list(nodes = all_n, edges = all_e, cross_edges = cross)
}
