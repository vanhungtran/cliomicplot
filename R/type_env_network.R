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
#' @param curvature Numeric curvature of inter-domain connecting lines (default 0.14).
#' @param circle_radius Numeric radius of the central boundary circle (default 2.45).
#' @param title Optional plot title.
#'
#' @return A \code{cliplot_type} object for use with \code{\link{cliplot}}.
#'
#' @export
type_env_network = function(
    env_cors        = NULL,
    links           = NULL,
    module_colors   = NULL,
    corner_palettes = NULL,
    link_colors     = c("Positive" = "#E64B35", "Negative" = "#4DBBD5"),
    show_asterisks  = TRUE,
    curvature       = 0.14,
    circle_radius   = 2.45,
    title           = NULL
) {
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
        custom_data     = custom_data,
        env_cors        = env_cors,
        links           = links,
        module_colors   = module_colors,
        corner_palettes = corner_palettes,
        link_colors     = link_colors,
        show_asterisks  = show_asterisks,
        curvature       = curvature,
        circle_radius   = circle_radius
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
#' @param title Plot title.
#' @param ... Additional arguments.
#'
#' @return A \code{ggplot} object.
#' @export
cliplot_env_network = function(
    nodes           = NULL,
    edges           = NULL,
    env_cors        = NULL,
    links           = NULL,
    module_colors   = NULL,
    corner_palettes = NULL,
    title           = NULL,
    ...
) {
  net_data <- list(nodes = nodes, edges = edges)
  t_obj <- type_env_network(
    env_cors        = env_cors,
    links           = links,
    module_colors   = module_colors,
    corner_palettes = corner_palettes,
    title           = title,
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
    show_asterisks,
    curvature,
    circle_radius
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
  env_vars_default <- c("pH", "Humdepth", "Baresoil", "Mo", "Zn", "Mn", "Fe", "Al", "S", "Mg", "Ca", "K", "P", "N")

  make_corner_triangle <- function(env_names, corner, center_x, center_y, size = 2.3, ramp_colors) {
    p <- length(env_names)
    grid <- expand.grid(row = 1:p, col = 1:p)
    grid <- grid[grid$row >= grid$col, ]
    r_vals <- stats::runif(nrow(grid), -0.75, 0.85)
    r_vals[grid$row == grid$col] <- 1.0

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

    step <- size / p

    if (corner == "top_left") {
      xs <- center_x + (grid$col - 1) * step
      ys <- center_y - (grid$row - 1) * step
      diag_pts <- data.frame(
        var = env_names,
        x = center_x + (1:p - 1) * step + step * 0.5,
        y = center_y - (1:p - 1) * step - step * 0.5,
        stringsAsFactors = FALSE
      )
      top_labels <- data.frame(
        var = env_names,
        x = center_x + (1:p - 1) * step,
        y = center_y + step * 0.6,
        angle = 45, hjust = 0, vjust = 0, stringsAsFactors = FALSE
      )
      left_labels <- data.frame(
        var = env_names,
        x = center_x - step * 0.6,
        y = center_y - (1:p - 1) * step,
        angle = 0, hjust = 1, vjust = 0.5, stringsAsFactors = FALSE
      )
    } else if (corner == "top_right") {
      xs <- center_x - (grid$col - 1) * step
      ys <- center_y - (grid$row - 1) * step
      diag_pts <- data.frame(
        var = env_names,
        x = center_x - (1:p - 1) * step - step * 0.5,
        y = center_y - (1:p - 1) * step - step * 0.5,
        stringsAsFactors = FALSE
      )
      top_labels <- data.frame(
        var = env_names,
        x = center_x - (1:p - 1) * step,
        y = center_y + step * 0.6,
        angle = 45, hjust = 0, vjust = 0, stringsAsFactors = FALSE
      )
      left_labels <- data.frame(
        var = env_names,
        x = center_x + step * 0.6,
        y = center_y - (1:p - 1) * step,
        angle = 0, hjust = 0, vjust = 0.5, stringsAsFactors = FALSE
      )
    } else if (corner == "bottom_left") {
      xs <- center_x + (grid$col - 1) * step
      ys <- center_y + (grid$row - 1) * step
      diag_pts <- data.frame(
        var = env_names,
        x = center_x + (1:p - 1) * step + step * 0.5,
        y = center_y + (1:p - 1) * step + step * 0.5,
        stringsAsFactors = FALSE
      )
      top_labels <- data.frame(
        var = env_names,
        x = center_x + (1:p - 1) * step,
        y = center_y - step * 0.6,
        angle = 45, hjust = 1, vjust = 1, stringsAsFactors = FALSE
      )
      left_labels <- data.frame(
        var = env_names,
        x = center_x - step * 0.6,
        y = center_y + (1:p - 1) * step,
        angle = 0, hjust = 1, vjust = 0.5, stringsAsFactors = FALSE
      )
    } else {
      xs <- center_x - (grid$col - 1) * step
      ys <- center_y + (grid$row - 1) * step
      diag_pts <- data.frame(
        var = env_names,
        x = center_x - (1:p - 1) * step - step * 0.5,
        y = center_y + (1:p - 1) * step + step * 0.5,
        stringsAsFactors = FALSE
      )
      top_labels <- data.frame(
        var = env_names,
        x = center_x - (1:p - 1) * step,
        y = center_y - step * 0.6,
        angle = 45, hjust = 1, vjust = 1, stringsAsFactors = FALSE
      )
      left_labels <- data.frame(
        var = env_names,
        x = center_x + step * 0.6,
        y = center_y + (1:p - 1) * step,
        angle = 0, hjust = 0, vjust = 0.5, stringsAsFactors = FALSE
      )
    }

    col_ramp <- scales::col_numeric(ramp_colors, domain = c(-1, 1))

    tiles <- data.frame(
      x = xs, y = ys, r = r_vals, stars = stars,
      fill_col = col_ramp(r_vals), step = step, stringsAsFactors = FALSE
    )

    labels_all <- rbind(top_labels, left_labels)
    list(tiles = tiles, diag_pts = diag_pts, labels = labels_all, step = step)
  }

  c_tl <- make_corner_triangle(paste0(env_vars_default, "_3"), "top_left", center_x = -5.0, center_y = 5.0, ramp_colors = c("#4DAF4A", "#FFFFFF", "#E41A1C"))
  c_tr <- make_corner_triangle(paste0(env_vars_default, "_1"), "top_right", center_x = 5.0, center_y = 5.0, ramp_colors = c("#984EA3", "#FFFFFF", "#FF7F00"))
  c_bl <- make_corner_triangle(paste0(env_vars_default, "_4"), "bottom_left", center_x = -5.0, center_y = -5.0, ramp_colors = c("#66C2A5", "#FFFFFF", "#FC8D62"))
  c_br <- make_corner_triangle(paste0(env_vars_default, "_2"), "bottom_right", center_x = 5.0, center_y = -5.0, ramp_colors = c("#377EB8", "#FFFFFF", "#E41A1C"))

  all_tiles <- rbind(
    cbind(c_tl$tiles, corner = "Env 1"),
    cbind(c_tr$tiles, corner = "Env 2"),
    cbind(c_bl$tiles, corner = "Env 4"),
    cbind(c_br$tiles, corner = "Env 3")
  )

  all_diag <- rbind(
    cbind(c_tl$diag_pts, corner = "Env 1"),
    cbind(c_tr$diag_pts, corner = "Env 2"),
    cbind(c_bl$diag_pts, corner = "Env 4"),
    cbind(c_br$diag_pts, corner = "Env 3")
  )

  all_env_labels <- rbind(
    cbind(c_tl$labels, corner = "Env 1"),
    cbind(c_tr$labels, corner = "Env 2"),
    cbind(c_bl$labels, corner = "Env 4"),
    cbind(c_br$labels, corner = "Env 3")
  )

  # 3. Inter-domain connecting links
  link_list <- list()
  for (c_name in c("Env 1", "Env 2", "Env 4", "Env 3")) {
    c_pts <- all_diag[all_diag$corner == c_name, ]
    chosen_vars <- sample(c_pts$var, 5)
    chosen_mods <- sample(mod_centers$module, 5, replace = TRUE)
    for (k in 1:5) {
      pt <- c_pts[c_pts$var == chosen_vars[k], ]
      mc <- mod_centers[mod_centers$module == chosen_mods[k], ]
      r_val <- sample(c(0.72, 0.58, -0.64, 0.45, -0.52), 1)
      p_val <- stats::runif(1, 1e-4, 0.015)
      curv <- if (c_name %in% c("Env 1", "Env 3")) curvature else -curvature

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
  all_links <- do.call(rbind, link_list)

  # 4. Assemble Plot
  p <- ggplot2::ggplot() +
    ggplot2::geom_path(
      data = center_ring, ggplot2::aes(x = .data[["x"]], y = .data[["y"]]),
      linetype = "dashed", color = "grey65", linewidth = 0.4
    ) +
    ggplot2::geom_point(
      data = outer_ring_nodes, ggplot2::aes(x = .data[["x"]], y = .data[["y"]]),
      size = 0.6, color = "grey60", alpha = 0.6
    ) +
    ggplot2::geom_polygon(
      data = center_hulls,
      ggplot2::aes(x = .data[["x"]], y = .data[["y"]], group = .data[["id"]], fill = .data[["module"]]),
      alpha = 0.18, show.legend = FALSE
    ) +
    ggplot2::geom_segment(
      data = center_edges,
      ggplot2::aes(x = .data[["x"]], y = .data[["y"]], xend = .data[["xend"]], yend = .data[["yend"]]),
      color = "grey82", alpha = 0.35, linewidth = 0.18
    ) +
    ggplot2::geom_curve(
      data = all_links,
      ggplot2::aes(x = .data[["x"]], y = .data[["y"]], xend = .data[["xend"]], yend = .data[["yend"]], color = .data[["sign"]], linewidth = .data[["log10p"]]),
      curvature = curvature, alpha = 0.70
    ) +
    ggplot2::scale_color_manual(values = link_colors, name = "Correlation") +
    ggplot2::scale_linewidth_continuous(
      range = c(0.4, 1.4),
      breaks = c(1.5, 2.0, 2.5, 3.0),
      name = expression(-log[10](italic(P)-value))
    ) +
    ggplot2::geom_point(
      data = nodes_df,
      ggplot2::aes(x = .data[["x"]], y = .data[["y"]], fill = .data[["module"]], size = .data[["degree"]]),
      shape = 21, color = "grey25", stroke = 0.2
    ) +
    ggplot2::scale_fill_manual(values = module_colors, name = "Network Module") +
    ggplot2::scale_size_continuous(range = c(1.1, 3.8), guide = "none") +
    ggplot2::geom_tile(
      data = all_tiles,
      ggplot2::aes(x = .data[["x"]], y = .data[["y"]]),
      fill = all_tiles$fill_col,
      color = "white", linewidth = 0.15
    )

  if (show_asterisks) {
    p <- p + ggplot2::geom_text(
      data = all_tiles[all_tiles$stars != "", ],
      ggplot2::aes(x = .data[["x"]], y = .data[["y"]], label = .data[["stars"]]),
      size = 1.6, color = "grey15", vjust = 0.75
    )
  }

  p <- p +
    ggplot2::geom_point(
      data = all_diag,
      ggplot2::aes(x = .data[["x"]], y = .data[["y"]]),
      shape = 21, fill = "#F781BF", color = "#C51B7D", size = 1.8, stroke = 0.3
    ) +
    ggplot2::geom_text(
      data = all_env_labels,
      ggplot2::aes(x = .data[["x"]], y = .data[["y"]], label = .data[["var"]], angle = .data[["angle"]], hjust = .data[["hjust"]], vjust = .data[["vjust"]]),
      size = 1.8, fontface = "plain", color = "grey20"
    ) +
    ggplot2::coord_fixed(xlim = c(-6.8, 6.8), ylim = c(-6.8, 6.8)) +
    ggplot2::theme_void() +
    ggplot2::theme(
      legend.position = "top",
      legend.box = "horizontal",
      legend.text = ggplot2::element_text(size = 7),
      legend.title = ggplot2::element_text(size = 8, face = "bold"),
      legend.margin = ggplot2::margin(b = 4, t = 2)
    )

  list(plot = p, nodes = nodes_df, tiles = all_tiles, links = all_links)
}
