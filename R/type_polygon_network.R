# ===========================================================================
# cliomicplot: Modular Polygon Ring Network Layout (PolygonClusterG / PolygonRrClusterG)
# Co-Occurrence Community Modularity & Spatial Ring Topologies
# Inspired by ggClusterNet 2 (PolygonClusterG, PolygonRrClusterG, Wen et al. 2025)
# ===========================================================================

#' Modular Polygon Ring Network Plot Type
#'
#' @description
#' Creates a publication-grade modular polygon ring network layout where community
#' modules are placed as circular node rings along the vertices of a macro-polygon
#' (Wen et al. iMeta 2025). Supports both constant ring radii (\code{"PolygonClusterG"})
#' and adaptive ring radii proportional to module member count (\code{"PolygonRrClusterG"}).
#'
#' @param adaptive_radius Logical. If TRUE (default, \code{"PolygonRrClusterG"}), module ring
#'   radii scale with \eqn{\sqrt{N_k / \bar{N}}} so larger modules occupy larger rings.
#'   If FALSE (\code{"PolygonClusterG"}), all module rings have uniform radius.
#' @param macro_radius Numeric radius for the macro-polygon placing module centers (default 3.8).
#' @param base_ring_radius Numeric base radius for module circular rings (default 1.15).
#' @param show_module_hulls Logical. If TRUE (default), displays shaded circular hulls behind each module ring.
#' @param hull_alpha Numeric transparency for module ring background hulls (default 0.12).
#' @param show_module_badges Logical. If TRUE (default), renders a central pill badge in each ring showing module ID and node count.
#' @param intra_edge_color Color for intra-module edges (default \code{"#CBD5E1"}, light slate).
#' @param intra_edge_alpha Numeric transparency for intra-module edges (default 0.45).
#' @param intra_edge_width Numeric line width for intra-module edges (default 0.25).
#' @param inter_edge_color Color or palette for cross-module edges (default \code{"#E64B35"}).
#' @param inter_edge_alpha Numeric transparency for cross-module edges (default 0.65).
#' @param inter_edge_width Numeric line width for cross-module edges (default 0.45).
#' @param inter_edge_curved Logical. If TRUE (default), cross-module edges are drawn as smooth curved arcs.
#' @param module_palette Optional named vector of colors for modules.
#' @param node_size_range Numeric vector of length 2 for node sizes mapped to intra-module degree (default \code{c(1.8, 5.0)}).
#' @param label_top Integer. Number of top hub taxa per module to label (default 2).
#' @param label_size Numeric font size for node labels (default 2.6).
#' @param nodes Optional data frame of nodes with columns \code{node} and \code{module}.
#' @param edges Optional data frame of edges with columns \code{from} and \code{to}.
#'
#' @return A \code{cliplot_type} object for use with \code{\link{cliplot}}.
#'
#' @export
#' @examples
#' \dontrun{
#' cliplot(type = type_polygon_network(nodes = nodes_df, edges = edges_df, adaptive_radius = TRUE))
#' }
type_polygon_network = function(
    adaptive_radius     = TRUE,
    macro_radius        = 3.8,
    base_ring_radius    = 1.15,
    show_module_hulls   = TRUE,
    hull_alpha          = 0.12,
    show_module_badges  = TRUE,
    intra_edge_color    = "#CBD5E1",
    intra_edge_alpha    = 0.45,
    intra_edge_width    = 0.25,
    inter_edge_color    = "#E64B35",
    inter_edge_alpha    = 0.65,
    inter_edge_width    = 0.45,
    inter_edge_curved   = TRUE,
    module_palette      = NULL,
    node_size_range     = c(1.8, 5.0),
    label_top           = 2,
    label_size          = 2.6,
    nodes               = NULL,
    edges               = NULL
) {
  default_mod_pal <- c(
    "#3B4992", "#EE0000", "#008B45", "#631879", "#008280",
    "#BB0021", "#5F559B", "#A20056", "#808180", "#1B1919"
  )

  cliplot_type(
    name = "polygon_network",
    data = function(settings, ...) {
      settings$adaptive_radius    <- adaptive_radius
      settings$macro_radius       <- macro_radius
      settings$base_ring_radius   <- base_ring_radius
      settings$show_module_hulls  <- show_module_hulls
      settings$hull_alpha         <- hull_alpha
      settings$show_module_badges <- show_module_badges
      settings$intra_edge_color   <- intra_edge_color
      settings$intra_edge_alpha   <- intra_edge_alpha
      settings$intra_edge_width   <- intra_edge_width
      settings$inter_edge_color   <- inter_edge_color
      settings$inter_edge_alpha   <- inter_edge_alpha
      settings$inter_edge_width   <- inter_edge_width
      settings$inter_edge_curved  <- inter_edge_curved
      settings$module_palette     <- module_palette %||% default_mod_pal
      settings$node_size_range    <- node_size_range
      settings$label_top          <- label_top
      settings$label_size         <- label_size
      settings$nodes              <- nodes
      settings$edges              <- edges
      if (is.null(settings$legend)) settings$legend <- "bottom"
    },
    draw = function(data, mapping, settings, ...) {
      nodes_df <- settings$nodes
      edges_df <- settings$edges

      # Generate demo modular network if null
      if (is.null(nodes_df) || is.null(edges_df)) {
        set.seed(42)
        n_mods <- 5
        mod_sizes <- c(18, 14, 12, 9, 8)
        node_list <- list()
        for (m in seq_len(n_mods)) {
          m_name <- paste0("Mod_", m)
          nd_names <- paste0("ASV_M", m, "_", sprintf("%02d", seq_len(mod_sizes[m])))
          node_list[[m]] <- data.frame(
            node   = nd_names,
            module = m_name,
            phylum = sample(c("Bacteroidetes", "Firmicutes", "Actinobacteria", "Proteobacteria"),
                            mod_sizes[m], replace = TRUE),
            stringsAsFactors = FALSE
          )
        }
        nodes_df <- do.call(rbind, node_list)

        # Generate intra and inter edges
        edge_list <- list()
        for (m in seq_len(n_mods)) {
          m_nodes <- nodes_df$node[nodes_df$module == paste0("Mod_", m)]
          grid_m <- expand.grid(from = m_nodes, to = m_nodes, stringsAsFactors = FALSE)
          grid_m <- grid_m[grid_m$from < grid_m$to, ]
          # Sample ~25% intra connections
          grid_m <- grid_m[stats::runif(nrow(grid_m)) < 0.28, ]
          grid_m$type <- "intra"
          edge_list[[length(edge_list) + 1]] <- grid_m
        }

        # Sparse inter-module connections
        all_grid <- expand.grid(from = nodes_df$node, to = nodes_df$node, stringsAsFactors = FALSE)
        all_grid <- all_grid[all_grid$from < all_grid$to, ]
        all_grid <- all_grid[stats::runif(nrow(all_grid)) < 0.018, ]
        all_grid$type <- "inter"
        edge_list[[length(edge_list) + 1]] <- all_grid

        edges_df <- do.call(rbind, edge_list)
      }

      names(nodes_df) <- tolower(names(nodes_df))
      names(edges_df) <- tolower(names(edges_df))

      u_mods <- sort(unique(as.character(nodes_df$module)))
      k_mods <- length(u_mods)

      # Macro-polygon module centers
      theta_macro <- seq(pi/2, 2*pi + pi/2, length.out = k_mods + 1)[seq_len(k_mods)]
      center_x <- settings$macro_radius * cos(theta_macro)
      center_y <- settings$macro_radius * sin(theta_macro)
      names(center_x) <- names(center_y) <- u_mods

      # Calculate node degrees
      all_edges_vec <- c(as.character(edges_df$from), as.character(edges_df$to))
      deg_tbl <- table(all_edges_vec)
      nodes_df$degree <- as.numeric(deg_tbl[as.character(nodes_df$node)])
      nodes_df$degree[is.na(nodes_df$degree)] <- 0

      # Module sizes and radii
      mod_counts <- table(nodes_df$module)
      mean_count <- mean(mod_counts)

      ring_radii <- numeric(k_mods)
      names(ring_radii) <- u_mods
      for (m in u_mods) {
        if (settings$adaptive_radius) {
          # PolygonRrClusterG: radius scales with sqrt(N_k / mean_N)
          ring_radii[m] <- settings$base_ring_radius * sqrt(max(0.4, mod_counts[m] / mean_count))
        } else {
          # PolygonClusterG: constant radius
          ring_radii[m] <- settings$base_ring_radius
        }
      }

      # Compute node ring coordinates
      placed_list <- list()
      badge_list <- list()
      hull_list <- list()

      for (m in u_mods) {
        m_nodes <- nodes_df[nodes_df$module == m, , drop = FALSE]
        n_m <- nrow(m_nodes)
        cx <- center_x[m]
        cy <- center_y[m]
        rr <- ring_radii[m]

        # Angles for nodes in this ring
        th_ring <- seq(0, 2*pi, length.out = n_m + 1)[seq_len(n_m)]
        m_nodes$x <- cx + rr * cos(th_ring)
        m_nodes$y <- cy + rr * sin(th_ring)
        placed_list[[m]] <- m_nodes

        # Module Center Badge
        badge_list[[m]] <- data.frame(
          module = m,
          x      = cx,
          y      = cy,
          radius = rr,
          count  = n_m,
          label  = paste0(m, "\n(n=", n_m, ")"),
          stringsAsFactors = FALSE
        )

        # Hull polygon points
        th_hull <- seq(0, 2*pi, length.out = 40)
        hull_list[[m]] <- data.frame(
          module = m,
          x      = cx + (rr * 1.22) * cos(th_hull),
          y      = cy + (rr * 1.22) * sin(th_hull),
          stringsAsFactors = FALSE
        )
      }

      placed_nodes <- do.call(rbind, placed_list)
      rownames(placed_nodes) <- as.character(placed_nodes$node)
      badge_df <- do.call(rbind, badge_list)
      hull_df  <- do.call(rbind, hull_list)

      # Map edge endpoints
      edges_df$from_mod <- placed_nodes[as.character(edges_df$from), "module"]
      edges_df$to_mod   <- placed_nodes[as.character(edges_df$to), "module"]
      edges_df$is_intra <- edges_df$from_mod == edges_df$to_mod

      edges_df$x_start <- placed_nodes[as.character(edges_df$from), "x"]
      edges_df$y_start <- placed_nodes[as.character(edges_df$from), "y"]
      edges_df$x_end   <- placed_nodes[as.character(edges_df$to), "x"]
      edges_df$y_end   <- placed_nodes[as.character(edges_df$to), "y"]
      edges_df <- edges_df[!is.na(edges_df$x_start) & !is.na(edges_df$x_end), ]

      intra_edges <- edges_df[edges_df$is_intra, , drop = FALSE]
      inter_edges <- edges_df[!edges_df$is_intra, , drop = FALSE]

      # Color mapping
      mod_pal <- settings$module_palette
      if (length(mod_pal) < k_mods) mod_pal <- rep(mod_pal, length.out = k_mods)
      mod_colors <- stats::setNames(mod_pal[seq_len(k_mods)], u_mods)

      # Keystone Hub Labels (top degree per module)
      label_list <- list()
      for (m in u_mods) {
        m_sub <- placed_nodes[placed_nodes$module == m, ]
        top_m <- utils::head(m_sub[order(-m_sub$degree), ], settings$label_top)
        top_m <- top_m[top_m$degree > 0, ]
        if (nrow(top_m) > 0) label_list[[m]] <- top_m
      }
      top_labels <- if (length(label_list) > 0) do.call(rbind, label_list) else data.frame()

      # Assemble graphic
      p <- ggplot2::ggplot()

      # 1. Shaded Circular Background Hulls
      if (settings$show_module_hulls) {
        p <- p + ggplot2::geom_polygon(
          data = hull_df,
          ggplot2::aes(x = .data$x, y = .data$y, group = .data$module, fill = .data$module),
          alpha = settings$hull_alpha,
          color = NA,
          show.legend = FALSE
        )
      }

      # 2. Intra-Module Edges (straight thin segments)
      if (nrow(intra_edges) > 0) {
        p <- p + ggplot2::geom_segment(
          data = intra_edges,
          ggplot2::aes(x = .data$x_start, y = .data$y_start, xend = .data$x_end, yend = .data$y_end),
          color = settings$intra_edge_color,
          alpha = settings$intra_edge_alpha,
          linewidth = settings$intra_edge_width
        )
      }

      # 3. Inter-Module Edges (smooth curved or colored cross-links)
      if (nrow(inter_edges) > 0) {
        if (settings$inter_edge_curved) {
          p <- p + ggplot2::geom_curve(
            data = inter_edges,
            ggplot2::aes(x = .data$x_start, y = .data$y_start, xend = .data$x_end, yend = .data$y_end),
            color = settings$inter_edge_color,
            alpha = settings$inter_edge_alpha,
            linewidth = settings$inter_edge_width,
            curvature = 0.22,
            lineend = "round"
          )
        } else {
          p <- p + ggplot2::geom_segment(
            data = inter_edges,
            ggplot2::aes(x = .data$x_start, y = .data$y_start, xend = .data$x_end, yend = .data$y_end),
            color = settings$inter_edge_color,
            alpha = settings$inter_edge_alpha,
            linewidth = settings$inter_edge_width
          )
        }
      }

      # 4. Center Module Badge
      if (settings$show_module_badges) {
        p <- p +
          ggplot2::geom_point(
            data = badge_df,
            ggplot2::aes(x = .data$x, y = .data$y, fill = .data$module),
            size = 8.5,
            shape = 21,
            color = "white",
            stroke = 1.0,
            alpha = 0.90,
            show.legend = FALSE
          ) +
          ggplot2::geom_text(
            data = badge_df,
            ggplot2::aes(x = .data$x, y = .data$y, label = .data$module),
            size = 2.4,
            fontface = "bold",
            color = "white"
          )
      }

      # 5. Nodes
      p <- p +
        ggplot2::geom_point(
          data = placed_nodes,
          ggplot2::aes(x = .data$x, y = .data$y, fill = .data$module, size = .data$degree),
          shape = 21,
          color = "white",
          stroke = 0.4,
          alpha = 0.95
        ) +
        ggplot2::scale_fill_manual(values = mod_colors, name = "Module: ") +
        ggplot2::scale_size_continuous(range = settings$node_size_range, name = "Degree: ")

      # 6. Keystone Node Labels
      if (nrow(top_labels) > 0) {
        if (requireNamespace("ggrepel", quietly = TRUE)) {
          p <- p + ggrepel::geom_text_repel(
            data = top_labels,
            ggplot2::aes(x = .data$x, y = .data$y, label = .data$node),
            size = settings$label_size,
            fontface = "bold",
            color = "#1E293B",
            bg.color = "white",
            bg.r = 0.12,
            box.padding = 0.15,
            point.padding = 0.15,
            max.overlaps = 50,
            show.legend = FALSE
          )
        } else {
          p <- p + ggplot2::geom_text(
            data = top_labels,
            ggplot2::aes(x = .data$x, y = .data$y, label = .data$node),
            size = settings$label_size,
            fontface = "bold",
            color = "#1E293B",
            vjust = -0.5,
            show.legend = FALSE
          )
        }
      }

      # Coordinates and Canvas Boundaries
      max_coord <- settings$macro_radius + max(ring_radii) * 1.5 + 0.8
      p <- p +
        ggplot2::coord_fixed(xlim = c(-max_coord, max_coord), ylim = c(-max_coord, max_coord)) +
        ggplot2::theme_void() +
        ggplot2::theme(
          legend.position = settings$legend,
          legend.box = "horizontal",
          legend.title = ggplot2::element_text(size = 8, face = "bold"),
          legend.text  = ggplot2::element_text(size = 7.5),
          plot.margin  = ggplot2::margin(10, 10, 10, 10, "pt")
        ) +
        ggplot2::guides(
          fill = ggplot2::guide_legend(nrow = 1, override.aes = list(size = 4)),
          size = ggplot2::guide_legend(nrow = 1)
        )

      p
    }
  )
}
