# ===========================================================================
# cliomicplot: Zi-Pi Keystone Ecological Role Plot
# Advanced Microbial Co-Occurrence Network Topologies & Keystone Identification
# Guimera & Amaral (Nature 2005), Deng et al. (Science 2012), Wen et al. (iMeta 2025)
# ===========================================================================

#' Compute Zi (Within-Module Connectivity) and Pi (Participation Coefficient)
#'
#' @description
#' Calculates node topological roles within a modular network: within-module
#' degree z-score (\eqn{Z_i}) and participation coefficient (\eqn{P_i}).
#'
#' @param adj_mat Numeric adjacency matrix (symmetric, unweighted or weighted > 0)
#'   with node identifiers in row and column names.
#' @param modules Named vector or data frame mapping node names to module IDs.
#' @param z_threshold Numeric cutoff for module hubs on \eqn{Z_i} (default 2.5).
#' @param p_threshold Numeric cutoff for connectors on \eqn{P_i} (default 0.62).
#'
#' @return A \code{data.frame} with columns:
#'   \itemize{
#'     \item \code{node}: Node identifier
#'     \item \code{module}: Module assignment
#'     \item \code{degree}: Total degree (number of connections)
#'     \item \code{within_degree}: Intra-module degree (\eqn{k_{i, s_i}})
#'     \item \code{Zi}: Within-module connectivity z-score
#'     \item \code{Pi}: Among-module participation coefficient
#'     \item \code{role}: Classified topological role (\code{"Network hub"}, \code{"Module hub"}, \code{"Connector"}, or \code{"Peripheral"})
#'   }
#'
#' @export
#' @examples
#' \dontrun{
#' adj <- matrix(sample(c(0, 1), 100, replace = TRUE, prob = c(0.8, 0.2)), 10, 10)
#' diag(adj) <- 0
#' adj <- (adj + t(adj)) / 2
#' rownames(adj) <- colnames(adj) <- paste0("OTU_", 1:10)
#' mods <- setNames(rep(c("M1", "M2"), each = 5), rownames(adj))
#' res <- calc_zipi(adj, mods)
#' }
calc_zipi = function(adj_mat, modules, z_threshold = 2.5, p_threshold = 0.62) {
  adj_mat <- as.matrix(adj_mat)
  nodes <- rownames(adj_mat)
  if (is.null(nodes)) {
    nodes <- paste0("Node_", seq_len(nrow(adj_mat)))
    rownames(adj_mat) <- colnames(adj_mat) <- nodes
  }

  if (is.data.frame(modules)) {
    if ("node" %in% names(modules) && "module" %in% names(modules)) {
      mod_vec <- stats::setNames(as.character(modules$module), as.character(modules$node))
    } else {
      mod_vec <- stats::setNames(as.character(modules[[2]]), as.character(modules[[1]]))
    }
  } else {
    mod_vec <- as.character(modules)
    if (is.null(names(mod_vec))) {
      names(mod_vec) <- nodes
    }
  }

  # Ensure all nodes have module assignments
  common_nodes <- intersect(nodes, names(mod_vec))
  if (length(common_nodes) < length(nodes)) {
    warning("Some nodes missing module assignments; assigning to 'Unassigned'")
    missing_nodes <- setdiff(nodes, common_nodes)
    mod_vec[missing_nodes] <- "Unassigned"
  }
  mod_vec <- mod_vec[nodes]

  bin_adj <- (adj_mat > 0) * 1
  diag(bin_adj) <- 0

  tot_deg <- rowSums(bin_adj)
  unique_mods <- unique(mod_vec)

  # Compute within-module degrees
  k_within <- numeric(length(nodes))
  names(k_within) <- nodes

  for (m in unique_mods) {
    m_nodes <- names(mod_vec)[mod_vec == m]
    if (length(m_nodes) == 1) {
      k_within[m_nodes] <- 0
    } else {
      sub_adj <- bin_adj[m_nodes, m_nodes, drop = FALSE]
      k_within[m_nodes] <- rowSums(sub_adj)
    }
  }

  # Compute Zi: (k_within - mean(k_within_m)) / sd(k_within_m)
  zi <- numeric(length(nodes))
  names(zi) <- nodes

  for (m in unique_mods) {
    m_nodes <- names(mod_vec)[mod_vec == m]
    kw_m <- k_within[m_nodes]
    sd_m <- stats::sd(kw_m)
    if (is.na(sd_m) || sd_m == 0) {
      zi[m_nodes] <- 0
    } else {
      zi[m_nodes] <- (kw_m - mean(kw_m)) / sd_m
    }
  }

  # Compute Pi: 1 - sum((k_is / k_i)^2)
  pi <- numeric(length(nodes))
  names(pi) <- nodes

  for (i in seq_along(nodes)) {
    nd <- nodes[i]
    k_i <- tot_deg[nd]
    if (k_i == 0) {
      pi[nd] <- 0
    } else {
      sum_sq <- 0
      for (m in unique_mods) {
        m_nodes <- names(mod_vec)[mod_vec == m]
        kis <- sum(bin_adj[nd, m_nodes])
        sum_sq <- sum_sq + (kis / k_i)^2
      }
      pi[nd] <- max(0, 1 - sum_sq)
    }
  }

  # Determine roles
  roles <- character(length(nodes))
  for (i in seq_along(nodes)) {
    z_val <- zi[i]
    p_val <- pi[i]
    if (z_val > z_threshold && p_val > p_threshold) {
      roles[i] <- "Network hub"
    } else if (z_val > z_threshold && p_val <= p_threshold) {
      roles[i] <- "Module hub"
    } else if (z_val <= z_threshold && p_val > p_threshold) {
      roles[i] <- "Connector"
    } else {
      roles[i] <- "Peripheral"
    }
  }

  role_factor <- factor(roles, levels = c("Peripheral", "Connector", "Module hub", "Network hub"))

  data.frame(
    node          = nodes,
    module        = mod_vec,
    degree        = tot_deg,
    within_degree = k_within,
    Zi            = round(zi, 3),
    Pi            = round(pi, 3),
    role          = role_factor,
    stringsAsFactors = FALSE
  )
}

#' Zi-Pi Keystone Ecological Role Plot Type
#'
#' @description
#' Creates a publication-grade Zi-Pi topological role plot for microbial co-occurrence
#' networks and multi-omics communities (Wen et al. iMeta 2025; Guimera & Amaral Nature 2005).
#' Partitions the \eqn{P_i} vs \eqn{Z_i} Cartesian plane into four ecological roles:
#' \itemize{
#'   \item \strong{Peripherals} (\eqn{Z_i \le 2.5, P_i \le 0.62}): Specialists with few inter-module links.
#'   \item \strong{Connectors} (\eqn{Z_i \le 2.5, P_i > 0.62}): Generalists linking distinct modules.
#'   \item \strong{Module hubs} (\eqn{Z_i > 2.5, P_i \le 0.62}): Highly connected specialists within their module.
#'   \item \strong{Network hubs} (\eqn{Z_i > 2.5, P_i > 0.62}): Super-generalists critical to network stability.
#' }
#'
#' @param z_threshold Numeric cutoff for module hubs on \eqn{Z_i} (default 2.5).
#' @param p_threshold Numeric cutoff for connectors on \eqn{P_i} (default 0.62).
#' @param show_quadrants Logical. If TRUE (default), displays soft shaded background tiles
#'   for each of the 4 topological quadrants with watermark role labels.
#' @param quadrant_alpha Numeric transparency for background quadrant tiles (default 0.05).
#' @param quadrant_labels Logical. If TRUE (default), renders small text badges naming each quadrant.
#' @param label_hubs Logical. If TRUE (default), labels keystone nodes (Network hubs, Module hubs, Connectors).
#' @param label_top Integer. Maximum number of keystone taxa to label (default 8).
#' @param label_size Numeric size for node text labels (default 3.0).
#' @param point_size Numeric base point size, or column name for continuous sizing (default 2.8).
#' @param point_alpha Numeric transparency for points (default 0.85).
#' @param show_rug Logical. If TRUE (default), adds marginal rugs along axes to indicate point densities.
#' @param show_summary Logical. If TRUE (default), adds an automated summary banner with percentage tallies.
#' @param role_colors Optional named vector of colors for roles.
#' @param adj_mat Optional adjacency matrix. If supplied, \eqn{Z_i} and \eqn{P_i} are computed automatically.
#' @param modules Optional module assignments when \code{adj_mat} is provided.
#'
#' @return A \code{cliplot_type} object for use with \code{\link{cliplot}}.
#'
#' @export
#' @examples
#' \dontrun{
#' # From pre-calculated Zi and Pi:
#' cliplot(Zi ~ Pi | Role, data = zipi_df, type = type_zipi())
#'
#' # From adjacency matrix:
#' cliplot(type = type_zipi(adj_mat = net_adj, modules = net_mods))
#' }
type_zipi = function(
    z_threshold       = 2.5,
    p_threshold       = 0.62,
    show_quadrants    = TRUE,
    quadrant_alpha    = 0.06,
    quadrant_labels   = TRUE,
    label_hubs        = TRUE,
    label_top         = 8,
    label_size        = 3.0,
    point_size        = 2.8,
    point_alpha       = 0.85,
    show_rug          = TRUE,
    show_summary      = TRUE,
    role_colors       = NULL,
    adj_mat           = NULL,
    modules           = NULL
) {
  default_role_colors <- c(
    "Peripheral"  = "#4A6FA5",   # Muted slate blue
    "Connector"   = "#00A087",   # Teal green
    "Module hub"  = "#E64B35",   # Crimson red
    "Network hub" = "#7E6148"    # Amber bronze
  )
  if (!is.null(role_colors)) {
    default_role_colors[names(role_colors)] <- role_colors
  }

  cliplot_type(
    name = "zipi",
    data = function(settings, ...) {
      # If raw adjacency matrix and modules are provided, compute Zi-Pi
      if (!is.null(adj_mat) && !is.null(modules)) {
        calc_df <- calc_zipi(adj_mat, modules, z_threshold = z_threshold, p_threshold = p_threshold)
        settings$data <- calc_df
        settings$x <- calc_df$Pi
        settings$y <- calc_df$Zi
        settings$by <- calc_df$role
        settings$x_var <- "Pi"
        settings$y_var <- "Zi"
        settings$by_var <- "role"
      } else {
        df <- settings$data
        # Auto-detect column names if not explicit
        if (!is.null(df)) {
          col_names <- tolower(names(df))
          if (is.null(settings$x_var) || settings$x_var == "x") {
            p_idx <- which(col_names %in% c("pi", "p", "participation", "p_value_raw"))
            if (length(p_idx) > 0) settings$x_var <- names(df)[p_idx[1]]
          }
          if (is.null(settings$y_var) || settings$y_var == "y") {
            z_idx <- which(col_names %in% c("zi", "z", "within_module", "z_score"))
            if (length(z_idx) > 0) settings$y_var <- names(df)[z_idx[1]]
          }

          if (!is.null(settings$x_var) && settings$x_var %in% names(df)) settings$x <- df[[settings$x_var]]
          if (!is.null(settings$y_var) && settings$y_var %in% names(df)) settings$y <- df[[settings$y_var]]

          # If role column exists, use it
          if (is.null(settings$by)) {
            r_idx <- which(col_names %in% c("role", "topological_role", "category"))
            if (length(r_idx) > 0) {
              settings$by <- factor(df[[r_idx[1]]])
              settings$by_var <- names(df)[r_idx[1]]
            } else if (!is.null(settings$x) && !is.null(settings$y)) {
              # Auto-classify roles
              roles <- ifelse(settings$y > z_threshold & settings$x > p_threshold, "Network hub",
                       ifelse(settings$y > z_threshold & settings$x <= p_threshold, "Module hub",
                       ifelse(settings$y <= z_threshold & settings$x > p_threshold, "Connector", "Peripheral")))
              settings$by <- factor(roles, levels = c("Peripheral", "Connector", "Module hub", "Network hub"))
              settings$by_var <- "role"
              df$role <- settings$by
              settings$data <- df
            }
          }
        }
      }

      settings$z_threshold     <- z_threshold
      settings$p_threshold     <- p_threshold
      settings$show_quadrants  <- show_quadrants
      settings$quadrant_alpha  <- quadrant_alpha
      settings$quadrant_labels <- quadrant_labels
      settings$label_hubs      <- label_hubs
      settings$label_top       <- label_top
      settings$label_size      <- label_size
      settings$point_size      <- point_size
      settings$point_alpha     <- point_alpha
      settings$show_rug        <- show_rug
      settings$show_summary    <- show_summary
      settings$role_colors     <- default_role_colors
      if (is.null(settings$legend)) settings$legend <- "top"
    },
    draw = function(data, mapping, settings, ...) {
      df <- settings$data
      if (is.null(df)) {
        df <- data.frame(
          Pi   = settings$x,
          Zi   = settings$y,
          role = settings$by %||% factor("Peripheral"),
          stringsAsFactors = FALSE
        )
      }

      x_col <- settings$x_var %||% "Pi"
      y_col <- settings$y_var %||% "Zi"
      by_col <- settings$by_var %||% "role"

      # Determine axis limits with padding
      max_p <- max(c(df[[x_col]], 1.0), na.rm = TRUE)
      min_p <- min(c(df[[x_col]], 0.0), na.rm = TRUE)
      x_lim <- c(min(0, min_p), max(1.0, max_p * 1.05))

      max_z <- max(c(df[[y_col]], 3.5), na.rm = TRUE)
      min_z <- min(c(df[[y_col]], -2.0), na.rm = TRUE)
      y_lim <- c(min(-2.5, min_z * 1.1), max(4.0, max_z * 1.15))

      p <- ggplot2::ggplot(df, ggplot2::aes(x = .data[[x_col]], y = .data[[y_col]]))

      # 1. Innovative Shaded Quadrant Background Tiles
      if (settings$show_quadrants) {
        quad_df <- data.frame(
          xmin = c(x_lim[1], settings$p_threshold, x_lim[1], settings$p_threshold),
          xmax = c(settings$p_threshold, x_lim[2], settings$p_threshold, x_lim[2]),
          ymin = c(y_lim[1], y_lim[1], settings$z_threshold, settings$z_threshold),
          ymax = c(settings$z_threshold, settings$z_threshold, y_lim[2], y_lim[2]),
          fill = c("#F4F6F9", "#E8F8F5", "#FEF9E7", "#FDEDEC")  # Soft peripheral, connector, module hub, network hub
        )
        p <- p + ggplot2::geom_rect(
          data = quad_df,
          ggplot2::aes(xmin = .data$xmin, xmax = .data$xmax, ymin = .data$ymin, ymax = .data$ymax),
          inherit.aes = FALSE,
          fill = quad_df$fill,
          alpha = settings$quadrant_alpha
        )
      }

      # 2. Reference Threshold Lines
      p <- p +
        ggplot2::geom_vline(
          xintercept = settings$p_threshold,
          linetype = "dashed",
          color = "#8A92A0",
          linewidth = 0.4
        ) +
        ggplot2::geom_hline(
          yintercept = settings$z_threshold,
          linetype = "dashed",
          color = "#8A92A0",
          linewidth = 0.4
        )

      # 3. Watermark Quadrant Labels
      if (settings$quadrant_labels) {
        label_df <- data.frame(
          x = c(
            x_lim[1] + 0.03 * diff(x_lim),
            x_lim[2] - 0.03 * diff(x_lim),
            x_lim[1] + 0.03 * diff(x_lim),
            x_lim[2] - 0.03 * diff(x_lim)
          ),
          y = c(
            y_lim[1] + 0.04 * diff(y_lim),
            y_lim[1] + 0.04 * diff(y_lim),
            y_lim[2] - 0.04 * diff(y_lim),
            y_lim[2] - 0.04 * diff(y_lim)
          ),
          label = c(
            "Peripherals (Specialists)",
            "Connectors (Generalists)",
            "Module Hubs",
            "Network Hubs (Super-generalists)"
          ),
          hjust = c(0, 1, 0, 1),
          color = c("#7F8C8D", "#16A085", "#D35400", "#C0392B")
        )
        p <- p + ggplot2::geom_text(
          data = label_df,
          ggplot2::aes(x = .data$x, y = .data$y, label = .data$label, hjust = .data$hjust),
          inherit.aes = FALSE,
          size = 2.8,
          fontface = "bold.italic",
          color = label_df$color,
          alpha = 0.75
        )
      }

      # 4. Optional Marginal Density Rugs
      if (settings$show_rug) {
        p <- p + ggplot2::geom_rug(
          ggplot2::aes(color = .data[[by_col]]),
          alpha = 0.4,
          length = ggplot2::unit(2, "mm"),
          linewidth = 0.35,
          show.legend = FALSE
        )
      }

      # 5. Scatter Points
      if (by_col %in% names(df)) {
        p <- p + ggplot2::geom_point(
          ggplot2::aes(color = .data[[by_col]], fill = .data[[by_col]]),
          size  = settings$point_size,
          alpha = settings$point_alpha,
          shape = 21,
          stroke = 0.3
        ) +
          ggplot2::scale_color_manual(values = settings$role_colors, name = "Topological Role: ") +
          ggplot2::scale_fill_manual(values = settings$role_colors, name = "Topological Role: ")
      } else {
        p <- p + ggplot2::geom_point(
          color = "#3B4992",
          fill  = "#4DBBD5",
          size  = settings$point_size,
          alpha = settings$point_alpha,
          shape = 21,
          stroke = 0.3
        )
      }

      # 6. Keystone Hub Text Callouts
      if (settings$label_hubs) {
        node_col <- NULL
        for (cand in c("node", "taxa", "taxon", "genus", "species", "name", "id")) {
          if (cand %in% tolower(names(df))) {
            node_col <- names(df)[which(tolower(names(df)) == cand)[1]]
            break
          }
        }

        if (!is.null(node_col)) {
          # Prioritize Network hubs > Module hubs > Connectors
          keystones <- df[df[[y_col]] > settings$z_threshold | df[[x_col]] > settings$p_threshold, , drop = FALSE]
          if (nrow(keystones) > 0) {
            keystones$priority <- ifelse(keystones[[y_col]] > settings$z_threshold & keystones[[x_col]] > settings$p_threshold, 3,
                                  ifelse(keystones[[y_col]] > settings$z_threshold, 2, 1))
            keystones <- keystones[order(-keystones$priority, -keystones[[y_col]], -keystones[[x_col]]), , drop = FALSE]
            label_sub <- utils::head(keystones, settings$label_top)

            if (requireNamespace("ggrepel", quietly = TRUE)) {
              p <- p + ggrepel::geom_text_repel(
                data = label_sub,
                ggplot2::aes(x = .data[[x_col]], y = .data[[y_col]], label = .data[[node_col]]),
                size = settings$label_size,
                fontface = "bold",
                color = "#2C3E50",
                bg.color = "white",
                bg.r = 0.12,
                box.padding = 0.25,
                point.padding = 0.2,
                max.overlaps = 50,
                segment.size = 0.25,
                segment.color = "#7F8C8D",
                show.legend = FALSE
              )
            } else {
              p <- p + ggplot2::geom_text(
                data = label_sub,
                ggplot2::aes(x = .data[[x_col]], y = .data[[y_col]], label = .data[[node_col]]),
                size = settings$label_size,
                fontface = "bold",
                color = "#2C3E50",
                vjust = -0.6,
                show.legend = FALSE
              )
            }
          }
        }
      }

      # 7. Axis Scales and Labels
      p <- p +
        ggplot2::scale_x_continuous(
          limits = x_lim,
          breaks = seq(0, 1.0, by = 0.2),
          expand = ggplot2::expansion(mult = c(0.02, 0.02))
        ) +
        ggplot2::scale_y_continuous(
          limits = y_lim,
          expand = ggplot2::expansion(mult = c(0.04, 0.04))
        ) +
        ggplot2::labs(
          x = "Participation Coefficient (Among-Module Connectivity, Pi)",
          y = "Within-Module Connectivity (Degree Z-score, Zi)"
        ) +
        ggplot2::guides(
          color = ggplot2::guide_legend(nrow = 1, byrow = TRUE, override.aes = list(size = 3.5)),
          fill  = ggplot2::guide_legend(nrow = 1, byrow = TRUE)
        )

      # 8. Automated Role Summary Banner in Caption if enabled
      if (settings$show_summary && by_col %in% names(df)) {
        tbl <- table(df[[by_col]])
        tot <- sum(tbl)
        pct <- round(100 * tbl / tot, 1)
        summary_str <- paste0(
          "Peripherals: ", pct["Peripheral"] %||% "0", "% (n=", tbl["Peripheral"] %||% "0", ") | ",
          "Connectors: ", pct["Connector"] %||% "0", "% (n=", tbl["Connector"] %||% "0", ") | ",
          "Module Hubs: ", pct["Module hub"] %||% "0", "% (n=", tbl["Module hub"] %||% "0", ") | ",
          "Network Hubs: ", pct["Network hub"] %||% "0", "% (n=", tbl["Network hub"] %||% "0", ")"
        )
        p <- p + ggplot2::labs(caption = summary_str)
      }

      p
    }
  )
}
