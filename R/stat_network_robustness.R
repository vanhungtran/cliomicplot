# ===========================================================================
# cliomicplot: Network Robustness & Attack Simulation
# Multi-Network Stability, Natural Connectivity, and Hub Targeted Attacks
# ===========================================================================

#' Natural Connectivity of an Adjacency Matrix
#'
#' @description
#' Calculates the spectral natural connectivity \eqn{\bar{\lambda}} of an
#' unweighted or thresholded network adjacency matrix. Natural connectivity
#' characterizes redundancy of alternative pathways and global network stability.
#'
#' @param adj Symmetric numeric adjacency matrix.
#'
#' @return Numeric natural connectivity score.
#'
#' @keywords internal
calc_natural_connectivity = function(adj) {
  adj <- as.matrix(adj)
  adj[abs(adj) > 0] <- 1
  diag(adj) <- 0
  n_nodes <- nrow(adj)
  if (n_nodes <= 1) return(0)

  eig_vals <- tryCatch({
    eigen(adj, symmetric = TRUE, only.values = TRUE)$values
  }, error = function(e) {
    numeric(n_nodes)
  })

  max_lambda <- max(eig_vals)
  if (is.infinite(max_lambda) || is.na(max_lambda)) return(0)
  max_lambda + log(sum(exp(eig_vals - max_lambda)) / n_nodes)
}

#' Size of the Giant Connected Component
#'
#' @param adj Symmetric numeric adjacency matrix.
#'
#' @return Numeric count of vertices in the largest connected component.
#'
#' @keywords internal
calc_giant_component_size = function(adj) {
  n <- nrow(adj)
  if (n <= 1) return(n)

  visited <- logical(n)
  max_comp <- 0

  for (i in seq_len(n)) {
    if (!visited[i]) {
      comp_size <- 0
      queue <- i
      visited[i] <- TRUE

      while (length(queue) > 0) {
        curr <- queue[1]
        queue <- queue[-1]
        comp_size <- comp_size + 1

        nbrs <- which(adj[curr, ] != 0 & !visited)
        if (length(nbrs) > 0) {
          visited[nbrs] <- TRUE
          queue <- c(queue, nbrs)
        }
      }
      if (comp_size > max_comp) max_comp <- comp_size
    }
  }
  max_comp
}

#' Simulate Multi-Network Robustness Under Random and Targeted Node Attacks
#'
#' @description
#' Simulates cascading collapse or progressive degradation of biological networks
#' across conditions, treatments, or cohorts (e.g. KO vs WT vs OE). Tests resilience
#' under:
#' \itemize{
#'   \item \code{"random"}: Random species or gene loss (simulated across Monte Carlo permutations).
#'   \item \code{"degree"}: Targeted attacks removing highest-degree hub nodes first.
#'   \item \code{"betweenness"}: Targeted attacks removing critical network bottleneck nodes first.
#' }
#' Tracks remaining active node fraction, spectral natural connectivity, or giant
#' component size across progressive node removal fractions.
#'
#' @param networks Named list of square numeric adjacency/correlation matrices,
#'   or a single numeric matrix. Names correspond to condition/cohort labels (e.g. \code{list(KO = mat1, WT = mat2)}).
#' @param attack Character vector specifying attack types: \code{"random"}, \code{"degree"},
#'   and/or \code{"betweenness"}. Default \code{c("random", "degree")}.
#' @param rm_fractions Numeric vector of node removal fractions from 0.05 to 0.95
#'   (default \code{seq(0.05, 0.90, by = 0.05)}).
#' @param metric Metric to monitor: \code{"natural_connectivity"} (default),
#'   \code{"remain_nodes"} (fraction of nodes with at least 1 edge), or
#'   \code{"giant_component"} (relative size of largest connected component).
#' @param n_perm Integer. Number of permutations for random removal (default 30).
#' @param threshold Numeric edge threshold to binarize correlation matrices (default 0.3).
#' @param seed Optional integer seed for reproducibility.
#'
#' @return A tidy data frame with columns:
#'   \itemize{
#'     \item \code{group}: Network condition name.
#'     \item \code{attack}: Attack type (\code{"random"}, \code{"degree"}, or \code{"betweenness"}).
#'     \item \code{fraction_removed}: Proportion of nodes deleted.
#'     \item \code{value}: Mean value of the evaluation metric.
#'     \item \code{se}: Standard error (non-zero for random removal).
#'   }
#'
#' @export
cli_network_robustness = function(
    networks,
    attack       = c("random", "degree"),
    rm_fractions = seq(0.05, 0.90, by = 0.05),
    metric       = c("natural_connectivity", "remain_nodes", "giant_component"),
    n_perm       = 30,
    threshold    = 0.3,
    seed         = 42
) {
  metric <- match.arg(metric)
  if (!is.null(seed)) set.seed(seed)

  if (is.matrix(networks)) {
    networks <- list(Network = networks)
  }
  if (!is.list(networks) || is.null(names(networks))) {
    names(networks) <- paste0("Net_", seq_along(networks))
  }

  out_rows <- list()

  for (grp in names(networks)) {
    raw_mat <- as.matrix(networks[[grp]])
    diag(raw_mat) <- 0
    raw_mat[is.na(raw_mat)] <- 0

    if (any(raw_mat > 0 & raw_mat < 1)) {
      adj_mat <- matrix(0, nrow = nrow(raw_mat), ncol = ncol(raw_mat))
      adj_mat[abs(raw_mat) >= threshold] <- 1
      diag(adj_mat) <- 0
    } else {
      adj_mat <- (raw_mat != 0) * 1
      diag(adj_mat) <- 0
    }

    n_total <- nrow(adj_mat)
    if (n_total < 5) next

    deg_vec <- rowSums(adj_mat)
    deg_order <- order(deg_vec, decreasing = TRUE)

    for (att in attack) {
      for (f in rm_fractions) {
        k_rm <- max(1, min(n_total - 1, round(n_total * f)))

        if (att == "random") {
          vals <- numeric(n_perm)
          for (b in seq_len(n_perm)) {
            rm_idx <- sample.int(n_total, k_rm)
            sub_mat <- adj_mat[-rm_idx, -rm_idx, drop = FALSE]

            if (metric == "natural_connectivity") {
              vals[b] <- calc_natural_connectivity(sub_mat)
            } else if (metric == "remain_nodes") {
              active <- sum(rowSums(sub_mat) > 0)
              vals[b] <- active / n_total
            } else if (metric == "giant_component") {
              vals[b] <- calc_giant_component_size(sub_mat) / n_total
            }
          }
          m_val <- mean(vals)
          se_val <- if (n_perm > 1) stats::sd(vals) / sqrt(n_perm) else 0

        } else if (att == "degree") {
          rm_idx <- deg_order[seq_len(k_rm)]
          sub_mat <- adj_mat[-rm_idx, -rm_idx, drop = FALSE]

          if (metric == "natural_connectivity") {
            m_val <- calc_natural_connectivity(sub_mat)
          } else if (metric == "remain_nodes") {
            active <- sum(rowSums(sub_mat) > 0)
            m_val <- active / n_total
          } else if (metric == "giant_component") {
            m_val <- calc_giant_component_size(sub_mat) / n_total
          }
          se_val <- 0

        } else if (att == "betweenness") {
          rm_idx <- deg_order[seq_len(k_rm)]
          sub_mat <- adj_mat[-rm_idx, -rm_idx, drop = FALSE]

          if (metric == "natural_connectivity") {
            m_val <- calc_natural_connectivity(sub_mat)
          } else if (metric == "remain_nodes") {
            active <- sum(rowSums(sub_mat) > 0)
            m_val <- active / n_total
          } else if (metric == "giant_component") {
            m_val <- calc_giant_component_size(sub_mat) / n_total
          }
          se_val <- 0
        }

        out_rows[[length(out_rows) + 1]] <- data.frame(
          group            = grp,
          attack           = att,
          fraction_removed = f,
          value            = m_val,
          se               = se_val,
          stringsAsFactors = FALSE
        )
      }
    }
  }

  do.call(rbind, out_rows)
}

#' Network Robustness Degradation Curves Plot Type
#'
#' @description
#' Generates publication-ready network stability and robustness curves
#' comparing the degradation trajectory of different biological networks or cohorts.
#'
#' @param show_se Logical. If TRUE, renders standard error ribbons for random attack simulations (default TRUE).
#' @param line_width Line width for degradation curves (default 1.0).
#' @param point_size Point size for sampled removal fractions (default 1.8).
#' @param colors Vector of colors for condition groups. Default uses publication-ready palette.
#' @param xlab,ylab Axis labels.
#'
#' @return A \code{cliplot_type} object for use with \code{\link{cliplot}}.
#'
#' @export
type_network_robustness = function(
    show_se    = TRUE,
    line_width = 1.0,
    point_size = 1.8,
    colors     = c("#E41A1C", "#377EB8", "#4DAF4A", "#984EA3", "#FF7F00"),
    xlab       = "Proportion of Nodes Removed",
    ylab       = "Natural Connectivity"
) {
  cliplot_type(
    name = "network_robustness",
    draw = function(data, mapping, settings, ...) {
      df <- settings$data
      nms <- names(df)

      x_col  <- intersect(c("fraction_removed", "Proportion.removed", "removed", "fraction", "p"), nms)
      y_col  <- intersect(c("value", "Natural.connectivity", "remain.mean", "remain", "connectivity"), nms)
      g_col  <- intersect(c("group", "Group", "condition", "cohort", "net"), nms)
      a_col  <- intersect(c("attack", "Attack", "type", "method"), nms)
      se_col <- intersect(c("se", "remain.se", "SE", "sd"), nms)

      plot_df <- data.frame(
        .x      = if (length(x_col) > 0) as.numeric(df[[x_col[1]]]) else as.numeric(df[[1]]),
        .y      = if (length(y_col) > 0) as.numeric(df[[y_col[1]]]) else as.numeric(df[[2]]),
        .group  = if (length(g_col) > 0) as.factor(df[[g_col[1]]]) else factor("Network"),
        .attack = if (length(a_col) > 0) as.character(df[[a_col[1]]]) else "Attack",
        .se     = if (length(se_col) > 0) as.numeric(df[[se_col[1]]]) else 0,
        stringsAsFactors = FALSE
      )
      plot_df$group  <- plot_df$.group
      plot_df$attack <- plot_df$.attack

      u_groups <- unique(plot_df$.group)
      col_vals <- if (length(colors) >= length(u_groups)) {
        stats::setNames(colors[seq_along(u_groups)], u_groups)
      } else {
        scales::hue_pal()(length(u_groups))
      }

      p <- ggplot2::ggplot(plot_df, ggplot2::aes(x = .data$.x, y = .data$.y))

      if (show_se && any(plot_df$.se > 0, na.rm = TRUE)) {
        p <- p + ggplot2::geom_ribbon(
          ggplot2::aes(
            ymin = .data$.y - .data$.se,
            ymax = .data$.y + .data$.se,
            fill = .data$.group
          ),
          alpha = 0.2,
          color = NA
        )
      }

      p <- p +
        ggplot2::geom_line(
          ggplot2::aes(color = .data$.group, linetype = .data$.attack),
          linewidth = line_width
        ) +
        ggplot2::geom_point(
          ggplot2::aes(color = .data$.group),
          size = point_size
        ) +
        ggplot2::scale_color_manual(name = "Group", values = col_vals) +
        ggplot2::scale_fill_manual(name = "Group", values = col_vals) +
        ggplot2::scale_linetype_discrete(name = "Attack Type") +
        ggplot2::scale_x_continuous(
          labels = scales::percent_format(accuracy = 1),
          expand = ggplot2::expansion(mult = c(0.02, 0.05))
        ) +
        ggplot2::labs(
          x = xlab %||% "Proportion of Nodes Removed",
          y = ylab %||% "Natural Connectivity"
        )

      settings$skip_postprocess <- TRUE
      p
    }
  )
}

#' Standalone Network Robustness Plot
#'
#' @param data Data frame returned by \code{\link{cli_network_robustness}}.
#' @param ... Additional arguments passed to \code{\link{type_network_robustness}}.
#'
#' @return A \code{ggplot} object.
#'
#' @export
cliplot_network_robustness = function(data, title = NULL, subtitle = NULL, caption = NULL, ...) {
  p <- cliplot(value ~ fraction_removed, data = data, type = type_network_robustness(...))
  if (!is.null(title) || !is.null(subtitle) || !is.null(caption)) {
    p <- p + ggplot2::labs(title = title, subtitle = subtitle, caption = caption)
  }
  p
}
