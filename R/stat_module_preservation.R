# ===========================================================================
# cliomicplot: Module Preservation Analysis & Figure Archetype
# Multi-Omics Cross-Network Consensus & Preservation (WGCNA Framework)
# ===========================================================================

#' Evaluate Cross-Network Module Preservation Statistics
#'
#' @description
#' Computes module preservation statistics across reference and test networks
#' based on the Langfelder et al. (2011) framework. Evaluates whether module
#' density (average intramodular connectivity) and connectivity patterns
#' (hub gene structure) observed in a reference network are preserved in an
#' independent test network under a null permutation distribution.
#'
#' @param ref_data Numeric matrix or data frame of reference network profiles
#'   (samples in rows, features/genes/taxa in columns).
#' @param test_data Numeric matrix or data frame of test network profiles
#'   (samples in rows, features in columns). Must share column names with \code{ref_data}.
#' @param ref_modules Character or factor vector specifying module membership
#'   for each feature in \code{ref_data}.
#' @param test_modules Optional vector of module assignments in test data.
#' @param n_perm Integer. Number of permutation iterations to build the null distribution
#'   (default 100).
#' @param method Character correlation metric: \code{"pearson"} (default) or \code{"spearman"}.
#' @param seed Optional integer seed for reproducible permutations.
#'
#' @return A data frame containing module-level preservation metrics:
#'   \itemize{
#'     \item \code{module}: Module name.
#'     \item \code{size}: Number of member features.
#'     \item \code{z_density}: Standardized score for intramodular correlation density.
#'     \item \code{z_connectivity}: Standardized score for intramodular hub rank preservation.
#'     \item \code{z_summary}: Composite preservation score \eqn{(Z_{density} + Z_{connectivity}) / 2}.
#'     \item \code{p_density}: Permutation p-value for density preservation.
#'     \item \code{p_connectivity}: Permutation p-value for connectivity preservation.
#'     \item \code{preservation}: Categorical status: \code{"Strong"} (\eqn{Z \ge 10}),
#'           \code{"Moderate"} (\eqn{2 \le Z < 10}), or \code{"None"} (\eqn{Z < 2}).
#'   }
#'
#' @references
#' Langfelder P, Luo R, Oldham MC, Horvath S (2011). Is My Network Module Preserved
#' and Reproducible? \emph{PLoS Computational Biology}, 7(1): e1001057.
#'
#' @export
cli_module_preservation = function(
    ref_data,
    test_data,
    ref_modules,
    test_modules = NULL,
    n_perm       = 100,
    method       = c("pearson", "spearman"),
    seed         = 42
) {
  method <- match.arg(method)
  if (!is.null(seed)) set.seed(seed)

  ref_mat  <- as.matrix(ref_data)
  test_mat <- as.matrix(test_data)

  shared_cols <- intersect(colnames(ref_mat), colnames(test_mat))
  if (length(shared_cols) < 5) {
    if (ncol(ref_mat) == ncol(test_mat)) {
      shared_cols <- colnames(ref_mat) <- colnames(test_mat) <- paste0("F_", seq_len(ncol(ref_mat)))
    } else {
      stop("ref_data and test_data must share common feature names (columns).")
    }
  }

  ref_mat  <- ref_mat[, shared_cols, drop = FALSE]
  test_mat <- test_mat[, shared_cols, drop = FALSE]

  if (length(ref_modules) != length(shared_cols)) {
    if (!is.null(names(ref_modules)) && all(shared_cols %in% names(ref_modules))) {
      ref_modules <- ref_modules[shared_cols]
    } else {
      stop("Length of ref_modules must match the number of shared features.")
    }
  }

  # Reference and test correlation matrices (absolute values for co-expression)
  r_ref  <- abs(stats::cor(ref_mat, method = method, use = "pairwise.complete.obs"))
  r_test <- abs(stats::cor(test_mat, method = method, use = "pairwise.complete.obs"))
  diag(r_ref)  <- 0
  diag(r_test) <- 0
  r_ref[is.na(r_ref)]   <- 0
  r_test[is.na(r_test)] <- 0

  p_total <- length(shared_cols)
  u_mods  <- setdiff(unique(ref_modules), c("grey", "gray", "0", "None", NA))

  calc_module_stats <- function(idx, r_ref_mat, r_test_mat) {
    m_size <- length(idx)
    if (m_size < 3) return(c(density = NA_real_, conn = NA_real_))

    sub_ref  <- r_ref_mat[idx, idx, drop = FALSE]
    sub_test <- r_test_mat[idx, idx, drop = FALSE]

    dens_val <- sum(sub_test) / (m_size * (m_size - 1))

    k_ref  <- rowSums(sub_ref)
    k_test <- rowSums(sub_test)
    conn_val <- if (stats::sd(k_ref) > 1e-8 && stats::sd(k_test) > 1e-8) {
      stats::cor(k_ref, k_test)
    } else {
      0
    }
    c(density = dens_val, conn = conn_val)
  }

  res_list <- list()

  for (mod in u_mods) {
    mod_idx <- which(ref_modules == mod)
    m_size  <- length(mod_idx)
    if (m_size < 4) next

    obs_stats <- calc_module_stats(mod_idx, r_ref, r_test)

    null_dens <- numeric(n_perm)
    null_conn <- numeric(n_perm)

    for (b in seq_len(n_perm)) {
      perm_idx <- sample.int(p_total, m_size)
      sub_null <- r_test[perm_idx, perm_idx, drop = FALSE]

      null_dens[b] <- sum(sub_null) / (m_size * (m_size - 1))
      k_ref_mod    <- rowSums(r_ref[mod_idx, mod_idx, drop = FALSE])
      k_null_mod   <- rowSums(sub_null)

      null_conn[b] <- if (stats::sd(k_null_mod) > 1e-8 && stats::sd(k_ref_mod) > 1e-8) {
        stats::cor(k_ref_mod, k_null_mod)
      } else {
        0
      }
    }

    sd_dens <- stats::sd(null_dens)
    sd_conn <- stats::sd(null_conn)

    z_dens <- if (sd_dens > 1e-8) (obs_stats["density"] - mean(null_dens)) / sd_dens else 0
    z_conn <- if (sd_conn > 1e-8) (obs_stats["conn"] - mean(null_conn)) / sd_conn else 0
    z_sum  <- (z_dens + z_conn) / 2

    p_dens <- (sum(null_dens >= obs_stats["density"]) + 1) / (n_perm + 1)
    p_conn <- (sum(null_conn >= obs_stats["conn"]) + 1) / (n_perm + 1)

    status <- if (z_sum >= 10) "Strong" else if (z_sum >= 2) "Moderate" else "None"

    res_list[[mod]] <- data.frame(
      module         = mod,
      size           = m_size,
      z_density      = round(z_dens, 2),
      z_connectivity = round(z_conn, 2),
      z_summary      = round(z_sum, 2),
      p_density      = round(p_dens, 4),
      p_connectivity = round(p_conn, 4),
      preservation   = status,
      stringsAsFactors = FALSE
    )
  }

  do.call(rbind, res_list)
}

#' Cross-Network Module Preservation Plot Type
#'
#' @description
#' Generates publication-grade module preservation scatter plots displaying
#' composite \eqn{Z_{summary}} scores against module size. Highlights standard
#' WGCNA thresholds (\eqn{Z = 2} and \eqn{Z = 10}) separating non-preserved,
#' moderately preserved, and strongly preserved network modules.
#'
#' @param z_cutoff_low Numeric threshold for moderate preservation (default 2.0).
#' @param z_cutoff_high Numeric threshold for strong preservation (default 10.0).
#' @param show_labels Logical. If TRUE, renders module name labels next to points (default TRUE).
#' @param show_threshold_labels Logical. If TRUE, displays reference threshold tags (default TRUE).
#' @param point_size Numeric size of module points (default 3.5).
#' @param colors Named character vector of colors for preservation status categories:
#'   \code{"Strong"}, \code{"Moderate"}, and \code{"None"}.
#' @param label_size Numeric size for text labels (default 3.2).
#' @param xlab,ylab Axis labels.
#'
#' @return A \code{cliplot_type} object for use with \code{\link{cliplot}}.
#'
#' @export
type_module_preservation = function(
    z_cutoff_low          = 2.0,
    z_cutoff_high         = 10.0,
    show_labels           = TRUE,
    show_threshold_labels = TRUE,
    point_size            = 3.5,
    colors                = c(Strong = "#1B9E77", Moderate = "#2171B5", None = "#D95F02"),
    label_size            = 3.2,
    xlab                  = "Module Size (Number of Nodes)",
    ylab                  = expression(italic(Z)[summary]~"Preservation Score")
) {
  cliplot_type(
    name = "module_preservation",
    draw = function(data, mapping, settings, ...) {
      df <- settings$data
      nms <- names(df)

      size_col <- intersect(c("size", "Size", "n", "nodes", "module_size"), nms)
      z_col    <- intersect(c("z_summary", "Z_summary", "zsummary", "Zsummary", "z", "Z"), nms)
      mod_col  <- intersect(c("module", "Module", "mod", "name"), nms)

      x_vec <- if (length(size_col) > 0) df[[size_col[1]]] else df[[1]]
      y_vec <- if (length(z_col) > 0) df[[z_col[1]]] else df[[2]]
      m_vec <- if (length(mod_col) > 0) as.character(df[[mod_col[1]]]) else paste0("Mod_", seq_len(nrow(df)))

      status_vec <- if ("preservation" %in% nms) {
        as.character(df$preservation)
      } else {
        ifelse(y_vec >= z_cutoff_high, "Strong",
               ifelse(y_vec >= z_cutoff_low, "Moderate", "None"))
      }

      plot_df <- data.frame(
        .x            = as.numeric(x_vec),
        .y            = as.numeric(y_vec),
        .module       = m_vec,
        .preservation = factor(status_vec, levels = c("Strong", "Moderate", "None")),
        stringsAsFactors = FALSE
      )

      x_range <- range(plot_df$.x, na.rm = TRUE)
      y_max   <- max(c(plot_df$.y, z_cutoff_high * 1.3), na.rm = TRUE)
      y_min   <- min(c(plot_df$.y, -0.5), na.rm = TRUE)

      p <- ggplot2::ggplot(plot_df, ggplot2::aes(x = .data$.x, y = .data$.y)) +
        # Threshold lines
        ggplot2::geom_hline(
          yintercept = z_cutoff_high,
          linetype   = "dashed",
          color      = "#1B9E77",
          linewidth  = 0.6
        ) +
        ggplot2::geom_hline(
          yintercept = z_cutoff_low,
          linetype   = "dashed",
          color      = "#2171B5",
          linewidth  = 0.6
        ) +
        # Points
        ggplot2::geom_point(
          ggplot2::aes(color = .data$.preservation),
          size  = point_size,
          alpha = 0.9
        )

      # Optional threshold annotation labels
      if (show_threshold_labels) {
        label_df <- data.frame(
          x = rep(x_range[2], 2),
          y = c(z_cutoff_high, z_cutoff_low),
          label = c(paste0("Strong (Z > ", z_cutoff_high, ")"),
                    paste0("Moderate (Z > ", z_cutoff_low, ")")),
          color = c("#1B9E77", "#2171B5")
        )
        p <- p + ggplot2::geom_text(
          data        = label_df,
          mapping     = ggplot2::aes(x = .data$x, y = .data$y, label = .data$label),
          hjust       = 1.05,
          vjust       = -0.4,
          size        = 3.0,
          color       = label_df$color,
          fontface    = "bold",
          inherit.aes = FALSE
        )
      }

      # Optional module labels
      if (show_labels) {
        p <- p + ggplot2::geom_text(
          ggplot2::aes(label = .data$.module),
          vjust       = -0.8,
          size        = label_size,
          color       = "#2D3748",
          check_overlap = TRUE
        )
      }

      p <- p +
        ggplot2::scale_color_manual(
          name   = "Preservation",
          values = colors,
          drop   = FALSE
        ) +
        ggplot2::scale_y_continuous(
          limits = c(y_min, y_max),
          expand = ggplot2::expansion(mult = c(0.05, 0.1))
        ) +
        ggplot2::scale_x_continuous(
          expand = ggplot2::expansion(mult = c(0.08, 0.08))
        ) +
        ggplot2::labs(
          x = xlab %||% "Module Size (Number of Nodes)",
          y = ylab %||% expression(italic(Z)[summary]~"Preservation Score")
        )

      settings$skip_postprocess <- TRUE
      p
    }
  )
}

#' Standalone Module Preservation Figure
#'
#' @description
#' High-level wrapper to plot cross-network module preservation results.
#'
#' @param data Data frame returned by \code{\link{cli_module_preservation}} or containing
#'   size and z_summary columns.
#' @param ... Additional styling arguments passed to \code{\link{type_module_preservation}}.
#'
#' @return A \code{ggplot} object.
#'
#' @export
cliplot_module_preservation = function(data, title = NULL, subtitle = NULL, caption = NULL, ...) {
  p <- cliplot(z_summary ~ size, data = data, type = type_module_preservation(...))
  if (!is.null(title) || !is.null(subtitle) || !is.null(caption)) {
    p <- p + ggplot2::labs(title = title, subtitle = subtitle, caption = caption)
  }
  p
}
