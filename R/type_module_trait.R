# ===========================================================================
# cliomicplot: Module-Trait & Environmental Factor Correlation Heatmap
# Multi-Omics & Clinical Systems Biology Figure Archetype
# ===========================================================================

#' Module-Trait and Environmental Factor Correlation Heatmap
#'
#' @description
#' Creates publication-ready correlation heatmaps linking biological network modules
#' (e.g. WGCNA module eigengenes, microbial cluster abundances, metabolic modules)
#' with external environmental factors, clinical phenotypes, or physiological exposures
#' (e.g., pH, temperature, humidity, pollution, SCORAD, age, TEWL).
#' Computes and displays correlation coefficients (\eqn{r} or \eqn{\rho}), exact two-sided
#' p-values, Benjamini-Hochberg FDR adjustments, significance asterisks, and optional
#' hierarchical clustering for modules and environmental factors.
#'
#' @param method Correlation method: \code{"pearson"} (default) or \code{"spearman"}.
#' @param cor_col Column name for correlation coefficients in precomputed data
#'   (default auto-detected: \code{"cor"}, \code{"r"}, \code{"rho"}, \code{"estimate"}).
#' @param p_col Column name for p-values in precomputed data
#'   (default auto-detected: \code{"pvalue"}, \code{"p"}, \code{"pval"}, \code{"padj"}, \code{"qvalue"}).
#' @param show_labels Cell annotation format: \code{"both"} (\code{"0.42\n(p=0.01)"}, default),
#'   \code{"coef_stars"} (\code{"0.42***"}), \code{"coef"} (\code{"0.42"}), \code{"stars"} (\code{"***"}),
#'   or \code{"none"}.
#' @param sig_digits Number of decimal digits for correlation coefficient (default 2).
#' @param colors Diverging color gradient vector of length 3 (low, mid, high).
#'   Default \code{c("#2166AC", "#F7F7F7", "#B2182B")} (standard blue-white-red).
#' @param limits Numeric vector of length 2 for color scale limits (default \code{c(-1, 1)}).
#' @param cluster_modules Logical. If TRUE, reorders module rows using hierarchical clustering. Default FALSE.
#' @param cluster_env Logical. If TRUE, reorders environmental factor columns using hierarchical clustering. Default FALSE.
#' @param p_cutoff Optional significance threshold (e.g. \code{0.05}). If provided, cells with p-values
#'   exceeding this cutoff will have blanked text labels.
#' @param border_color Outline color for tiles (default \code{"#E2E8F0"}).
#' @param border_size Stroke width for tile borders (default \code{0.35}).
#' @param text_size Font size for cell annotations (default 3.0).
#' @param text_color Text color. If \code{"auto"} (default), selects high-contrast text color based on cell fill.
#' @param legend_title Title for color legend (default \code{expression(italic(r))}).
#' @param xlab,ylab Axis labels.
#'
#' @return A \code{cliplot_type} object for use with \code{\link{cliplot}}.
#'
#' @examples
#' \dontrun{
#' # Precomputed module-trait correlations
#' cor_df <- expand.grid(
#'   Module = c("M1_Inflammation", "M2_SkinBarrier", "M3_Microbiome"),
#'   Factor = c("Skin_pH", "Humidity", "SCORAD")
#' )
#' cor_df$cor <- c(0.45, -0.22, 0.65, -0.38, 0.52, -0.15, 0.72, 0.48, -0.31)
#' cor_df$pvalue <- c(0.001, 0.08, 1e-5, 0.005, 0.0001, 0.25, 1e-7, 0.0002, 0.01)
#'
#' cliplot(Module ~ Factor, data = cor_df, type = type_module_trait())
#' }
#'
#' @export
type_module_trait = function(
    method          = c("pearson", "spearman"),
    cor_col         = NULL,
    p_col           = NULL,
    show_labels     = c("both", "coef_stars", "coef", "stars", "none"),
    sig_digits      = 2,
    colors          = c("#2166AC", "#F7F7F7", "#B2182B"),
    limits          = c(-1, 1),
    cluster_modules = FALSE,
    cluster_env     = FALSE,
    p_cutoff        = NULL,
    border_color    = "#E2E8F0",
    border_size     = 0.35,
    text_size       = 3.0,
    text_color      = "auto",
    legend_title    = NULL,
    xlab            = NULL,
    ylab            = NULL
) {
  method      <- match.arg(method)
  show_labels <- match.arg(show_labels)

  cliplot_type(
    data = function(settings, ...) {
      df <- settings$data

      # Case 1: Direct numeric matrix passed
      if (is.matrix(df) || (is.data.frame(df) && all(vapply(df, is.numeric, logical(1))) && is.null(settings$y))) {
        r_mat <- as.matrix(df)
        u_mods <- rownames(r_mat) %||% paste0("Module_", seq_len(nrow(r_mat)))
        u_env  <- colnames(r_mat) %||% paste0("Factor_", seq_len(ncol(r_mat)))
        rownames(r_mat) <- u_mods
        colnames(r_mat) <- u_env

        trait_df <- expand.grid(.y = u_mods, .x = u_env, stringsAsFactors = FALSE)
        trait_df$.cor  <- as.vector(r_mat)
        trait_df$.pval <- rep(NA_real_, nrow(trait_df))
        trait_df$.padj <- rep(NA_real_, nrow(trait_df))
      } else {
        nms <- if (!is.null(df)) names(df) else character(0)

        # Detect candidate column names for module (.y) and environmental factor (.x)
        candidates_mod <- intersect(c("module", "Module", "ME", "cluster", "Cluster", "pathway", "Pathway"), nms)
        candidates_env <- intersect(c("factor", "Factor", "trait", "Trait", "env", "Env", "exposure", "Exposure", "phenotype"), nms)

        # Detect correlation and p-value columns in precomputed long data
        candidates_cor  <- intersect(c("cor", "r", "rho", "estimate", "coef", "beta", "correlation"), nms)
        candidates_pval <- intersect(c("pvalue", "p", "pval", "padj", "qvalue", "q", "FDR", "fdr"), nms)

        c_col <- cor_col %||% if (length(candidates_cor) > 0) candidates_cor[1] else NULL
        p_val_col <- p_col %||% if (length(candidates_pval) > 0) candidates_pval[1] else NULL

        is_precomputed_long <- !is.null(c_col) && c_col %in% nms

        if (is_precomputed_long) {
          # Long pre-computed table
          x_name <- if (is.character(settings$x) && length(settings$x) == 1 && settings$x %in% nms) {
            settings$x
          } else if (length(candidates_env) > 0) {
            candidates_env[1]
          } else {
            setdiff(nms, c(c_col, p_val_col, candidates_mod))[1] %||% nms[1]
          }

          y_name <- if (is.character(settings$y) && length(settings$y) == 1 && settings$y %in% nms) {
            settings$y
          } else if (length(candidates_mod) > 0) {
            candidates_mod[1]
          } else {
            setdiff(nms, c(c_col, p_val_col, x_name))[1] %||% nms[min(2, length(nms))]
          }

          x_vec <- if (!is.null(settings$x) && length(settings$x) == nrow(df)) as.character(settings$x) else as.character(df[[x_name]])
          y_vec <- if (!is.null(settings$y) && length(settings$y) == nrow(df)) as.character(settings$y) else as.character(df[[y_name]])

          cor_vals  <- as.numeric(df[[c_col]])
          pval_vals <- if (!is.null(p_val_col) && p_val_col %in% nms) as.numeric(df[[p_val_col]]) else rep(NA_real_, nrow(df))

          trait_df <- data.frame(
            .x    = x_vec,
            .y    = y_vec,
            .cor  = cor_vals,
            .pval = pval_vals,
            stringsAsFactors = FALSE
          )
          trait_df$.padj <- if (all(is.na(trait_df$.pval))) rep(NA_real_, nrow(df)) else stats::p.adjust(trait_df$.pval, method = "BH")
        } else {
          # Sample-level raw dataset: calculate correlations between module columns and factor columns
          # Select numeric columns
          num_cols <- names(df)[vapply(df, is.numeric, logical(1))]
          if (length(num_cols) < 2) {
            stop("type_module_trait requires at least two numeric variables or a precomputed correlation table", call. = FALSE)
          }

          mod_cols <- intersect(grep("ME|mod|clust", num_cols, ignore.case = TRUE, value = TRUE), num_cols)
          if (length(mod_cols) == 0) mod_cols <- num_cols[1:max(1, floor(length(num_cols) / 2))]
          env_cols <- setdiff(num_cols, mod_cols)
          if (length(env_cols) == 0) env_cols <- num_cols

          mod_mat <- as.matrix(df[, mod_cols, drop = FALSE])
          env_mat <- as.matrix(df[, env_cols, drop = FALSE])

          r_mat <- stats::cor(mod_mat, env_mat, use = "pairwise.complete.obs", method = method)
          p_mat <- matrix(NA_real_, nrow = nrow(r_mat), ncol = ncol(r_mat), dimnames = dimnames(r_mat))
          n_obs <- nrow(df)

          for (i in seq_len(nrow(r_mat))) {
            for (j in seq_len(ncol(r_mat))) {
              r <- r_mat[i, j]
              if (!is.na(r) && abs(r) < 1) {
                t_stat <- r * sqrt((n_obs - 2) / (1 - r^2))
                p_mat[i, j] <- 2 * stats::pt(-abs(t_stat), df = n_obs - 2)
              } else if (!is.na(r) && abs(r) >= 1) {
                p_mat[i, j] <- 0
              }
            }
          }

          trait_df <- expand.grid(.y = rownames(r_mat), .x = colnames(r_mat), stringsAsFactors = FALSE)
          trait_df$.cor  <- as.vector(r_mat)
          trait_df$.pval <- as.vector(p_mat)
          trait_df$.padj <- stats::p.adjust(trait_df$.pval, method = "BH")
        }
      }

      # Hierarchical clustering of modules (.y) and environmental factors (.x)
      u_x <- unique(trait_df$.x)
      u_y <- unique(trait_df$.y)

      if (isTRUE(cluster_modules) && length(u_y) >= 2 && length(u_x) >= 1) {
        wide_y <- stats::reshape(trait_df[, c(".x", ".y", ".cor")], timevar = ".x", idvar = ".y", direction = "wide")
        mat_y <- as.matrix(wide_y[, -1, drop = FALSE])
        mat_y[is.na(mat_y)] <- 0
        rownames(mat_y) <- wide_y$.y
        d_y <- stats::dist(mat_y)
        hc_y <- stats::hclust(d_y)
        u_y <- wide_y$.y[hc_y$order]
      }

      if (isTRUE(cluster_env) && length(u_x) >= 2 && length(u_y) >= 1) {
        wide_x <- stats::reshape(trait_df[, c(".x", ".y", ".cor")], timevar = ".y", idvar = ".x", direction = "wide")
        mat_x <- as.matrix(wide_x[, -1, drop = FALSE])
        mat_x[is.na(mat_x)] <- 0
        rownames(mat_x) <- wide_x$.x
        d_x <- stats::dist(mat_x)
        hc_x <- stats::hclust(d_x)
        u_x <- wide_x$.x[hc_x$order]
      }

      trait_df$.x <- factor(trait_df$.x, levels = u_x)
      trait_df$.y <- factor(trait_df$.y, levels = rev(u_y))

      # Cell label formatting
      lbls <- character(nrow(trait_df))
      for (i in seq_len(nrow(trait_df))) {
        r_val <- trait_df$.cor[i]
        p_val <- trait_df$.pval[i]

        if (is.na(r_val)) {
          lbls[i] <- ""
          next
        }

        # Apply p_cutoff filter if set
        if (!is.null(p_cutoff) && !is.na(p_val) && p_val > p_cutoff) {
          lbls[i] <- ""
          next
        }

        r_str <- sprintf(paste0("%.", sig_digits, "f"), r_val)
        stars <- if (!is.na(p_val)) {
          if (p_val < 0.001) "***" else if (p_val < 0.01) "**" else if (p_val < 0.05) "*" else ""
        } else {
          ""
        }

        txt <- switch(
          show_labels,
          both = {
            if (!is.na(p_val)) {
              p_str <- if (p_val < 0.001) "<0.001" else sprintf("%.3f", p_val)
              paste0(r_str, "\n(p=", p_str, ")")
            } else {
              r_str
            }
          },
          coef_stars = paste0(r_str, stars),
          coef       = r_str,
          stars      = stars,
          none       = ""
        )
        lbls[i] <- txt
      }
      trait_df$.label <- lbls

      # High-contrast automatic text color
      trait_df$.txt_col <- ifelse(abs(trait_df$.cor) > 0.52, "white", "grey20")

      settings$trait_df           <- trait_df
      settings$trait_colors       <- colors
      settings$trait_limits       <- limits
      settings$trait_border_color <- border_color
      settings$trait_border_size  <- border_size
      settings$trait_text_size    <- text_size
      settings$trait_text_color   <- text_color
      settings$trait_legend_title <- legend_title
      settings$trait_xlab         <- xlab
      settings$trait_ylab         <- ylab
    },

    draw = function(data, mapping, settings, ...) {
      df <- settings$trait_df
      if (is.null(df) || nrow(df) == 0) return(ggplot2::ggplot())

      leg_title <- settings$trait_legend_title %||% expression(italic(r))

      p <- ggplot2::ggplot(df, ggplot2::aes(x = .data[[".x"]], y = .data[[".y"]])) +
        ggplot2::geom_tile(
          ggplot2::aes(fill = .data[[".cor"]]),
          color = settings$trait_border_color,
          linewidth = settings$trait_border_size
        ) +
        ggplot2::scale_fill_gradient2(
          low      = settings$trait_colors[1],
          mid      = settings$trait_colors[2],
          high     = settings$trait_colors[3],
          midpoint = 0,
          limits   = settings$trait_limits,
          name     = leg_title
        ) +
        ggplot2::labs(
          x = settings$trait_xlab %||% settings$xlab %||% "",
          y = settings$trait_ylab %||% settings$ylab %||% ""
        ) +
        ggplot2::theme(
          axis.text.x = ggplot2::element_text(angle = 45, hjust = 1, vjust = 1),
          panel.grid = ggplot2::element_blank()
        )

      if (identical(settings$trait_text_color, "auto")) {
        p <- p + ggplot2::geom_text(
          ggplot2::aes(label = .data[[".label"]], color = .data[[".txt_col"]]),
          size = settings$trait_text_size,
          lineheight = 0.85,
          na.rm = TRUE
        ) +
        ggplot2::scale_color_identity()
      } else {
        p <- p + ggplot2::geom_text(
          ggplot2::aes(label = .data[[".label"]]),
          color = settings$trait_text_color,
          size = settings$trait_text_size,
          lineheight = 0.85,
          na.rm = TRUE
        )
      }

      p
    },
    name = "module_trait"
  )
}
