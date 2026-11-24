# ===========================================================================
# cliomicplot: Automated Calculation of Empirical Omics-to-Environment Links
# Empirical Statistical Linkage Engine for cliplot_env_network
# ===========================================================================

#' Automated Calculation of Empirical Omics-to-Environment Links
#'
#' @description
#' Calculates empirical statistical linkages connecting omics features or network modules
#' with external clinical or environmental indicators. The resulting data frame
#' directly feeds into \code{\link{cliplot_env_network}} via its \code{links} argument.
#' Computes module eigengenes (or centroid abundance profiles), runs bivariate
#' correlation tests (Spearman or Pearson) or Mantel dissimilarity associations,
#' applies Benjamini-Hochberg False Discovery Rate (FDR) adjustments, and selects
#' statistically significant or top-ranked linkages.
#'
#' @param comm_data Numeric matrix or data frame of omics/microbiome abundance
#'   (samples in rows, features in columns).
#' @param env_data Numeric matrix, data frame, or named list of environmental/clinical
#'   indicators (samples in rows, indicators in columns). If a named list of 1 to 4
#'   matrices/data frames is supplied, each domain is processed with its domain tag.
#' @param modules Optional named character or factor vector mapping feature IDs to
#'   module names (e.g. \code{c("Taxon_1" = "Mod_1", "Taxon_2" = "Mod_1")}).
#'   If NULL, features are grouped into modules automatically using hierarchical clustering,
#'   or individual features are tested directly.
#' @param method Correlation metric: \code{"spearman"} (default, robust to outliers)
#'   or \code{"pearson"}.
#' @param fdr_cutoff Numeric significance threshold for Benjamini-Hochberg adjusted
#'   p-values (default 0.05). If NULL or if no links meet the threshold, the top
#'   \code{top_n} links are retained.
#' @param top_n Optional integer specifying the maximum number of links to retain per
#'   module or environmental domain (default 5).
#' @param min_abs_r Numeric minimum absolute correlation coefficient \eqn{|r|}
#'   required to retain a link (default 0.25).
#' @param module_summary Summary method for features within a module: \code{"eigengene"}
#'   (first principal component, default) or \code{"mean"} (standardized mean abundance).
#'
#' @return A data frame containing formatted link linkages with columns:
#'   \item{env}{Name of the environmental or clinical indicator variable}
#'   \item{module}{Name of the target network module}
#'   \item{r}{Bivariate correlation coefficient}
#'   \item{p}{Raw asymptotic two-sided p-value}
#'   \item{fdr}{Benjamini-Hochberg adjusted p-value (q-value)}
#'   \item{log10p}{Negative log10 p-value (\eqn{-\log_{10}(P)})}
#'   \item{sign}{Direction of association: \code{"Positive"} or \code{"Negative"}}
#'   \item{domain}{Domain name if \code{env_data} was supplied as a named list}
#'
#' @examples
#' set.seed(42)
#' comm <- matrix(rpois(50 * 30, lambda = 15), nrow = 50, ncol = 30)
#' colnames(comm) <- paste0("Taxon_", 1:30)
#' env <- data.frame(
#'   pH = rnorm(50, 6.5, 0.8),
#'   Moisture = rnorm(50, 45, 10),
#'   Temp = rnorm(50, 22, 4)
#' )
#' mods <- setNames(paste0("Mod_", rep(1:3, each = 10)), colnames(comm))
#' links <- cli_calc_env_links(comm, env, modules = mods, method = "spearman")
#' head(links)
#'
#' @export
cli_calc_env_links = function(
    comm_data,
    env_data,
    modules        = NULL,
    method         = c("spearman", "pearson"),
    fdr_cutoff     = 0.05,
    top_n          = 5,
    min_abs_r      = 0.20,
    module_summary = c("eigengene", "mean")
) {
  method <- match.arg(method)
  module_summary <- match.arg(module_summary)

  comm_mat <- as.matrix(comm_data)
  if (nrow(comm_mat) < 3) {
    stop("comm_data must have at least 3 samples (rows).", call. = FALSE)
  }

  # Harmonize env_data
  env_list <- list()
  if (is.list(env_data) && !is.data.frame(env_data)) {
    for (d_name in names(env_data)) {
      d_mat <- as.matrix(env_data[[d_name]])
      if (nrow(d_mat) == nrow(comm_mat)) {
        env_list[[d_name]] <- d_mat
      }
    }
    if (length(env_list) == 0) {
      stop("Sample counts in env_data list elements do not match comm_data.", call. = FALSE)
    }
  } else {
    env_mat <- as.matrix(env_data)
    if (nrow(env_mat) != nrow(comm_mat)) {
      stop("Row count (samples) in env_data must match comm_data.", call. = FALSE)
    }
    env_list[["External_Domain"]] <- env_mat
  }

  # Build module profiles
  if (is.null(modules)) {
    # Auto-partition features into modules via hierarchical clustering if > 5 features
    p_feat <- ncol(comm_mat)
    if (p_feat >= 6) {
      d_feat <- stats::dist(t(scale(comm_mat)))
      hc <- stats::hclust(d_feat, method = "average")
      k_mods <- min(7, max(3, round(p_feat / 6)))
      cl <- stats::cutree(hc, k = k_mods)
      modules <- stats::setNames(paste0("Mod_", cl), colnames(comm_mat))
    } else {
      modules <- stats::setNames(paste0("Mod_", seq_len(p_feat)), colnames(comm_mat))
    }
  }

  u_mods <- unique(as.character(modules))
  n_samples <- nrow(comm_mat)
  mod_profiles <- matrix(0, nrow = n_samples, ncol = length(u_mods))
  colnames(mod_profiles) <- u_mods

  for (m in u_mods) {
    feat_in_m <- names(modules)[modules == m]
    if (length(feat_in_m) == 0) feat_in_m <- colnames(comm_mat)[which(modules == m)]
    feat_in_m <- intersect(feat_in_m, colnames(comm_mat))

    if (length(feat_in_m) == 0) {
      mod_profiles[, m] <- 0
      next
    }

    sub_m <- comm_mat[, feat_in_m, drop = FALSE]

    if (module_summary == "eigengene" && ncol(sub_m) >= 2) {
      # First principal component (module eigengene)
      pc_res <- tryCatch({
        pca <- stats::prcomp(sub_m, center = TRUE, scale. = TRUE)
        pc1 <- pca$x[, 1]
        # Align sign with average standardized expression
        avg_expr <- rowMeans(scale(sub_m), na.rm = TRUE)
        if (stats::cor(pc1, avg_expr, use = "complete.obs") < 0) pc1 <- -pc1
        pc1
      }, error = function(e) {
        rowMeans(scale(sub_m), na.rm = TRUE)
      })
      mod_profiles[, m] <- pc_res
    } else {
      # Standardized mean abundance profile
      sc_m <- scale(sub_m)
      mod_profiles[, m] <- rowMeans(sc_m, na.rm = TRUE)
    }
  }

  # Compute associations across all modules and all environmental indicators
  res_list <- list()

  for (d_i in seq_along(env_list)) {
    d_name <- names(env_list)[d_i]
    d_mat <- env_list[[d_i]]
    var_names <- if (!is.null(colnames(d_mat))) colnames(d_mat) else paste0("Var_", seq_len(ncol(d_mat)))

    for (v_j in seq_len(ncol(d_mat))) {
      v_name <- var_names[v_j]
      y_vec <- d_mat[, v_j]

      if (stats::sd(y_vec, na.rm = TRUE) == 0 || all(is.na(y_vec))) next

      for (m in u_mods) {
        x_vec <- mod_profiles[, m]
        if (stats::sd(x_vec, na.rm = TRUE) == 0 || all(is.na(x_vec))) next

        ok <- stats::complete.cases(x_vec, y_vec)
        if (sum(ok) < 4) next

        test <- tryCatch({
          stats::cor.test(x_vec[ok], y_vec[ok], method = method)
        }, error = function(e) NULL)

        if (!is.null(test) && !is.na(test$estimate)) {
          r_val <- as.numeric(test$estimate)
          p_val <- as.numeric(test$p.value)
          res_list[[length(res_list) + 1]] <- data.frame(
            env     = v_name,
            module  = m,
            r       = r_val,
            p       = p_val,
            domain  = d_name,
            stringsAsFactors = FALSE
          )
        }
      }
    }
  }

  if (length(res_list) == 0) {
    warning("No valid correlations could be computed between comm_data and env_data.", call. = FALSE)
    return(data.frame(
      env = character(0), module = character(0), r = numeric(0),
      p = numeric(0), fdr = numeric(0), log10p = numeric(0),
      sign = character(0), domain = character(0), stringsAsFactors = FALSE
    ))
  }

  raw_df <- do.call(rbind, res_list)
  raw_df$fdr <- stats::p.adjust(raw_df$p, method = "BH")
  raw_df$log10p <- -log10(pmax(raw_df$p, 1e-12))
  raw_df$sign <- ifelse(raw_df$r > 0, "Positive", "Negative")

  # Filtering
  filtered_df <- raw_df[abs(raw_df$r) >= min_abs_r, ]

  if (!is.null(fdr_cutoff)) {
    sig_df <- filtered_df[filtered_df$fdr <= fdr_cutoff, ]
    # Fallback to top_n if FDR is too stringent
    if (nrow(sig_df) < max(3, top_n)) {
      ranked <- filtered_df[order(filtered_df$p), ]
      n_keep <- min(nrow(ranked), max(3, top_n * length(env_list)))
      filtered_df <- ranked[seq_len(n_keep), ]
    } else {
      filtered_df <- sig_df
    }
  }

  if (!is.null(top_n) && nrow(filtered_df) > 0) {
    # Keep top_n per domain
    parts <- split(filtered_df, filtered_df$domain)
    kept_list <- lapply(parts, function(sub_d) {
      ord <- order(sub_d$p)
      sub_d[ord[seq_len(min(top_n, nrow(sub_d)))], ]
    })
    filtered_df <- do.call(rbind, kept_list)
  }

  # Order by significance
  filtered_df <- filtered_df[order(filtered_df$p), ]
  rownames(filtered_df) <- NULL
  filtered_df
}
