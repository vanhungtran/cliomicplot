# ===========================================================================
# cliomicplot: Mantel and Partial Mantel Association Testing
# Matrix & Subnetwork Associations with Environmental Variables
# ===========================================================================

#' Compute Dissimilarity / Distance Matrix
#'
#' @param mat Numeric matrix (samples in rows, features in columns).
#' @param method Distance metric: \code{"bray"}, \code{"jaccard"}, \code{"euclidean"}, or \code{"manhattan"}.
#'
#' @return A \code{dist} object.
#'
#' @keywords internal
calc_dist_matrix = function(mat, method = c("bray", "jaccard", "euclidean", "manhattan")) {
  method <- match.arg(method)
  mat <- as.matrix(mat)

  # Shift negative values for abundance-based metrics
  if (method == "bray" && any(mat < 0, na.rm = TRUE)) {
    mat <- mat - min(mat, na.rm = TRUE)
  }

  if (method %in% c("bray", "jaccard") && requireNamespace("vegan", quietly = TRUE)) {
    return(vegan::vegdist(mat, method = method))
  }

  if (method == "bray") {
    # Self-contained Bray-Curtis distance
    n <- nrow(mat)
    d_mat <- matrix(0, n, n)
    for (i in seq_len(n - 1)) {
      for (j in seq(i + 1, n)) {
        s_diff <- sum(abs(mat[i, ] - mat[j, ]))
        s_sum  <- sum(mat[i, ] + mat[j, ])
        d_mat[i, j] <- d_mat[j, i] <- if (s_sum > 0) s_diff / s_sum else 0
      }
    }
    rownames(d_mat) <- colnames(d_mat) <- rownames(mat)
    return(stats::as.dist(d_mat))
  } else if (method == "jaccard") {
    bin_mat <- (mat > 0) * 1
    return(stats::dist(bin_mat, method = "binary"))
  } else {
    return(stats::dist(mat, method = method))
  }
}

#' Mantel and Partial Mantel Test for Omics-Environment Associations
#'
#' @description
#' Evaluates association between biological community or module distance matrices
#' and environmental factor distance matrices using matrix permutations.
#' Supports partial Mantel tests to control for confounders such as geographic distance,
#' sample collection batch, sequencing depth, or subject age.
#'
#' @param comm_data Numeric matrix or data frame of community abundances, module profiles,
#'   or an existing \code{dist} object (samples in rows, features in columns).
#' @param env_data Numeric matrix or data frame of environmental variables, clinical traits,
#'   or an existing \code{dist} object (samples in rows, variables in columns).
#' @param dist_comm Dissimilarity metric for community data: \code{"bray"} (default),
#'   \code{"jaccard"}, \code{"euclidean"}, or \code{"manhattan"}.
#' @param dist_env Dissimilarity metric for environmental factors: \code{"euclidean"} (default)
#'   or \code{"manhattan"}.
#' @param method Correlation method between distance vectors: \code{"pearson"} (default) or \code{"spearman"}.
#' @param n_perm Integer. Number of permutation iterations to evaluate statistical significance (default 999).
#' @param partial_data Optional matrix, data frame, or \code{dist} object of confounding covariates
#'   to condition upon for partial Mantel testing.
#' @param by_variable Logical. If TRUE (default), tests each environmental variable individually
#'   in addition to the overall multivariate environmental matrix.
#' @param seed Optional integer seed for reproducible permutations.
#'
#' @return A data frame containing:
#'   \itemize{
#'     \item \code{variable}: Environmental factor name (or \code{"All_Variables"}).
#'     \item \code{statistic}: Mantel correlation coefficient \eqn{r}.
#'     \item \code{pvalue}: Two-sided permutation p-value.
#'     \item \code{padj}: Benjamini-Hochberg FDR-adjusted p-value.
#'     \item \code{signif}: Significance asterisks (* p < 0.05, ** p < 0.01, *** p < 0.001).
#'     \item \code{method}: Test designation (\code{"Mantel"} or \code{"Partial_Mantel"}).
#'   }
#'
#' @references
#' Mantel N (1967). The detection of disease clustering and a generalized regression approach.
#' \emph{Cancer Research}, 27(2): 209-220.
#'
#' @export
cli_mantel = function(
    comm_data,
    env_data,
    dist_comm       = c("bray", "jaccard", "euclidean", "manhattan"),
    dist_env        = c("euclidean", "manhattan"),
    method          = c("pearson", "spearman"),
    n_perm          = 999,
    partial_data    = NULL,
    by_variable     = TRUE,
    seed            = 42
) {
  dist_comm <- match.arg(dist_comm)
  dist_env  <- match.arg(dist_env)
  method    <- match.arg(method)
  if (!is.null(seed)) set.seed(seed)

  # Prepare community distance matrix
  d_comm <- if (inherits(comm_data, "dist")) {
    comm_data
  } else {
    calc_dist_matrix(comm_data, method = dist_comm)
  }
  n_samp <- attr(d_comm, "Size")
  mat_comm <- as.matrix(d_comm)
  diag(mat_comm) <- 0

  # Partial covariate distance matrix if provided
  d_part <- NULL
  if (!is.null(partial_data)) {
    d_part <- if (inherits(partial_data, "dist")) {
      partial_data
    } else {
      calc_dist_matrix(partial_data, method = "euclidean")
    }
  }

  idx_tri <- which(upper.tri(mat_comm))
  v_comm  <- mat_comm[idx_tri]

  v_part <- if (!is.null(d_part)) as.matrix(d_part)[idx_tri] else NULL

  run_single_mantel <- function(d_e, var_name) {
    mat_e <- as.matrix(d_e)
    diag(mat_e) <- 0
    v_env <- mat_e[idx_tri]

    if (stats::sd(v_env) < 1e-8 || stats::sd(v_comm) < 1e-8) {
      return(data.frame(
        variable  = var_name,
        statistic = 0,
        pvalue    = 1,
        method    = if (is.null(v_part)) "Mantel" else "Partial_Mantel",
        stringsAsFactors = FALSE
      ))
    }

    if (is.null(v_part)) {
      # Standard Mantel test
      r_obs <- stats::cor(v_comm, v_env, method = method)
      perm_r <- numeric(n_perm)

      for (b in seq_len(n_perm)) {
        p_idx <- sample.int(n_samp)
        perm_mat <- mat_comm[p_idx, p_idx]
        perm_r[b] <- stats::cor(perm_mat[idx_tri], v_env, method = method)
      }
      p_val <- (sum(abs(perm_r) >= abs(r_obs)) + 1) / (n_perm + 1)
      t_method <- "Mantel"

    } else {
      # Partial Mantel test
      r_xy <- stats::cor(v_comm, v_env, method = method)
      r_xz <- stats::cor(v_comm, v_part, method = method)
      r_yz <- stats::cor(v_env, v_part, method = method)

      denom <- sqrt(max(0, (1 - r_xz^2) * (1 - r_yz^2)))
      r_obs <- if (denom > 1e-8) (r_xy - r_xz * r_yz) / denom else 0

      perm_r <- numeric(n_perm)
      for (b in seq_len(n_perm)) {
        p_idx <- sample.int(n_samp)
        perm_mat <- mat_comm[p_idx, p_idx]
        p_comm <- perm_mat[idx_tri]

        p_r_xy <- stats::cor(p_comm, v_env, method = method)
        p_r_xz <- stats::cor(p_comm, v_part, method = method)
        p_denom <- sqrt(max(0, (1 - p_r_xz^2) * (1 - r_yz^2)))
        perm_r[b] <- if (p_denom > 1e-8) (p_r_xy - p_r_xz * r_yz) / p_denom else 0
      }
      p_val <- (sum(abs(perm_r) >= abs(r_obs)) + 1) / (n_perm + 1)
      t_method <- "Partial_Mantel"
    }

    data.frame(
      variable  = var_name,
      statistic = round(r_obs, 4),
      pvalue    = round(p_val, 4),
      method    = t_method,
      stringsAsFactors = FALSE
    )
  }

  res_rows <- list()

  if (inherits(env_data, "dist")) {
    res_rows[[1]] <- run_single_mantel(env_data, "Overall_Env")
  } else {
    env_df <- as.data.frame(env_data)

    # Standardize numeric environmental factors
    num_cols <- vapply(env_df, is.numeric, logical(1))
    env_num  <- scale(as.matrix(env_df[, num_cols, drop = FALSE]))

    if (by_variable && ncol(env_num) > 1) {
      for (col_nm in colnames(env_num)) {
        d_single <- stats::dist(env_num[, col_nm, drop = FALSE], method = dist_env)
        res_rows[[length(res_rows) + 1]] <- run_single_mantel(d_single, col_nm)
      }
    }

    # Overall multivariate environmental distance
    d_multi <- stats::dist(env_num, method = dist_env)
    res_rows[[length(res_rows) + 1]] <- run_single_mantel(d_multi, "All_Variables")
  }

  out <- do.call(rbind, res_rows)
  out$padj <- round(stats::p.adjust(out$pvalue, method = "BH"), 4)
  out$signif <- ifelse(out$pvalue < 0.001, "***",
                ifelse(out$pvalue < 0.01, "**",
                ifelse(out$pvalue < 0.05, "*", "")))

  out
}

#' Partial Correlation with Covariate Adjustment
#'
#' @description
#' Computes first-order or higher-order partial correlation coefficients between
#' two variables or feature sets after partialling out linear confounding
#' effects of specified covariates (e.g., age, sex, library size, batch).
#'
#' @param x Numeric vector or matrix of target variables.
#' @param y Numeric vector or matrix of response variables.
#' @param z Numeric vector, matrix, or data frame of confounding covariates.
#' @param method Correlation metric: \code{"pearson"} (default) or \code{"spearman"}.
#'
#' @return A data frame containing estimate, t-statistic, p-value, and FDR padj.
#'
#' @export
cli_partial_cor = function(
    x,
    y,
    z      = NULL,
    method = c("pearson", "spearman")
) {
  method <- match.arg(method)

  if (is.null(z)) {
    # Simple bivariate correlation
    test <- stats::cor.test(as.numeric(x), as.numeric(y), method = method)
    return(data.frame(
      estimate = as.numeric(test$estimate),
      pvalue   = as.numeric(test$p.value),
      padj     = as.numeric(test$p.value),
      stringsAsFactors = FALSE
    ))
  }

  z_mat <- as.matrix(z)

  # Residualize x and y with respect to z
  fit_x <- stats::lm(x ~ z_mat)
  fit_y <- stats::lm(y ~ z_mat)

  res_x <- stats::residuals(fit_x)
  res_y <- stats::residuals(fit_y)

  test <- stats::cor.test(res_x, res_y, method = method)
  data.frame(
    estimate = as.numeric(test$estimate),
    pvalue   = as.numeric(test$p.value),
    padj     = as.numeric(test$p.value),
    stringsAsFactors = FALSE
  )
}
