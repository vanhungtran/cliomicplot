# ===========================================================================
# cliomicplot: Permutation Significance Testing for Cross-Network Differences
# Non-Parametric Hypothesis Testing for Network Topology Differences
# ===========================================================================

#' Permutation Significance Testing for Cross-Network Structural Differences
#'
#' @description
#' Evaluates whether global topological structural metrics (edge density, natural
#' connectivity, negative-to-positive edge ratio, mean degree, and clustering coefficient)
#' differ significantly between experimental conditions (e.g. KO vs WT, Case vs Control,
#' or Healthy vs Lesional). Generates an empirical null distribution by shuffling
#' sample group labels \eqn{B} times, recomputing correlation networks, and calculating
#' two-sided permutation p-values.
#'
#' @param comm_data Numeric matrix or data frame of feature abundances (samples in rows,
#'   features in columns).
#' @param group Factor or character vector specifying condition assignments for each sample.
#'   Must match \code{nrow(comm_data)} and contain at least 2 distinct groups.
#' @param ref_group Optional character string designating the baseline reference group.
#'   If NULL, the first level of \code{group} is used.
#' @param test_group Optional character string designating the comparison group.
#'   If NULL, the second level of \code{group} is used.
#' @param cor_method Correlation metric: \code{"spearman"} (default) or \code{"pearson"}.
#' @param cor_cutoff Absolute correlation threshold \eqn{|r|} to establish network edges (default 0.35).
#' @param p_cutoff Significance threshold for correlation p-values (default 0.05).
#' @param n_perm Integer. Number of permutation iterations (default 199).
#' @param seed Optional integer seed for reproducibility.
#'
#' @return A data frame summarizing observed metrics, permuted null means, standard errors,
#'   and two-sided permutation p-values:
#'   \item{metric}{Topological network metric}
#'   \item{ref_val}{Observed metric value in the reference condition}
#'   \item{test_val}{Observed metric value in the test condition}
#'   \item{delta_obs}{Observed difference (\code{test_val - ref_val})}
#'   \item{perm_mean}{Mean of the permuted null differences}
#'   \item{perm_se}{Standard error of the permuted null differences}
#'   \item{p_perm}{Two-sided empirical permutation p-value}
#'
#' @examples
#' set.seed(42)
#' comm <- matrix(rpois(40 * 25, lambda = 12), nrow = 40, ncol = 25)
#' colnames(comm) <- paste0("Taxon_", 1:25)
#' grp <- rep(c("Control", "Treatment"), each = 20)
#' perm_res <- cli_network_permutation(comm, grp, n_perm = 49, seed = 42)
#' print(perm_res)
#'
#' @export
cli_network_permutation = function(
    comm_data,
    group,
    ref_group  = NULL,
    test_group = NULL,
    cor_method = c("spearman", "pearson"),
    cor_cutoff = 0.35,
    p_cutoff   = 0.05,
    n_perm     = 199,
    seed       = NULL
) {
  if (!is.null(seed)) set.seed(seed)
  cor_method <- match.arg(cor_method)

  mat <- as.matrix(comm_data)
  group <- as.character(group)

  if (nrow(mat) != length(group)) {
    stop("Length of group vector must match nrow(comm_data).", call. = FALSE)
  }

  u_grps <- unique(group)
  if (length(u_grps) < 2) {
    stop("group must contain at least 2 distinct levels.", call. = FALSE)
  }

  if (is.null(ref_group)) ref_group <- u_grps[1]
  if (is.null(test_group)) test_group <- setdiff(u_grps, ref_group)[1]

  if (!ref_group %in% u_grps || !test_group %in% u_grps) {
    stop("ref_group and test_group must be valid levels in group.", call. = FALSE)
  }

  # Internal function to calculate network properties from a sample subset
  compute_net_metrics <- function(sub_mat) {
    p_dim <- ncol(sub_mat)
    n_samp <- nrow(sub_mat)
    if (n_samp < 4 || p_dim < 3) {
      return(c(density = 0, natural_conn = 0, neg_pos_ratio = 0, mean_deg = 0, clust_coef = 0))
    }

    # Pairwise correlation
    c_res <- stats::cor(sub_mat, method = cor_method, use = "pairwise.complete.obs")
    c_res[is.na(c_res)] <- 0
    diag(c_res) <- 0

    # Calculate asymptotic p-values
    df_res <- n_samp - 2
    t_stat <- c_res * sqrt(pmax(df_res, 1) / pmax(1 - c_res^2, 1e-10))
    p_mat <- 2 * stats::pt(-abs(t_stat), df = df_res)

    # Thresholded adjacency
    adj_pos <- (c_res >= cor_cutoff & p_mat <= p_cutoff) * 1
    adj_neg <- (c_res <= -cor_cutoff & p_mat <= p_cutoff) * 1
    adj <- adj_pos + adj_neg
    diag(adj) <- 0

    e_pos <- sum(adj_pos) / 2
    e_neg <- sum(adj_neg) / 2
    e_tot <- e_pos + e_neg
    max_e <- (p_dim * (p_dim - 1)) / 2

    # 1. Density
    dens <- if (max_e > 0) e_tot / max_e else 0

    # 2. Natural Connectivity
    nat_conn <- calc_natural_connectivity(adj)

    # 3. Negative-to-Positive ratio
    np_ratio <- if (e_pos > 0) e_neg / e_pos else 0

    # 4. Mean Degree
    m_deg <- if (p_dim > 0) (2 * e_tot) / p_dim else 0

    # 5. Mean Clustering Coefficient
    deg <- rowSums(adj)
    cc_vec <- numeric(p_dim)
    for (i in seq_len(p_dim)) {
      if (deg[i] >= 2) {
        nbrs <- which(adj[i, ] == 1)
        sub_adj <- adj[nbrs, nbrs]
        actual_e <- sum(sub_adj) / 2
        possible_e <- (length(nbrs) * (length(nbrs) - 1)) / 2
        cc_vec[i] <- if (possible_e > 0) actual_e / possible_e else 0
      }
    }
    m_cc <- mean(cc_vec, na.rm = TRUE)

    c(
      density          = dens,
      natural_conn     = nat_conn,
      neg_pos_ratio    = np_ratio,
      mean_deg         = m_deg,
      clust_coef       = m_cc
    )
  }

  idx_ref  <- which(group == ref_group)
  idx_test <- which(group == test_group)
  idx_both <- c(idx_ref, idx_test)
  sub_mat_both <- mat[idx_both, , drop = FALSE]
  sub_grp_both <- group[idx_both]

  # Observed metrics
  obs_ref  <- compute_net_metrics(mat[idx_ref, , drop = FALSE])
  obs_test <- compute_net_metrics(mat[idx_test, , drop = FALSE])
  delta_obs <- obs_test - obs_ref

  # Permutation iterations
  n_metrics <- length(delta_obs)
  perm_diffs <- matrix(0, nrow = n_perm, ncol = n_metrics)
  colnames(perm_diffs) <- names(delta_obs)

  n_ref <- length(idx_ref)
  n_tot <- length(idx_both)

  for (b in seq_len(n_perm)) {
    perm_order <- sample(n_tot)
    b_ref  <- sub_mat_both[perm_order[1:n_ref], , drop = FALSE]
    b_test <- sub_mat_both[perm_order[(n_ref + 1):n_tot], , drop = FALSE]

    m_ref  <- compute_net_metrics(b_ref)
    m_test <- compute_net_metrics(b_test)
    perm_diffs[b, ] <- m_test - m_ref
  }

  # Two-sided empirical p-values
  p_vals <- numeric(n_metrics)
  perm_means <- numeric(n_metrics)
  perm_ses <- numeric(n_metrics)

  for (m_i in seq_len(n_metrics)) {
    d_obs <- delta_obs[m_i]
    null_dist <- perm_diffs[, m_i]
    perm_means[m_i] <- mean(null_dist, na.rm = TRUE)
    perm_ses[m_i]   <- stats::sd(null_dist, na.rm = TRUE)
    # Conservative permutation p-value: (1 + sum(|null| >= |obs|)) / (B + 1)
    p_vals[m_i] <- (1 + sum(abs(null_dist) >= abs(d_obs))) / (n_perm + 1)
  }

  metric_labels <- c(
    density          = "Edge Density",
    natural_conn     = "Natural Connectivity",
    neg_pos_ratio    = "Negative/Positive Ratio",
    mean_deg         = "Mean Degree",
    clust_coef       = "Clustering Coefficient"
  )

  res_df <- data.frame(
    metric     = unname(metric_labels[names(delta_obs)]),
    ref_val    = round(unname(obs_ref), 4),
    test_val   = round(unname(obs_test), 4),
    delta_obs  = round(unname(delta_obs), 4),
    perm_mean  = round(perm_means, 4),
    perm_se    = round(perm_ses, 4),
    p_perm     = round(p_vals, 4),
    stringsAsFactors = FALSE
  )

  attr(res_df, "ref_group")  <- ref_group
  attr(res_df, "test_group") <- test_group
  attr(res_df, "n_perm")     <- n_perm
  class(res_df) <- c("cli_network_permutation", "data.frame")
  res_df
}

#' Print Method for Network Permutation Test
#' @param x Object of class \code{cli_network_permutation}.
#' @param ... Additional arguments.
#' @export
print.cli_network_permutation = function(x, ...) {
  ref_g  <- attr(x, "ref_group")  %||% "Reference"
  test_g <- attr(x, "test_group") %||% "Test"
  n_p    <- attr(x, "n_perm")     %||% nrow(x)

  cat("\n=================================================================\n")
  cat(" cliomicplot: Cross-Network Permutation Test\n")
  cat(" Reference (", ref_g, ") vs Comparison (", test_g, ")\n", sep = "")
  cat(" Number of Permutations: ", n_p, "\n", sep = "")
  cat("=================================================================\n")
  print.data.frame(x, row.names = FALSE)
  cat("-----------------------------------------------------------------\n")
  cat(" Note: Two-sided empirical p-value: (1 + sum(|delta_null| >= |delta_obs|)) / (B + 1)\n\n")
  invisible(x)
}
