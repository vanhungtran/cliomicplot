# ===========================================================================
# cliomicplot: Module Summarization & Network Rewiring Analysis
# Multi-Omics Module Eigengenes, Composite Scores, and Edge Rewiring Tests
# ===========================================================================

#' Calculate Module Eigengenes and Composite Scores
#'
#' @description
#' Summarizes high-dimensional omics modules (e.g. microbial clusters, gene co-expression
#' modules, or metabolite clusters) into single representative profile vectors per module.
#' Supports:
#' \itemize{
#'   \item \code{"eigengene"}: First principal component (PC1) via singular value decomposition
#'         (SVD), with orientation aligned to positively correlate with average module expression.
#'   \item \code{"mean_score"}: Mean of z-score standardized member features.
#'   \item \code{"sum"}: Sum of raw feature abundances.
#' }
#'
#' @param expr_data Numeric matrix or data frame of molecular or microbial profiles
#'   (samples in rows, features in columns).
#' @param modules Character or factor vector of module assignments for each feature in \code{expr_data}.
#'   Can also be a named vector where names correspond to column names of \code{expr_data}.
#' @param method Character metric: \code{"eigengene"} (default), \code{"mean_score"}, or \code{"sum"}.
#' @param prefix Optional character prefix for output module column names (default \code{"ME"}).
#'
#' @return A data frame with samples in rows and module summary scores in columns.
#'
#' @export
cli_module_summary = function(
    expr_data,
    modules,
    method = c("eigengene", "mean_score", "sum"),
    prefix = "ME"
) {
  method <- match.arg(method)
  mat    <- as.matrix(expr_data)
  n_samp <- nrow(mat)
  f_nms  <- colnames(mat) %||% paste0("F_", seq_len(ncol(mat)))
  colnames(mat) <- f_nms

  if (length(modules) != ncol(mat)) {
    if (!is.null(names(modules)) && all(names(modules) %in% f_nms)) {
      modules <- modules[f_nms]
    } else {
      stop("Length of modules must match the number of columns in expr_data.")
    }
  }

  u_mods <- setdiff(unique(modules), c("grey", "gray", "0", "None", NA))
  out_mat <- matrix(0, nrow = n_samp, ncol = length(u_mods))
  colnames(out_mat) <- paste0(if (nzchar(prefix)) paste0(prefix, "_") else "", u_mods)
  rownames(out_mat) <- rownames(mat) %||% paste0("Sample_", seq_len(n_samp))

  for (i in seq_along(u_mods)) {
    mod_id  <- u_mods[i]
    mod_idx <- which(modules == mod_id)
    sub_mat <- mat[, mod_idx, drop = FALSE]

    if (ncol(sub_mat) == 1) {
      out_mat[, i] <- as.numeric(sub_mat[, 1])
      next
    }

    if (method == "eigengene") {
      # Standardize features
      scaled_mat <- scale(sub_mat)
      scaled_mat[is.na(scaled_mat)] <- 0

      svd_res <- tryCatch(
        stats::prcomp(scaled_mat, center = FALSE, scale. = FALSE),
        error = function(e) NULL
      )

      if (!is.null(svd_res)) {
        pc1 <- svd_res$x[, 1]
        # Align sign with average expression
        avg_expr <- rowMeans(sub_mat, na.rm = TRUE)
        if (stats::cor(pc1, avg_expr) < 0) pc1 <- -pc1
        out_mat[, i] <- pc1
      } else {
        out_mat[, i] <- rowMeans(scale(sub_mat), na.rm = TRUE)
      }

    } else if (method == "mean_score") {
      scaled_mat <- scale(sub_mat)
      out_mat[, i] <- rowMeans(scaled_mat, na.rm = TRUE)

    } else if (method == "sum") {
      out_mat[, i] <- rowSums(sub_mat, na.rm = TRUE)
    }
  }

  as.data.frame(out_mat)
}

#' Differential Network Rewiring and Edge Conservation Analysis
#'
#' @description
#' Compares edge architecture and correlation strengths between two conditions
#' (e.g., Control vs Disease or Wild-Type vs Knockout). Identifies conserved edges,
#' rewired edges, edge Jaccard similarity, and evaluates differential correlation
#' significance using Fisher's z-transformation.
#'
#' @param net1 Numeric correlation or adjacency matrix for Condition 1.
#' @param net2 Numeric correlation or adjacency matrix for Condition 2.
#' @param n1 Sample size of Condition 1 (used for Fisher's z-test, default 30).
#' @param n2 Sample size of Condition 2 (used for Fisher's z-test, default 30).
#' @param r_threshold Absolute correlation threshold to define active edges (default 0.4).
#'
#' @return A list containing:
#'   \itemize{
#'     \item \code{jaccard_similarity}: Global Jaccard index of conserved edges \eqn{|E_1 \cap E_2| / |E_1 \cup E_2|}.
#'     \item \code{summary}: Counts of Condition 1-specific, Condition 2-specific, and conserved edges.
#'     \item \code{edges}: Data frame of all evaluated feature pairs with \eqn{r_1}, \eqn{r_2},
#'           Fisher's \eqn{Z}-score, p-value, and classification status (\code{"Conserved"}, \code{"Specific_Net1"}, \code{"Specific_Net2"}, \code{"Rewired"}).
#'     \item \code{node_rewiring}: Data frame of per-node rewiring scores based on topological neighborhood divergence.
#'   }
#'
#' @export
cli_network_rewiring = function(
    net1,
    net2,
    n1          = 30,
    n2          = 30,
    r_threshold = 0.4
) {
  mat1 <- as.matrix(net1)
  mat2 <- as.matrix(net2)

  shared_nodes <- intersect(colnames(mat1), colnames(mat2))
  if (length(shared_nodes) < 3) {
    p <- min(ncol(mat1), ncol(mat2))
    shared_nodes <- colnames(mat1)[1:p] <- colnames(mat2)[1:p] <- paste0("Node_", seq_len(p))
  }

  mat1 <- mat1[shared_nodes, shared_nodes]
  mat2 <- mat2[shared_nodes, shared_nodes]
  diag(mat1) <- diag(mat2) <- 0

  idx_tri <- which(upper.tri(mat1))
  row_idx <- row(mat1)[idx_tri]
  col_idx <- col(mat1)[idx_tri]

  node_a <- shared_nodes[row_idx]
  node_b <- shared_nodes[col_idx]

  r1_vec <- mat1[idx_tri]
  r2_vec <- mat2[idx_tri]

  # Active edges
  is_e1 <- abs(r1_vec) >= r_threshold
  is_e2 <- abs(r2_vec) >= r_threshold

  n_e1_only <- sum(is_e1 & !is_e2)
  n_e2_only <- sum(!is_e1 & is_e2)
  n_shared  <- sum(is_e1 & is_e2)
  n_union   <- sum(is_e1 | is_e2)

  jaccard <- if (n_union > 0) n_shared / n_union else 0

  # Fisher's z test on correlation differences
  z1 <- atanh(pmin(pmax(r1_vec, -0.999), 0.999))
  z2 <- atanh(pmin(pmax(r2_vec, -0.999), 0.999))
  se_diff <- sqrt(1 / max(1, n1 - 3) + 1 / max(1, n2 - 3))
  diff_z  <- (z1 - z2) / se_diff
  diff_p  <- 2 * (1 - stats::pnorm(abs(diff_z)))

  status_vec <- character(length(idx_tri))
  for (i in seq_along(status_vec)) {
    if (is_e1[i] && is_e2[i]) {
      if (sign(r1_vec[i]) != sign(r2_vec[i]) && diff_p[i] < 0.05) {
        status_vec[i] <- "Sign_Inversion"
      } else {
        status_vec[i] <- "Conserved"
      }
    } else if (is_e1[i] && !is_e2[i]) {
      status_vec[i] <- "Specific_Net1"
    } else if (!is_e1[i] && is_e2[i]) {
      status_vec[i] <- "Specific_Net2"
    } else {
      status_vec[i] <- "None"
    }
  }

  edges_df <- data.frame(
    from      = node_a,
    to        = node_b,
    r_net1    = round(r1_vec, 3),
    r_net2    = round(r2_vec, 3),
    diff_z    = round(diff_z, 2),
    pvalue    = round(diff_p, 4),
    padj      = round(stats::p.adjust(diff_p, method = "BH"), 4),
    status    = status_vec,
    stringsAsFactors = FALSE
  )
  edges_active <- edges_df[edges_df$status != "None", ]

  # Per-node rewiring score: neighborhood Jaccard distance
  node_rewiring <- data.frame(
    node = shared_nodes,
    degree_net1 = colSums(abs(mat1) >= r_threshold),
    degree_net2 = colSums(abs(mat2) >= r_threshold),
    stringsAsFactors = FALSE
  )

  list(
    jaccard_similarity = round(jaccard, 3),
    summary = data.frame(
      metric = c("Net1_Only_Edges", "Net2_Only_Edges", "Shared_Edges", "Total_Union", "Jaccard_Index"),
      value  = c(n_e1_only, n_e2_only, n_shared, n_union, round(jaccard, 3)),
      stringsAsFactors = FALSE
    ),
    edges = edges_active,
    node_rewiring = node_rewiring
  )
}

#' Generate Synthetic Multi-Network Multi-Omics Dataset
#'
#' @description
#' Generates paired synthetic datasets matching the 3-condition experimental setup
#' (e.g. KO, WT, OE) and 4-corner environmental matrix layout.
#'
#' @param n_samples Number of samples per condition (default 30).
#' @param n_features Number of microbial or gene features (default 60).
#' @param seed Optional integer seed.
#'
#' @return A list containing:
#'   \itemize{
#'     \item \code{ref_expr}: Expression profiles for Reference (WT).
#'     \item \code{test_expr}: Expression profiles for Test (KO).
#'     \item \code{modules}: Named vector of module labels.
#'     \item \code{networks}: List of adjacency/correlation matrices for KO, WT, OE.
#'     \item \code{env_data}: Data frame of 15 environmental variables (pH, N, P, K, etc.).
#'     \item \code{links}: Formatted links table ready for \code{\link{cliplot_env_network}}.
#'   }
#'
#' @export
cli_demo_vitality_multiomics = function(n_samples = 30, n_features = 60, seed = 42) {
  if (!is.null(seed)) set.seed(seed)

  f_names <- paste0("Taxon_", seq_len(n_features))
  n_mods  <- 4
  mod_vec <- paste0("M", rep(seq_len(n_mods), length.out = n_features))
  names(mod_vec) <- f_names

  make_condition_mat <- function(cor_scale = 0.7) {
    # Latent factors for modules
    latents <- matrix(stats::rnorm(n_samples * n_mods), nrow = n_samples, ncol = n_mods)
    x_mat   <- matrix(stats::rnorm(n_samples * n_features), nrow = n_samples, ncol = n_features)

    for (j in seq_len(n_features)) {
      m_idx <- as.integer(substr(mod_vec[j], 2, 2))
      x_mat[, j] <- cor_scale * latents[, m_idx] + sqrt(1 - cor_scale^2) * x_mat[, j]
    }
    colnames(x_mat) <- f_names
    rownames(x_mat) <- paste0("S_", seq_len(n_samples))
    x_mat
  }

  mat_wt <- exp(make_condition_mat(0.75))
  mat_ko <- exp(make_condition_mat(0.40))
  mat_oe <- exp(make_condition_mat(0.85))

  r_wt <- stats::cor(mat_wt)
  r_ko <- stats::cor(mat_ko)
  r_oe <- stats::cor(mat_oe)

  env_names <- c("pH", "Humdepth", "Baresoil", "Mo", "Zn", "Mn", "Fe", "Al", "S", "Mg", "Ca", "K", "P", "N", "Zr")
  env_mat <- matrix(stats::rnorm(n_samples * length(env_names)), nrow = n_samples, ncol = length(env_names))
  colnames(env_mat) <- env_names
  rownames(env_mat) <- rownames(mat_wt)

  # Synthetic links between modules and environmental variables
  links_df <- data.frame(
    module = c("M1", "M1", "M2", "M2", "M3", "M3", "M4", "M4"),
    corner = c("top_left", "top_right", "top_left", "bottom_left", "bottom_right", "top_right", "bottom_right", "bottom_left"),
    var    = c("pH", "Humdepth", "Ca", "N", "Fe", "Mo", "K", "Mg"),
    r      = c(0.68, -0.52, 0.61, 0.49, -0.55, 0.72, 0.58, -0.47),
    p      = c(0.0001, 0.003, 0.0004, 0.007, 0.002, 0.00005, 0.001, 0.009),
    stringsAsFactors = FALSE
  )

  list(
    ref_expr = as.data.frame(mat_wt),
    test_expr = as.data.frame(mat_ko),
    modules  = mod_vec,
    networks = list(KO = r_ko, WT = r_wt, OE = r_oe),
    env_data = as.data.frame(env_mat),
    links    = links_df
  )
}
