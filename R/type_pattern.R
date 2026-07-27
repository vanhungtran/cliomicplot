# ===========================================================================
# cliomicplot: Pattern Clustering Type
# Inspired by xOmicsShiny patternmodule.R (k-means, Mfuzz, PAM clustering)
# ===========================================================================

#' Expression Pattern Clustering Type
#'
#' @description Creates publication-ready expression pattern clustering plots.
#'   Groups features (genes/proteins) by their expression profiles across
#'   conditions or time points using k-means, PAM, or soft clustering.
#'   Inspired by xOmicsShiny's pattern module.
#'
#' @param k Number of clusters (default 6)
#' @param method Clustering method: "kmeans", "pam", or "mfuzz" (default "kmeans")
#' @param ncol Number of columns in the facet grid (default 3)
#' @param scale Scale expression data before clustering? (default TRUE)
#' @param line_alpha Alpha for individual feature lines (default 0.35)
#' @param centroid_color Color for the cluster centroid line (default "#E64B35")
#' @param centroid_size Size for centroid line (default 1.2)
#' @param min_memb For "mfuzz": minimum membership threshold (default 0.4)
#' @param seed Random seed for reproducibility (default 123)
#' @param cluster_labels Optional named vector of cluster labels for annotation
#'
#' @return A \code{cliplot_type} object for use with \code{\link{cliplot}}.
#'
#' @details
#' Pattern clustering reveals groups of features that share similar expression
#' trajectories across conditions or time points. This is especially useful
#' for time-series or multi-condition omics experiments.
#'
#' The input should be a wide-format matrix or data frame where rows are
#' features (genes/proteins) and columns are conditions/time points.
#'
#' @examples
#' \dontrun{
#' # Cluster expression profiles into 6 patterns
#' cliplot(expr_wide, type = type_pattern(k = 6, method = "kmeans"))
#'
#' # PAM clustering with 4 clusters
#' cliplot(expr_wide, type = type_pattern(k = 4, method = "pam"))
#' }
#'
#' @export
type_pattern = function(
    k              = 6,
    method         = c("kmeans", "pam", "mfuzz"),
    ncol           = 3,
    scale          = TRUE,
    line_alpha     = 0.35,
    centroid_color = "#E64B35",
    centroid_size  = 1.2,
    min_memb       = 0.4,
    seed           = 123,
    cluster_labels = NULL
) {
  method = match.arg(method)

  cliplot_type(
    data = function(settings, ...) {
      # Expect a wide-format matrix/data.frame
      mat = settings$data
      if (is.null(mat)) stop("type_pattern requires a data matrix.", call. = FALSE)

      # Convert to matrix
      if (is.data.frame(mat)) {
        # Check if first column is labels
        first_is_numeric = is.numeric(mat[[1]])
        if (!first_is_numeric) {
          rn = mat[[1]]
          mat = as.matrix(mat[, -1, drop = FALSE])
          rownames(mat) = rn
        } else {
          mat = as.matrix(mat)
        }
      }

      if (scale) {
        mat = t(base::scale(t(mat)))
        mat[is.nan(mat) | is.infinite(mat)] = NA
      }

      # Create long format
      feature_names = rownames(mat) %||% paste0("Feature", 1:nrow(mat))
      group_names   = colnames(mat) %||% paste0("Group", 1:ncol(mat))

      long = reshape2::melt(mat)
      names(long) = c("Feature", "Group", "Value")
      long$Feature = as.character(long$Feature)
      long$Group = factor(long$Group, levels = group_names)

      # Cluster
      set.seed(seed)
      if (method == "kmeans") {
        mat_clean = mat; mat_clean[is.na(mat_clean)] = 0
        cl = stats::kmeans(mat_clean, centers = k)
        cluster_vec = cl$cluster
        long$Cluster = paste("Cluster", cluster_vec[long$Feature])
      } else if (method == "pam") {
        mat_clean = mat; mat_clean[is.na(mat_clean)] = 0
        cl = cluster::pam(mat_clean, k = k)
        cluster_vec = cl$clustering
        long$Cluster = paste("Cluster", cluster_vec[long$Feature])
      } else if (method == "mfuzz") {
        if (!requireNamespace("Mfuzz", quietly = TRUE)) {
          # Fallback to k-means
          mat_clean = mat; mat_clean[is.na(mat_clean)] = 0
          cl = stats::kmeans(mat_clean, centers = k)
          cluster_vec = cl$cluster
          long$Cluster = paste("Cluster", cluster_vec[long$Feature])
          message("Mfuzz not available; falling back to k-means.")
        } else {
          tmp_expr = methods::new("ExpressionSet", exprs = as.matrix(mat))
          cl = Mfuzz::mfuzz(tmp_expr, c = k, m = Mfuzz::mestimate(tmp_expr))
          cluster_vec = cl$cluster
          # Filter by membership
          memb = cl$membership
          max_memb = apply(memb, 1, max)
          keep = names(max_memb[max_memb >= min_memb])
          long = long[long$Feature %in% keep, ]
          long$Cluster = paste("Cluster", cluster_vec[long$Feature])
        }
      }

      # Sort clusters by size
      cluster_counts = sort(table(long$Cluster), decreasing = TRUE)
      long$Cluster = factor(long$Cluster, levels = names(cluster_counts))

      settings$pat_long           = long
      settings$pat_ncol           = ncol
      settings$pat_line_alpha     = line_alpha
      settings$pat_centroid_color = centroid_color
      settings$pat_centroid_size  = centroid_size
    },
    draw = function(data, mapping, settings, ...) {
      long = settings$pat_long
      if (is.null(long)) return(ggplot2::ggplot())

      # Add cluster sizes to facet labels
      cluster_info = stats::aggregate(
        Feature ~ Cluster, data = long,
        FUN = function(x) length(unique(x))
      )
      names(cluster_info)[2] = "N"
      cluster_info$Label = paste0(
        cluster_info$Cluster, " (n = ", cluster_info$N, ")"
      )
      names(cluster_info$Label) = cluster_info$Cluster
      long$ClusterLabel = cluster_info$Label[as.character(long$Cluster)]

      p = ggplot2::ggplot(long,
        ggplot2::aes(x = .data[["Group"]], y = .data[["Value"]])
      ) +
        ggplot2::geom_line(
          ggplot2::aes(group = .data[["Feature"]]),
          alpha = settings$pat_line_alpha,
          color = "grey60",
          linewidth = 0.3
        ) +
        ggplot2::stat_summary(
          ggplot2::aes(group = 1),
          fun = mean,
          geom  = "line",
          color = settings$pat_centroid_color,
          linewidth = settings$pat_centroid_size
        ) +
        ggplot2::facet_wrap(~ ClusterLabel, scales = "free_y",
                            ncol = settings$pat_ncol) +
        ggplot2::labs(
          x = "Condition / Time Point",
          y = if (scale) "Z-score" else "Expression"
        ) +
        ggplot2::theme(
          axis.text.x = ggplot2::element_text(
            angle = 45, hjust = 1, size = 9
          ),
          strip.text = ggplot2::element_text(
            face = "bold", size = 9
          ),
          legend.position = "none"
        )

      p
    },
    name = "pattern"
  )
}
