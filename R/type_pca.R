# ===========================================================================
# cliomicplot: PCA Plot Type
# For dimensionality reduction visualization (PCA, MDS, t-SNE, UMAP)
# ===========================================================================

#' PCA Plot Type
#'
#' @description Creates publication-ready PCA/MDS plots for visualizing
#'   high-dimensional omics data. Supports ellipses, sample labeling, and
#'   scree plots.
#'
#' @param pc_x Principal component for x-axis (default 1)
#' @param pc_y Principal component for y-axis (default 2)
#' @param center Center data before PCA (default TRUE)
#' @param scale. Scale data before PCA (default TRUE)
#' @param add_ellipse Add confidence ellipses (default TRUE)
#' @param ellipse_level Confidence level for ellipses (default 0.95)
#' @param point_size Point size (default 3.2)
#' @param point_alpha Point transparency (default 0.9)
#' @param point_stroke Point outline width.
#' @param label_samples Label sample names (default FALSE)
#' @param label_size Label text size (default 3)
#' @param show_scree Show scree plot as inset (default FALSE)
#' @param ellipse_alpha Fill transparency for confidence ellipses.
#' @param show_centroids Add group centroid markers.
#'
#' @return A cliplot_type object for use with \code{\link{cliplot}}.
#'
#' @examples
#' \dontrun{
#' # PCA of iris data
#' pca_data = iris[, 1:4]
#' cliplot(pca_data, type = "pca", by = iris$Species)
#'
#' # Custom PCA
#' cliplot(pca_data, type = type_pca(add_ellipse = TRUE, label_samples = TRUE),
#'         by = iris$Species)
#'
#' # From existing PCA results
#' pca_res = prcomp(iris[, 1:4], scale. = TRUE)
#' pca_scores = as.data.frame(pca_res$x)
#' pca_scores$Species = iris$Species
#' cliplot(PC2 ~ PC1, data = pca_scores, type = "points", by = Species)
#' }
#'
#' @export
type_pca = function(
    pc_x           = 1,
    pc_y           = 2,
    center         = TRUE,
    scale.         = TRUE,
    add_ellipse    = TRUE,
    ellipse_level  = 0.95,
    point_size     = 3.2,
    point_alpha    = 0.9,
    point_stroke   = 0.45,
    label_samples  = FALSE,
    label_size     = 3,
    show_scree     = FALSE,
    ellipse_alpha  = 0.13,
    show_centroids = TRUE
) {
  cliplot_type(
    data = function(settings, ...) {
      # Determine input: matrix/data frame or existing pca result
      input_data = settings$data

      # If x and y are already PC scores, skip PCA computation
      if (!is.null(settings$x) && !is.null(settings$y)) {
        # But only if data is NULL — otherwise x/y are from data.frame dispatch
        # and we need to use the full data matrix instead
        if (is.null(settings$data)) {
          df = data.frame(
            PC1 = settings$x,
            PC2 = settings$y,
            stringsAsFactors = FALSE
          )
          if (!is.null(settings$by)) df$Group = as.factor(settings$by)
          settings$pca_scores    = df
          settings$pca_var_expl  = NULL
          settings$pca_computed  = FALSE
          return()
        }
        # data is present — use it for PCA computation
        input_data = settings$data
      }

      # Compute PCA from data
      if (is.data.frame(input_data) || is.matrix(input_data)) {
        num_cols = names(input_data)[vapply(input_data, is.numeric, logical(1))]
        mat = as.matrix(input_data[, num_cols, drop = FALSE])
      } else {
        mat = as.matrix(input_data)
      }

      pca_res = stats::prcomp(mat, center = center, scale. = scale.)
      scores  = as.data.frame(pca_res$x)

      # Variance explained
      var_expl = round(100 * pca_res$sdev^2 / sum(pca_res$sdev^2), 1)

      df = data.frame(
        PC1 = scores[, pc_x],
        PC2 = scores[, pc_y],
        stringsAsFactors = FALSE
      )
      if (!is.null(settings$by)) df$Group = as.factor(settings$by)

      # Store in settings
      settings$pca_scores   = df
      settings$pca_var_expl = var_expl
      settings$pca_computed = TRUE
    },
    draw = function(data, mapping, settings, ...) {
      df = settings$pca_scores
      if (is.null(df)) return(ggplot2::ggplot())

      # Axis labels with variance explained
      xlab = if (!is.null(settings$pca_var_expl)) {
        sprintf("PC%d (%s%%)", pc_x, settings$pca_var_expl[pc_x])
      } else { "PC1" }
      ylab = if (!is.null(settings$pca_var_expl)) {
        sprintf("PC%d (%s%%)", pc_y, settings$pca_var_expl[pc_y])
      } else { "PC2" }

      has_group = "Group" %in% names(df)

      # Add sample labels column BEFORE ggplot() captures the data
      if (label_samples) {
        sample_names = rownames(settings$data) %||% rownames(df) %||% seq_len(nrow(df))
        if (length(sample_names) != nrow(df)) {
          sample_names = seq_len(nrow(df))
        }
        df$label = as.character(sample_names)
      }

      p = ggplot2::ggplot(df, ggplot2::aes(x = .data[["PC1"]], y = .data[["PC2"]])) +
        ggplot2::geom_hline(yintercept = 0, linetype = "dashed", color = "grey70", linewidth = 0.3) +
        ggplot2::geom_vline(xintercept = 0, linetype = "dashed", color = "grey70", linewidth = 0.3)

      # Add ellipses
      if (add_ellipse && has_group) {
        p = p + ggplot2::stat_ellipse(
          ggplot2::aes(fill = .data[["Group"]], color = .data[["Group"]]),
          level   = ellipse_level,
          alpha   = ellipse_alpha,
          geom    = "polygon",
          show.legend = FALSE
        )
      }

      if (has_group) {
        p = p + ggplot2::geom_point(
          ggplot2::aes(fill = .data[["Group"]]),
          shape = 21,
          color = "white",
          stroke = point_stroke,
          size = point_size,
          alpha = point_alpha
        )
      } else {
        p = p + ggplot2::geom_point(
          shape = 21,
          fill = "#2B6CB0",
          color = "white",
          stroke = point_stroke,
          size = point_size,
          alpha = point_alpha
        )
      }

      if (show_centroids && has_group) {
        centroids = stats::aggregate(
          df[, c("PC1", "PC2")],
          by = list(Group = df$Group),
          FUN = mean,
          na.rm = TRUE
        )
        p = p + ggplot2::geom_point(
          data = centroids,
          ggplot2::aes(x = .data[["PC1"]], y = .data[["PC2"]], color = .data[["Group"]]),
          inherit.aes = FALSE,
          shape = 4,
          stroke = 1.2,
          size = point_size + 1,
          show.legend = FALSE
        )
      }

      p = p + ggplot2::labs(x = xlab, y = ylab, fill = "", color = "")

      # Add sample labels
      if (label_samples) {
        p = p + ggrepel::geom_text_repel(
          ggplot2::aes(label = .data[["label"]]),
          size          = label_size,
          max.overlaps  = 30,
          show.legend   = FALSE
        )
      }

      p
    },
    name = "pca"
  )
}
