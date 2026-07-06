# ===========================================================================
# cliomicplot: Heatmap Type
# For omics data visualization
# ===========================================================================

#' Heatmap Type
#'
#' @description Creates publication-ready heatmaps for visualizing omics data
#'   matrices. Supports row/column clustering, annotation tracks, and
#'   customizable color scales.
#'
#' @param scale Scale rows/columns: "none", "row", "column" (default "row")
#' @param cluster_rows Cluster rows (default TRUE)
#' @param cluster_cols Cluster columns (default TRUE)
#' @param show_rownames Show row names (default FALSE, auto-enabled for <50 rows)
#' @param show_colnames Show column names (default TRUE)
#' @param color_low Low end of color gradient (default "#2166AC")
#' @param color_mid Midpoint color (default "#F7F7F7")
#' @param color_high High end of color gradient (default "#B2182B")
#' @param color_midpoint Midpoint value (default 0)
#' @param annotation_col Optional data frame of column annotations
#' @param annotation_row Optional data frame of row annotations
#' @param annotation_colors List of colors for annotations
#' @param border_color Border color for cells (default NA, no border)
#' @param fontsize Base font size (default 10)
#'
#' @return A cliplot_type object for use with \code{\link{cliplot}}.
#'
#' @examples
#' \dontrun{
#' # Basic heatmap from matrix
#' mat = matrix(rnorm(200), ncol = 10)
#' rownames(mat) = paste0("Gene", 1:20)
#' colnames(mat) = paste0("Sample", 1:10)
#' cliplot(mat, type = "heatmap")
#'
#' # With annotations
#' ann_col = data.frame(Group = rep(c("Control","Treatment"), each = 5))
#' rownames(ann_col) = colnames(mat)
#' cliplot(mat, type = type_heatmap(annotation_col = ann_col))
#' }
#'
#' @export
type_heatmap = function(
    scale             = "row",
    cluster_rows      = TRUE,
    cluster_cols      = TRUE,
    show_rownames     = NULL,
    show_colnames     = TRUE,
    color_low         = "#2166AC",
    color_mid         = "#F7F7F7",
    color_high        = "#B2182B",
    color_midpoint    = 0,
    annotation_col    = NULL,
    annotation_row    = NULL,
    annotation_colors = NULL,
    border_color      = NA,
    fontsize          = 10
) {
  cliplot_type(
    data = function(settings, ...) {
      # Expect a matrix or a data frame
      mat = settings$data
      if (is.data.frame(mat)) {
        # Only treat first column as row names if it's non-numeric
        # (matrices converted to data.frame have all numeric cols)
        first_is_numeric = is.numeric(mat[[1]])
        if (!first_is_numeric && ncol(mat) > 1) {
          rn = mat[[1]]
          mat = as.matrix(mat[, -1, drop = FALSE])
          rownames(mat) = rn
        } else {
          mat = as.matrix(mat)
        }
      } else if (!is.matrix(mat)) {
        mat = as.matrix(mat)
      }

      scale = match.arg(scale, c("none", "row", "column"))
      if (scale == "row") {
        mat = t(base::scale(t(mat)))
      } else if (scale == "column") {
        mat = base::scale(mat)
      }
      mat[is.nan(mat) | is.infinite(mat)] = NA

      settings$heatmap_mat   = mat
      settings$hm_scale      = scale
      settings$hm_cluster_r  = cluster_rows
      settings$hm_cluster_c  = cluster_cols
      settings$hm_show_rn    = show_rownames
      settings$hm_show_cn    = show_colnames
      settings$hm_ann_col    = annotation_col
      settings$hm_ann_row    = annotation_row
      settings$hm_ann_colors = annotation_colors
      settings$hm_border     = border_color
      settings$hm_fontsize   = fontsize

      # Color gradient breakpoints. The actual colorRamp2 object is built
      # lazily in the draw step, only when ComplexHeatmap (which depends on
      # circlize) is available, so circlize can stay a soft dependency.
      hm_min = min(mat, na.rm = TRUE)
      hm_max = max(mat, na.rm = TRUE)
      if (scale %in% c("row", "column")) {
        max_abs = max(abs(c(hm_min, hm_max)), na.rm = TRUE)
        hm_min = -max_abs
        hm_max = max_abs
      }
      settings$hm_breaks = c(hm_min, color_midpoint, hm_max)
      settings$hm_colors = c(color_low, color_mid, color_high)
    },
    draw = function(data, mapping, settings, ...) {
      mat = settings$heatmap_mat
      if (is.null(mat)) return(ggplot2::ggplot())

      # Auto-decide rownames
      show_rn = settings$hm_show_rn
      if (is.null(show_rn)) {
        show_rn = nrow(mat) < 50
      }

      # Build heatmap using ComplexHeatmap if available, else pheatmap
      if (requireNamespace("ComplexHeatmap", quietly = TRUE)) {

        hm_color = circlize::colorRamp2(settings$hm_breaks, settings$hm_colors)

        hm_args = list(
          matrix             = mat,
          col                = hm_color,
          cluster_rows       = settings$hm_cluster_r,
          cluster_columns    = settings$hm_cluster_c,
          show_row_names     = show_rn,
          show_column_names  = settings$hm_show_cn,
          border             = !is.na(settings$hm_border),
          rect_gp            = grid::gpar(col = settings$hm_border),
          name               = "Expression",
          row_title          = "",
          column_title       = "",
          heatmap_legend_param = list(title = "Z-score")
        )

        if (!is.null(settings$hm_ann_col)) {
          hm_args$top_annotation = ComplexHeatmap::HeatmapAnnotation(
            df = settings$hm_ann_col,
            col = settings$hm_ann_colors
          )
        }
        if (!is.null(settings$hm_ann_row)) {
          hm_args$left_annotation = ComplexHeatmap::rowAnnotation(
            df = settings$hm_ann_row,
            col = settings$hm_ann_colors
          )
        }

        # Draw heatmap
        hm = do.call(ComplexHeatmap::Heatmap, hm_args)
        ComplexHeatmap::draw(hm)

        # Return empty ggplot for API consistency
        return(ggplot2::ggplot())

      } else {

        # Fallback to pheatmap-style using ggplot2
        melted = reshape2::melt(mat)
        names(melted) = c("Row", "Column", "Value")

        if (settings$hm_cluster_r) {
          row_order = stats::hclust(stats::dist(mat))$order
          melted$Row = factor(melted$Row, levels = rownames(mat)[row_order])
        }
        if (settings$hm_cluster_c) {
          col_order = stats::hclust(stats::dist(t(mat)))$order
          melted$Column = factor(melted$Column, levels = colnames(mat)[col_order])
        }

        p = ggplot2::ggplot(melted, ggplot2::aes(
          x = .data[["Column"]], y = .data[["Row"]], fill = .data[["Value"]]
        )) +
          ggplot2::geom_tile(color = settings$hm_border) +
          ggplot2::scale_fill_gradient2(
            low      = color_low,
            mid      = color_mid,
            high     = color_high,
            midpoint = color_midpoint,
            name     = "Z-score"
          ) +
          ggplot2::labs(x = "", y = "") +
          ggplot2::theme(
            axis.text.x = ggplot2::element_text(
              angle = 45, hjust = 1, size = settings$hm_fontsize - 2
            ),
            axis.text.y = if (show_rn) {
              ggplot2::element_text(size = settings$hm_fontsize - 2)
            } else {
              ggplot2::element_blank()
            }
          )

        p
      }
    },
    name = "heatmap"
  )
}
