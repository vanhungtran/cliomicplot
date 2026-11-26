# ===========================================================================
# cliomicplot: Deconvolution Identifiability Plot Type
# For visualizing marker gene counts, collinearity, and zeroed cell types
# Reference: Microbiome transfer learning / deconvolution identifiability
# ===========================================================================

#' Deconvolution Identifiability Plot Type
#'
#' @description Creates publication-ready deconvolution identifiability plots
#'   that explain why specific cell types are detected versus zeroed in bulk
#'   tissue deconvolution. Combines marker gene counts (bars) on the primary
#'   axis with pairwise collinearity metrics (triangles) on a secondary axis.
#'
#' @param collinearity Column name or vector for maximum collinearity correlation
#'   (Pearson r in [0, 1]). Default looks for \code{"max_corr"}.
#' @param status Column name or vector for deconvolution status (e.g.
#'   \code{"predicted"} vs \code{"zeroed"}). Default looks for \code{"status"}.
#' @param flag Column name or vector for identifiability diagnostic flag (e.g.
#'   \code{"identifiable"}, \code{"collinear"}, \code{"few markers"},
#'   \code{"few markers + collinear"}). Default looks for \code{"note"} or \code{"flag"}.
#' @param max_marker_scale Numeric scaling factor for secondary axis transformation
#'   (default auto-detected from maximum marker count).
#' @param bar_width Numeric bar width (default 0.62).
#' @param point_size Numeric point size for collinearity glyphs (default 2.8).
#' @param status_colors Named vector of fill colors for deconvolution status
#'   (default: predicted = \code{"#2E8B57"}, zeroed = \code{"#C0C0C0"}).
#' @param flag_colors Named vector of colors for identifiability diagnostic flags
#'   (default: identifiable = \code{"#2166AC"}, collinear = \code{"#B2182B"},
#'   few markers = \code{"#F4A582"}, few markers + collinear = \code{"#7B3294"}).
#' @param ... Additional arguments.
#'
#' @return A \code{cliplot_type} object for use with \code{\link{cliplot}}.
#'
#' @examples
#' \dontrun{
#' df_ident = data.frame(
#'   cell_type = c("T_cell", "B_cell", "Macrophage", "Keratinocyte"),
#'   n_markers = c(13, 3, 42, 32),
#'   max_corr  = c(0.57, 0.90, 0.77, 0.46),
#'   status    = c("predicted", "predicted", "predicted", "zeroed"),
#'   note      = c("identifiable", "few markers + collinear", "collinear", "identifiable")
#' )
#' cliplot(n_markers ~ cell_type, data = df_ident, type = type_identifiability())
#' }
#'
#' @export
type_identifiability = function(
    collinearity     = NULL,
    status           = NULL,
    flag             = NULL,
    max_marker_scale = NULL,
    bar_width        = 0.62,
    point_size       = 2.8,
    status_colors    = c(predicted = "#2E8B57", zeroed = "#C0C0C0"),
    flag_colors      = c(identifiable = "#2166AC", collinear = "#B2182B",
                         "few markers" = "#F4A582", "few markers + collinear" = "#7B3294"),
    ...
) {
  cliplot_type(
    data = function(settings, ...) {
      x_vals = settings$x
      y_vals = settings$y
      by_vals = settings$by

      df = data.frame(
        cell_type = x_vals,
        n_markers = as.numeric(y_vals),
        stringsAsFactors = FALSE
      )
      if (!is.null(by_vals)) {
        df$signature = as.factor(by_vals)
      }

      raw_data = settings$data
      if (!is.null(raw_data) && is.data.frame(raw_data)) {
        nms = names(raw_data)

        # 1. Collinearity
        if (!is.null(collinearity) && collinearity %in% nms) {
          df$max_corr = raw_data[[collinearity]]
        } else if ("max_corr" %in% nms) {
          df$max_corr = raw_data$max_corr
        } else if ("collinearity" %in% nms) {
          df$max_corr = raw_data$collinearity
        } else if ("r" %in% nms) {
          df$max_corr = raw_data$r
        } else {
          df$max_corr = rep(0.5, nrow(df))
        }

        # 2. Deconvolution status
        if (!is.null(status) && status %in% nms) {
          df$status = raw_data[[status]]
        } else if ("status" %in% nms) {
          df$status = raw_data$status
        } else if ("prop" %in% nms) {
          df$status = ifelse(raw_data$prop > 0, "predicted", "zeroed")
        } else {
          df$status = ifelse(df$n_markers > 5, "predicted", "zeroed")
        }

        # 3. Identifiability flag / note
        if (!is.null(flag) && flag %in% nms) {
          df$flag = raw_data[[flag]]
        } else if ("note" %in% nms) {
          df$flag = raw_data$note
        } else if ("flag" %in% nms) {
          df$flag = raw_data$flag
        } else {
          few = df$n_markers <= 5
          coll = df$max_corr >= 0.7
          df$flag = ifelse(few & coll, "few markers + collinear",
                    ifelse(few, "few markers",
                    ifelse(coll, "collinear", "identifiable")))
        }
      } else {
        df$max_corr = rep(0.5, nrow(df))
        df$status   = ifelse(df$n_markers > 5, "predicted", "zeroed")
        df$flag     = "identifiable"
      }

      scale_val = max_marker_scale %||% max(df$n_markers, na.rm = TRUE)
      if (scale_val <= 0) scale_val = 60

      settings$ident_df            = df
      settings$ident_scale         = scale_val
      settings$ident_bar_width     = bar_width
      settings$ident_point_size    = point_size
      settings$ident_status_colors = status_colors
      settings$ident_flag_colors   = flag_colors
      settings$skip_postprocess    = TRUE
    },
    draw = function(data, mapping, settings, ...) {
      df = settings$ident_df
      if (is.null(df) || nrow(df) == 0) return(ggplot2::ggplot())

      scale_fac = settings$ident_scale

      # Order cell types by max_corr descending if not already factor
      if (!is.factor(df$cell_type)) {
        if ("signature" %in% names(df)) {
          df = df[order(df$signature, -df$max_corr), ]
        } else {
          df = df[order(-df$max_corr), ]
        }
        df$cell_type = factor(df$cell_type, levels = rev(unique(df$cell_type)))
      }

      p = ggplot2::ggplot(df, ggplot2::aes(x = .data[["cell_type"]])) +
        ggplot2::geom_col(
          ggplot2::aes(y = .data[["n_markers"]], fill = .data[["status"]]),
          width = settings$ident_bar_width,
          color = "black", linewidth = 0.35
        ) +
        ggplot2::geom_point(
          ggplot2::aes(y = .data[["max_corr"]] * scale_fac, color = .data[["flag"]]),
          size = settings$ident_point_size,
          shape = 17
        ) +
        ggplot2::scale_y_continuous(
          name = settings$ylab %||% "n marker genes (bars)",
          sec.axis = ggplot2::sec_axis(
            ~ . / scale_fac,
            name = "max collinearity r (triangles)",
            breaks = seq(0, 1, 0.2)
          )
        ) +
        ggplot2::scale_fill_manual(
          values = settings$ident_status_colors,
          name   = "Deconv Result"
        ) +
        ggplot2::scale_color_manual(
          values = settings$ident_flag_colors,
          name   = "Identifiability Flag"
        ) +
        ggplot2::coord_flip()

      if ("signature" %in% names(df)) {
        p = p + ggplot2::facet_wrap(~ signature, scales = "free_y")
      }

      p = p + ggplot2::labs(
        x = settings$xlab %||% NULL,
        title = settings$title %||% "Deconvolution Identifiability: Why Cell Types Get Zeroed",
        subtitle = settings$subtitle %||% "Bars = n marker genes; Triangles = max collinearity r with another cell type"
      )

      p
    },
    name = "identifiability"
  )
}
