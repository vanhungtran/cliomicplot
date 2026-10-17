# ===========================================================================
# cliomicplot: Slope Plot Type
# Inspired by cnsplots slopeplot() - paired before/after comparisons
# ===========================================================================

#' Slope Plot Type
#'
#' @description Creates publication-ready slope plots (slopegraphs) showing
#'   how paired values change between two or more conditions. Ideal for
#'   before/after treatment comparisons and paired time-course data. Inspired
#'   by cnsplots' \code{slopeplot()}.
#'
#' @param pair Column name (or vector) identifying paired observations. Lines
#'   connect the same subject across conditions.
#' @param color_by Optional column name used to color the paired lines (e.g.
#'   responder status). Defaults to the \code{by} grouping variable.
#' @param linewidth Width of the connecting lines (default 0.65).
#' @param point_size Size of the points (default 2.6).
#' @param line_alpha Transparency of the lines (default 0.65).
#' @param point_alpha Transparency of the points (default 0.9).
#' @param reorder_x Reorder the x-axis categories in order of appearance
#'   (default TRUE).
#'
#' @return A \code{cliplot_type} object for use with \code{\link{cliplot}}.
#'
#' @details
#' Use the formula interface \code{cliplot(value ~ condition, data, type =
#' "slope")} with \code{type_slope(pair = "subject")} to draw one line per
#' paired subject across conditions. The optional \code{color_by} column
#' highlights groups (e.g. responders vs non-responders).
#'
#' @examples
#' \dontrun{
#' cliplot(value ~ timepoint, data = paired_df,
#'         type = type_slope(pair = "patient", color_by = "responder"))
#' }
#'
#' @export
type_slope = function(
    pair        = NULL,
    color_by    = NULL,
    linewidth   = 0.65,
    point_size  = 2.6,
    line_alpha  = 0.65,
    point_alpha = 0.9,
    reorder_x   = TRUE
) {
  cliplot_type(
    data = function(settings, ...) {
      df = settings$data
      if (is.null(df)) {
        df = data.frame(x = settings$x, y = settings$y)
      } else {
        df = as.data.frame(df)
        df$x = settings$x %||% df[[1]]
        df$y = settings$y %||% df[[2]]
      }

      pair_col = pair
      if (is.null(pair_col)) {
        if (!is.null(settings$by)) {
          df$..pair.. = settings$by
          pair_col = "..pair.."
        } else {
          stop("type_slope needs a `pair` column or a `by` grouping variable.",
               call. = FALSE)
        }
      } else if (is.character(pair_col) && pair_col %in% names(df)) {
        # already a column in df
      } else {
        df$..pair.. = pair_col
        pair_col = "..pair.."
      }

      color_col = color_by
      if (is.null(color_col) && !is.null(settings$by)) {
        df$..by.. = as.factor(settings$by)
        color_col = "..by.."
      } else if (!is.null(color_col) && is.character(color_col) &&
                 color_col %in% names(df)) {
        # already a column
      } else if (!is.null(color_col)) {
        df$..color.. = color_col
        color_col = "..color.."
      }

      if (isTRUE(reorder_x) && (is.character(df$x) || is.factor(df$x))) {
        df$x = factor(df$x, levels = unique(df$x))
      }

      settings$slope_data = df
      settings$slope_pair = pair_col
      settings$slope_color = color_col
      settings$slope_linewidth = linewidth
      settings$slope_point_size = point_size
      settings$slope_line_alpha = line_alpha
      settings$slope_point_alpha = point_alpha
    },
    draw = function(data, mapping, settings, ...) {
      df = settings$slope_data
      if (is.null(df)) return(ggplot2::ggplot())
      pair_col = settings$slope_pair
      color_col = settings$slope_color

      p = ggplot2::ggplot(df, ggplot2::aes(
        x = .data[["x"]], y = .data[["y"]],
        group = .data[[pair_col]]
      )) +
        ggplot2::geom_line(
          linewidth = settings$slope_linewidth,
          alpha = settings$slope_line_alpha
        ) +
        ggplot2::geom_point(
          size = settings$slope_point_size,
          alpha = settings$slope_point_alpha,
          stroke = 0.3
        )

      if (!is.null(color_col)) {
        p = p + ggplot2::aes(color = .data[[color_col]])
        n_groups = length(unique(df[[color_col]]))
        p = p + ggplot2::scale_color_manual(
          values = get_cli_palette(settings$palette %||% "npg", n_groups)
        )
      }

      p = p + ggplot2::labs(
        x = if (is.null(settings$xlab)) "Condition" else settings$xlab,
        y = if (is.null(settings$ylab)) "Value" else settings$ylab
      )

      p

    },
    name = "slope"
  )
}

