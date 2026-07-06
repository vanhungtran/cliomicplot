# ===========================================================================
# cliomicplot: Spineplot, Rug, Abline, QQ Plot Types
# ===========================================================================

#' Spine Plot / Spinogram Type
#'
#' @description Creates spine plots or spinograms for visualizing the
#'   relationship between categorical variables. Width of bars represents
#'   marginal distribution; height represents conditional distribution.
#'
#' @param weights Optional vector of weights
#'
#' @return A cliplot_type object
#' @export
type_spineplot = function(weights = NULL) {
  cliplot_type(
    data = function(settings, ...) {
      df = settings$data
      if (is.null(df)) {
        df = data.frame(x = settings$x, y = settings$y)
      }
      if (!is.null(weights) && is.numeric(weights) && length(weights) == nrow(df)) {
        df$..wt.. = weights
      }
      settings$spine_data = df
    },
    draw = function(data, mapping, settings, ...) {
      df = settings$spine_data
      if (is.null(df)) df = data

      x_col = names(df)[1]
      y_col = if (ncol(df) >= 2) names(df)[2] else NULL

      if (is.null(y_col)) stop("Spineplot needs two categorical variables (x and y)")

      # Convert to factors
      df[[x_col]] = as.factor(df[[x_col]])
      if (!is.null(y_col)) df[[y_col]] = as.factor(df[[y_col]])

      if (!is.null(df[["..wt.."]])) {
        spine_table = xtabs(df[["..wt.."]] ~ df[[y_col]] + df[[x_col]])
      } else {
        spine_table = table(df[[y_col]], df[[x_col]])
      }

      # Convert to proportions for ggplot2
      prop_df = as.data.frame(spine_table / rowSums(spine_table))
      names(prop_df) = c("y", "x", "Freq")

      p = ggplot2::ggplot(prop_df, ggplot2::aes(
        x = .data[["x"]], y = .data[["Freq"]], fill = .data[["y"]]
      )) +
        ggplot2::geom_col(position = "fill", width = 0.9, color = "white", linewidth = 0.3) +
        ggplot2::scale_y_continuous(labels = scales::percent) +
        ggplot2::labs(x = x_col, y = "Proportion", fill = y_col) +
        ggplot2::theme_classic()

      p
    },
    name = "spineplot"
  )
}

#' Rug Plot Type
#'
#' @description Adds rug marks (small tick lines) to indicate data density
#'   along the x and/or y axes.
#'
#' @param sides Which sides to draw rugs on: "b" (both), "l" (left/bottom),
#'   "t" (top), "r" (right), or combinations like "bl", "tr"
#' @param alpha Rug transparency
#' @param linewidth Rug line width
#'
#' @return A cliplot_type object
#' @export
type_rug = function(sides = "b", alpha = 0.5, linewidth = 0.3) {
  cliplot_type(
    draw = function(data, mapping, settings, ...) {
      p = ggplot2::ggplot(data, mapping) +
        ggplot2::geom_point(
          alpha = 0.6, size = 2.5,
          color = settings$col.default %||% "#2B6CB0"
        )

      if (grepl("b", sides) || grepl("l", sides)) {
        p = p + ggplot2::geom_rug(
          sides = sides, alpha = alpha, linewidth = linewidth,
          color = settings$col.default %||% "#2B6CB0"
        )
      } else {
        p = p + ggplot2::geom_rug(
          sides = sides, alpha = alpha, linewidth = linewidth,
          color = settings$col.default %||% "#2B6CB0"
        )
      }

      p
    },
    name = "rug"
  )
}

#' Reference Lines Type
#'
#' @description Adds horizontal, vertical, or diagonal reference lines to a plot.
#'
#' @param intercept Intercept for abline (default 0). For hline: y value(s).
#'   For vline: x value(s).
#' @param slope Slope for diagonal lines (only for abline)
#' @param linewidth Line width
#' @param linetype Line type ("solid", "dashed", "dotted", etc.)
#' @param color Line color
#'
#' @return A cliplot_type object
#' @export
type_abline = function(intercept = 0, slope = 0, linewidth = 0.5,
                        linetype = "dashed", color = "grey50") {
  cliplot_type(
    data = function(settings, ...) {
      df = settings$data
      if (is.null(df)) df = data.frame(x = settings$x, y = settings$y)
      else df = data.frame(
        x = settings$x %||% df[[1]],
        y = settings$y %||% df[[2]]
      )
      settings$abline_data = df
    },
    draw = function(data, mapping, settings, ...) {
      df = settings$abline_data
      if (is.null(df)) df = data

      p = ggplot2::ggplot(df, ggplot2::aes(x = .data[["x"]], y = .data[["y"]])) +
        ggplot2::geom_point(
          alpha = 0.6, size = 2.5,
          color = settings$col.default %||% "#2B6CB0"
        )

      # Add the reference line
      if (slope == 0) {
        p = p + ggplot2::geom_hline(
          yintercept = intercept,
          linewidth = linewidth,
          linetype = linetype,
          color = color
        )
      } else if (is.infinite(slope) || length(intercept) > 1) {
        p = p + ggplot2::geom_vline(
          xintercept = intercept,
          linewidth = linewidth,
          linetype = linetype,
          color = color
        )
      } else {
        p = p + ggplot2::geom_abline(
          intercept = intercept, slope = slope,
          linewidth = linewidth,
          linetype = linetype,
          color = color
        )
      }

      p
    },
    name = "abline"
  )
}

#' Q-Q Plot Type
#'
#' @description Creates quantile-quantile plots to assess normality or
#'   compare two distributions.
#'
#' @param distribution Target distribution function (e.g., stats::qnorm)
#' @param dparams List of distribution parameters
#' @param point_size Point size
#' @param point_alpha Point transparency
#' @param show_line Add reference line (default TRUE)
#' @param line_color Color for reference line
#'
#' @return A cliplot_type object
#' @export
type_qq = function(distribution = stats::qnorm, dparams = list(),
                    point_size = 2, point_alpha = 0.6,
                    show_line = TRUE, line_color = "#E64B35") {
  cliplot_type(
    draw = function(data, mapping, settings, ...) {
      x_col = if ("x" %in% names(data)) "x"
              else names(data)[1]

      p = ggplot2::ggplot(data, ggplot2::aes(sample = .data[[x_col]])) +
        ggplot2::stat_qq(
          distribution = distribution,
          dparams = dparams,
          size = point_size,
          alpha = point_alpha
        ) +
        ggplot2::stat_qq_line(
          distribution = distribution,
          dparams = dparams,
          color = line_color,
          linewidth = 0.8
        )

      p
    },
    name = "qq"
  )
}
