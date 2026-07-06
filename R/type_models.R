# ===========================================================================
# cliomicplot: Model-Fitting Plot Types (lm, loess, spline)
# Add regression lines, smooth curves, and confidence bands
# ===========================================================================

#' Linear Model Fit Type
#'
#' @description Adds a linear regression line with optional confidence band.
#'   Useful for scatter plots with trend lines.
#'
#' @param se Show confidence band (default TRUE)
#' @param level Confidence level for the band (default 0.95)
#' @param linewidth Line width (default 1)
#' @param alpha Confidence band transparency (default 0.15)
#' @param formula Model formula override (default y ~ x)
#'
#' @return A cliplot_type object
#'
#' @examples
#' \dontrun{
#' cliplot(mpg ~ wt, data = mtcars, type = "lm")
#' cliplot(mpg ~ wt, data = mtcars, type = type_lm(level = 0.9))
#' }
#'
#' @export
type_lm = function(se = TRUE, level = 0.95, linewidth = 1, alpha = 0.15,
                    formula = y ~ x) {
  cliplot_type(
    data = function(settings, ...) {
      df = settings$data
      if (is.null(df)) {
        df = data.frame(x = settings$x, y = settings$y)
      } else {
        df = data.frame(
          x = settings$x %||% df[[1]],
          y = settings$y %||% df[[2]]
        )
      }
      if (!is.null(settings$by)) df$..by.. = as.factor(settings$by)
      settings$model_data = df
    },
    draw = function(data, mapping, settings, ...) {
      df = settings$model_data
      if (is.null(df)) df = data

      p = ggplot2::ggplot(df, ggplot2::aes(x = .data[["x"]], y = .data[["y"]])) +
        ggplot2::geom_smooth(
          method = "lm", formula = formula,
          se = se, level = level,
          linewidth = linewidth,
          alpha = alpha
        ) +
        ggplot2::geom_point(
          alpha = 0.6, size = 2.5,
          color = settings$col.default %||% "#2B6CB0"
        )

      if (!is.null(settings$by)) {
        p = ggplot2::ggplot(df, ggplot2::aes(
          x = .data[["x"]], y = .data[["y"]],
          color = .data[["..by.."]], fill = .data[["..by.."]]
        )) +
          ggplot2::geom_smooth(
            method = "lm", formula = formula,
            se = se, level = level,
            linewidth = linewidth,
            alpha = alpha
          ) +
          ggplot2::geom_point(alpha = 0.6, size = 2.5)
      }

      p
    },
    name = "lm"
  )
}

#' LOESS Smoothing Type
#'
#' @description Adds a LOESS (locally estimated scatterplot smoothing) curve
#'   with optional confidence band.
#'
#' @param se Show confidence band (default TRUE)
#' @param level Confidence level (default 0.95)
#' @param span Smoothing span; smaller = more wiggly (default 0.75)
#' @param linewidth Line width (default 1)
#' @param alpha Confidence band transparency (default 0.15)
#'
#' @return A cliplot_type object
#'
#' @examples
#' \dontrun{
#' cliplot(mpg ~ wt, data = mtcars, type = "loess")
#' }
#'
#' @export
type_loess = function(se = TRUE, level = 0.95, span = 0.75,
                       linewidth = 1, alpha = 0.15) {
  cliplot_type(
    data = function(settings, ...) {
      df = settings$data
      if (is.null(df)) df = data.frame(x = settings$x, y = settings$y)
      else df = data.frame(
        x = settings$x %||% df[[1]],
        y = settings$y %||% df[[2]]
      )
      if (!is.null(settings$by)) df$..by.. = as.factor(settings$by)
      settings$model_data = df
    },
    draw = function(data, mapping, settings, ...) {
      df = settings$model_data
      if (is.null(df)) df = data

      p = ggplot2::ggplot(df, ggplot2::aes(x = .data[["x"]], y = .data[["y"]])) +
        ggplot2::geom_smooth(
          method = "loess", formula = y ~ x,
          se = se, level = level, span = span,
          linewidth = linewidth, alpha = alpha
        ) +
        ggplot2::geom_point(
          alpha = 0.6, size = 2.5,
          color = settings$col.default %||% "#2B6CB0"
        )

      if (!is.null(settings$by)) {
        p = ggplot2::ggplot(df, ggplot2::aes(
          x = .data[["x"]], y = .data[["y"]],
          color = .data[["..by.."]], fill = .data[["..by.."]]
        )) +
          ggplot2::geom_smooth(
            method = "loess", formula = y ~ x,
            se = se, level = level, span = span,
            linewidth = linewidth, alpha = alpha
          ) +
          ggplot2::geom_point(alpha = 0.6, size = 2.5)
      }

      p
    },
    name = "lm"  # share legend behavior with lm
  )
}
