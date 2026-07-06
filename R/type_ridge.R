# ===========================================================================
# cliomicplot: Ridgeline Plot Type
# For visualizing distribution changes across groups (joy plots)
# Uses ggridges under the hood, falling back to faceted density
# ===========================================================================

#' Ridgeline Plot Type
#'
#' @description Creates ridgeline (joy) plots for visualizing distribution
#'   changes across ordered categories. Particularly effective for showing
#'   how a continuous variable varies across groups.
#'
#' @param scale Overall scaling factor for ridge heights (default 1.5)
#' @param alpha Fill transparency for ridges (default 0.6)
#' @param bandwidth Bandwidth for density estimation; NULL = auto
#' @param gradient Add vertical gradient fill within each ridge
#' @param quantile_lines Add quantile lines within ridges
#' @param quantiles Vector of quantiles to draw lines for (default c(0.25, 0.5, 0.75))
#' @param show_points Show underlying data points (default FALSE)
#' @param point_size Size of underlying data points
#' @param point_alpha Transparency of underlying points
#'
#' @return A cliplot_type object
#'
#' @examples
#' \dontrun{
#' # Classic ridgeline plot
#' cliplot(Temp ~ factor(Month), data = airquality, type = "ridge")
#'
#' # With gradient fill and quantile lines
#' cliplot(Sepal.Length ~ Species, data = iris,
#'         type = type_ridge(gradient = TRUE, quantile_lines = TRUE))
#' }
#'
#' @export
type_ridge = function(scale = 1.5, alpha = 0.6, bandwidth = NULL,
                       gradient = FALSE, quantile_lines = FALSE,
                       quantiles = c(0.25, 0.5, 0.75),
                       show_points = FALSE, point_size = 0.5,
                       point_alpha = 0.15) {
  cliplot_type(
    data = function(settings, ...) {
      df = settings$data
      if (is.null(df)) {
        df = data.frame(
          x = settings$x,
          y = settings$y %||% settings$x,
          stringsAsFactors = FALSE
        )
      } else {
        df = data.frame(
          x = settings$x %||% df[[1]],
          y = settings$y %||% (if (ncol(df) > 1) df[[2]] else NULL),
          stringsAsFactors = FALSE
        )
      }
      # Ensure the grouping factor is present
      if (!is.null(settings$by)) df$..by.. = as.factor(settings$by)
      settings$ridge_data = df
    },
    draw = function(data, mapping, settings, ...) {
      df = settings$ridge_data
      if (is.null(df)) df = data

      # Determine value (x_ridge) and group (y_ridge) columns
      # Ridge: x = numeric values, y = factor groups
      x_ridge = NULL  # numeric column
      y_ridge = NULL  # factor/group column

      # Try by column first
      if (!is.null(settings$by) && "..by.." %in% names(df)) {
        y_ridge = "..by.."
      }
      # Look for factor/character among first two columns
      for (cn in names(df)[1:min(2, ncol(df))]) {
        if (is.factor(df[[cn]]) || is.character(df[[cn]])) {
          y_ridge = cn
          break
        }
      }
      # Find numeric column
      for (cn in names(df)) {
        if (is.numeric(df[[cn]]) && cn != y_ridge) {
          x_ridge = cn
          break
        }
      }

      if (is.null(y_ridge)) {
        stop("Ridge plots need a grouping variable. Use `by` or provide a factor column.")
      }
      if (is.null(x_ridge)) x_ridge = setdiff(names(df), y_ridge)[1]

      df[[y_ridge]] = as.factor(df[[y_ridge]])

      p = ggplot2::ggplot(df, ggplot2::aes(
        x = .data[[x_ridge]], y = .data[[y_ridge]], fill = .data[[y_ridge]]
      ))

      if (requireNamespace("ggridges", quietly = TRUE)) {
        if (gradient) {
          p = p + ggridges::geom_density_ridges_gradient(
            scale = scale, alpha = alpha,
            bandwidth = bandwidth,
            quantile_lines = quantile_lines,
            quantiles = quantiles
          )
        } else {
          p = p + ggridges::geom_density_ridges(
            scale = scale, alpha = alpha,
            bandwidth = bandwidth,
            quantile_lines = quantile_lines,
            quantiles = quantiles
          )
        }

        if (show_points) {
          p = p + ggridges::geom_density_ridges(
            jittered_points = TRUE,
            position = ggridges::position_points_jitter(width = 0.05, height = 0),
            point_shape = 1, point_size = point_size, point_alpha = point_alpha,
            scale = scale, alpha = 0
          )
        }

        p = p + ggplot2::labs(x = x_ridge, y = "") +
          ggplot2::theme(legend.position = "none")

      } else {
        # Fallback: faceted density
        p = ggplot2::ggplot(df, ggplot2::aes(
          x = .data[[x_ridge]], fill = .data[[y_ridge]]
        )) +
          ggplot2::geom_density(alpha = alpha) +
          ggplot2::facet_wrap(
            stats::as.formula(paste("~", y_ridge)),
            ncol = 1, scales = "free_y", strip.position = "left"
          ) +
          ggplot2::labs(x = x_ridge, y = "") +
          ggplot2::theme(
            strip.background = ggplot2::element_blank(),
            strip.placement = "outside",
            legend.position = "none"
          )
      }

      p
    },
    name = "ridge"
  )
}
