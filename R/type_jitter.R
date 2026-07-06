# ===========================================================================
# cliomicplot: Jitter Plot Type
# For adding random noise to avoid overplotting
# ===========================================================================

#' Jitter Plot Type
#'
#' @description Creates scatter plots with random jitter to reduce overplotting.
#'   Useful for visualizing discrete or rounded continuous data.
#'
#' @param alpha Point transparency (0-1)
#' @param size Point size
#' @param width Amount of horizontal jitter (default 0.2)
#' @param height Amount of vertical jitter (default 0)
#' @param stroke Point outline stroke width
#'
#' @return A cliplot_type object for use with \code{\link{cliplot}}.
#'
#' @examples
#' \dontrun{
#' # Jitter points to show density
#' cliplot(mpg ~ factor(cyl), data = mtcars, type = "jitter")
#'
#' # Custom jitter
#' cliplot(mpg ~ factor(cyl), data = mtcars,
#'         type = type_jitter(width = 0.3, height = 0.1))
#' }
#'
#' @export
type_jitter = function(alpha = NULL, size = NULL, width = 0.2, height = 0,
                        stroke = 0.35) {
  cliplot_type(
    draw = function(data, mapping, settings, ...) {
      p = ggplot2::ggplot(data, mapping)
      if (!is.null(settings$by)) {
        p = p + ggplot2::geom_jitter(
          ggplot2::aes(color = .data[["..by.."]]),
          alpha  = alpha %||% 0.7,
          size   = size %||% 2.5,
          width  = width,
          height = height,
          stroke = stroke
        )
      } else {
        p = p + ggplot2::geom_jitter(
          alpha  = alpha %||% 0.7,
          size   = size %||% 2.5,
          width  = width,
          height = height,
          color  = settings$col.default %||% "#2B6CB0",
          stroke = stroke
        )
      }
      p
    },
    name = "jitter"
  )
}
