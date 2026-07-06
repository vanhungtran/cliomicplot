# ===========================================================================
# cliomicplot: Bubble Chart Type
# For visualizing three continuous variables (x, y, size)
# ===========================================================================

#' Bubble Chart Type
#'
#' @description Creates bubble charts where point size encodes a third variable.
#'   Uses the \code{size} aesthetic in ggplot2. The size variable can be
#'   provided via the \code{z} parameter or via the formula interface
#'   as \code{z ~ x | by} with \code{y} passed separately.
#'
#' @param z Variable or column name for the size dimension (bubble radius).
#'   If NULL, tries to find a third numeric column in the data.
#' @param alpha Point transparency (0-1)
#' @param scale_max Maximum bubble size in mm (default 15)
#' @param stroke Point outline stroke width
#' @param scale_size Logical; apply sqrt scaling to area (default TRUE)
#'
#' @return A cliplot_type object for use with \code{\link{cliplot}}.
#'
#' @examples
#' \dontrun{
#' # Bubble chart with built-in dataset
#' cliplot(mpg ~ wt, data = mtcars,
#'         type = type_bubble(z = mtcars$hp))
#'
#' # Custom bubble scaling
#' cliplot(Sepal.Length ~ Sepal.Width, data = iris,
#'         type = type_bubble(z = iris$Petal.Length, scale_max = 20))
#' }
#'
#' @export
type_bubble = function(z = NULL, alpha = 0.65, scale_max = 15, stroke = 0.35,
                        scale_size = TRUE) {
  cliplot_type(
    data = function(settings, ...) {
      # Build from settings x/y
      df = data.frame(
        x = settings$x,
        y = settings$y,
        stringsAsFactors = FALSE
      )

      # Resolve z variable for bubble size
      z_vals = z
      if (is.null(z_vals)) {
        if (!is.null(settings$extra_data)) {
          z_vals = settings$extra_data[[1]]
        } else {
          # Try to find a third numeric column from original data
          orig = settings$data
          if (is.data.frame(orig)) {
            num_cols = names(orig)[vapply(orig, is.numeric, logical(1))]
            # Find a column not matching the formula variables
            used = c(deparse(substitute(settings$x)), deparse(substitute(settings$y)))
            for (cn in num_cols) {
              if (!cn %in% used) {
                z_vals = orig[[cn]]
                break
              }
            }
          }
        }
      } else if (is.character(z) && length(z) == 1 && !is.null(settings$data) && z %in% names(settings$data)) {
        z_vals = settings$data[[z]]
      }

      df$..size.. = as.numeric(z_vals) %||% rep(1, nrow(df))
      if (scale_size) {
        df$..size.. = sqrt(pmax(df$..size.., 0.01))
      }

      # Add by column if grouping is used
      if (!is.null(settings$by)) {
        df$..by.. = as.factor(settings$by)
      }

      settings$bubble_data = df
    },
    draw = function(data, mapping, settings, ...) {
      df = settings$bubble_data
      if (is.null(df)) df = data
      if (!"..size.." %in% names(df)) {
        df$..size.. = 3
      }

      # Add size mapping to the default mapping
      mapping$size = quote(.data[["..size.."]])

      p = ggplot2::ggplot(df, mapping)

      if (!is.null(settings$by)) {
        p = p + ggplot2::geom_point(
          ggplot2::aes(color = .data[["..by.."]]),
          alpha = alpha,
          stroke = stroke
        )
      } else {
        p = p + ggplot2::geom_point(
          alpha = alpha,
          color = settings$col.default %||% "#2B6CB0",
          stroke = stroke
        )
      }

      p = p + ggplot2::scale_size_continuous(
        range = c(1, scale_max),
        guide = ggplot2::guide_legend(title = "Size")
      )

      p
    },
    name = "bubble"
  )
}
