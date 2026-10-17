# ===========================================================================
# cliomicplot: Donut Chart Type
# Inspired by cnsplots donutplot() - proportional composition visualization
# ===========================================================================

#' Donut Chart Type
#'
#' @description Creates publication-ready donut (ring) charts for visualizing
#'   proportions. A pie chart with a central hole that can carry a summary
#'   label. Inspired by cnsplots' \code{donutplot()}.
#'
#' @param show_percent Label each slice with its percentage (default TRUE).
#' @param label_size Size of the percentage labels (default 3.5).
#' @param center_label Optional text drawn in the center hole (default NULL).
#' @param center_value Optional bold value drawn under the center label
#'   (default NULL).
#' @param border_color Color of the slice borders (default "white").
#' @param border_width Width of the slice borders (default 1).
#' @param hole_lo Lower bound of the radial scale (default 0). The visual
#'   hole spans from \code{hole_lo} to the inner edge of the ring.
#' @param hole_hi Upper bound of the ring (default 2.6).
#'
#' @return A \code{cliplot_type} object for use with \code{\link{cliplot}}.
#'
#' @details
#' Pass a categorical variable (\code{cliplot(~ group, data, type = "donut")})
#' to count proportions, or a data frame with a category column and an
#' optional value column.
#'
#' @examples
#' \dontrun{
#' cliplot(~ Species, data = iris, type = "donut")
#' cliplot(category ~ value, data = composition_df, type = "donut")
#' }
#'
#' @export
type_donut = function(
    show_percent  = TRUE,
    label_size    = 3.5,
    center_label  = NULL,
    center_value  = NULL,
    border_color  = "white",
    border_width  = 1,
    hole_lo       = 0,
    hole_hi       = 2.6
) {
  cliplot_type(
    data = function(settings, ...) {
      df = settings$data
      if (!is.null(df) && is.data.frame(df) &&
          (!is.null(settings$x) || ncol(df) >= 2)) {
        cat = settings$x %||% df[[1]]
        val = settings$y
        if (is.null(val)) {
          if (is.numeric(df[[2]])) {
            val = df[[2]]
          } else {
            val = rep(1, length(cat))
          }
        }
        d = stats::aggregate(val, by = list(category = cat), FUN = sum)
        names(d) = c("category", "value")
      } else {
        cat = settings$x
        if (is.null(cat)) {
          stop("type_donut needs a categorical variable.", call. = FALSE)
        }
        d = as.data.frame(table(category = cat), stringsAsFactors = FALSE)
        names(d) = c("category", "value")
      }
      d = d[d$value > 0, , drop = FALSE]
      d$fraction = d$value / sum(d$value)
      d$percent = 100 * d$fraction
      d$label = if (isTRUE(show_percent)) {
        sprintf("%.1f%%", d$percent)
      } else {
        as.character(d$value)
      }
      d$ymax = cumsum(d$fraction)
      d$ymin = c(0, d$ymax[-length(d$ymax)])
      d$ypos = (d$ymin + d$ymax) / 2
      d$category = factor(d$category, levels = d$category)

      settings$donut_data = d
      settings$donut_label_size = label_size
      settings$donut_center_label = center_label
      settings$donut_center_value = center_value
      settings$donut_border_color = border_color
      settings$donut_border_width = border_width
      settings$donut_hole_lo = hole_lo
      settings$donut_hole_hi = hole_hi
    },
    draw = function(data, mapping, settings, ...) {
      d = settings$donut_data
      if (is.null(d)) return(ggplot2::ggplot())

      p = ggplot2::ggplot(d, ggplot2::aes(
        x = 2,
        y = .data[["fraction"]],
        fill = .data[["category"]]
      )) +
        ggplot2::geom_col(
          width = 1,
          color = settings$donut_border_color,
          linewidth = settings$donut_border_width
        ) +
        ggplot2::geom_text(
          ggplot2::aes(y = .data[["ypos"]], label = .data[["label"]]),
          size = settings$donut_label_size,
          color = "white", fontface = "bold"
        ) +
        ggplot2::coord_polar(theta = "y", start = 0) +
        ggplot2::xlim(c(settings$donut_hole_lo, settings$donut_hole_hi)) +
        ggplot2::scale_fill_manual(
          values = get_cli_palette(settings$palette %||% "npg", nrow(d)),
          guide = ggplot2::guide_legend(title = NULL)
        ) +
        ggplot2::theme_void() +
        ggplot2::theme(
          legend.position = settings$legend %||% "right",
          legend.text = ggplot2::element_text(size = 10)
        )

      if (!is.null(settings$donut_center_label) ||
          !is.null(settings$donut_center_value)) {
        center_txt = paste(
          c(settings$donut_center_value, settings$donut_center_label),
          collapse = "\n"
        )
        p = p + ggplot2::annotate(
          "text", x = 0, y = 0, label = center_txt,
          size = if (!is.null(settings$donut_center_value)) 3.6 else 3.2,
          fontface = if (!is.null(settings$donut_center_value)) "bold" else "plain"
        )
      }

      p


    },
    name = "donut"
  )
}




