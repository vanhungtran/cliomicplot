# ===========================================================================
# cliomicplot: Split / Half Violin Geom
# ===========================================================================

#' Split and Half Violin Geom
#'
#' @description A ggplot2 extension geom that renders split violins (alternating
#'   left and right halves for comparing two conditions) or directional half
#'   violins with integrated quantile lines.
#'
#' @param mapping Set of aesthetic mappings created by \code{\link[ggplot2]{aes}}.
#' @param data The data to be displayed in this layer.
#' @param stat The statistical transformation to use on the data for this layer
#'   (default \code{"ydensity"}).
#' @param position Position adjustment (default \code{"identity"}).
#' @param ... Other arguments passed on to \code{\link[ggplot2]{layer}}.
#' @param quantiles Numeric vector of quantiles to draw inside the violin
#'   (e.g., \code{c(0.25, 0.5, 0.75)}).
#' @param quantile_color Color for quantile lines (default \code{"grey25"}).
#' @param quantile_linewidth Line width for quantile lines (default 0.45).
#' @param quantile_linetype Line type for quantile lines (default \code{"solid"}).
#' @param side Drawing side: \code{"split"} (alternates left/right by group),
#'   \code{"left"}, \code{"right"}, or \code{"both"}.
#' @param trim If \code{TRUE} (default), trim the tails of the violins to the
#'   observed data range.
#' @param scale Density scaling method: \code{"width"} (default), \code{"area"},
#'   or \code{"count"}.
#' @param na.rm If \code{FALSE} (default), missing values are removed with a warning.
#' @param show.legend Logical. Should this layer be included in the legends?
#' @param inherit.aes If \code{FALSE}, overrides the default aesthetics.
#'
#' @return A ggplot2 layer.
#' @export
geom_split_violin <- function(
    mapping           = NULL,
    data              = NULL,
    stat              = "ydensity",
    position          = "identity",
    ...,
    quantiles         = NULL,
    quantile_color    = "grey25",
    quantile_linewidth = 0.45,
    quantile_linetype = "solid",
    side              = "split",
    trim              = TRUE,
    scale             = "width",
    na.rm             = FALSE,
    show.legend       = NA,
    inherit.aes       = TRUE
) {
  ggplot2::layer(
    data        = data,
    mapping     = mapping,
    stat        = stat,
    geom        = GeomSplitViolin,
    position    = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params      = list(
      quantiles          = quantiles,
      quantile_color     = quantile_color,
      quantile_linewidth = quantile_linewidth,
      quantile_linetype  = quantile_linetype,
      side               = side,
      trim               = trim,
      scale              = scale,
      na.rm              = na.rm,
      ...
    )
  )
}

#' @rdname geom_split_violin
#' @format NULL
#' @usage NULL
#' @export
GeomSplitViolin <- ggplot2::ggproto("GeomSplitViolin", ggplot2::GeomViolin,
  extra_params = c(
    "na.rm", "orientation", "lineend", "linejoin", "linemitre",
    "quantiles", "draw_quantiles", "quantile_color", "quantile_colour",
    "quantile_linewidth", "quantile_linetype", "side"
  ),
  setup_data = function(self, data, params) {
    data <- ggplot2::GeomViolin$setup_data(data, params)
    if (is.null(params$side) || params$side == "split") {
      # Within each x position, rank groups so condition 1 is always left, condition 2 is always right
      data$split_side <- "left"
      for (x_val in unique(data$x)) {
        idx <- which(data$x == x_val)
        grps <- sort(unique(data$group[idx]))
        if (length(grps) >= 2) {
          right_idx <- which(data$x == x_val & data$group == grps[2])
          data$split_side[right_idx] <- "right"
        }
      }
    } else {
      data$split_side <- params$side
    }
    data
  },
  draw_group = function(
    self,
    data,
    panel_params,
    coord,
    ...,
    quantiles          = NULL,
    draw_quantiles     = NULL,
    quantile_color     = "grey25",
    quantile_colour    = NULL,
    quantile_linewidth = 0.45,
    quantile_linetype  = "solid",
    side               = "split"
  ) {
    quantile_color <- quantile_colour %||% quantile_color
    # Compute horizontal boundaries from kernel density output
    data <- transform(data,
      xminv = x - violinwidth * (x - xmin),
      xmaxv = x + violinwidth * (xmax - x)
    )

    # Resolve active side for this group
    actual_side <- data$split_side[1] %||% side

    if (actual_side == "left") {
      # Left half: along outer curve (xminv), then down the central spine (x)
      newdata <- rbind(
        transform(data, x = xminv)[order(data$y), ],
        transform(data, x = x)[order(data$y, decreasing = TRUE), ]
      )
    } else if (actual_side == "right") {
      # Right half: up the central spine (x), then down the outer curve (xmaxv)
      newdata <- rbind(
        transform(data, x = x)[order(data$y), ],
        transform(data, x = xmaxv)[order(data$y, decreasing = TRUE), ]
      )
    } else {
      # Standard full violin
      newdata <- rbind(
        transform(data, x = xminv)[order(data$y), ],
        transform(data, x = xmaxv)[order(data$y, decreasing = TRUE), ]
      )
    }
    newdata <- rbind(newdata, newdata[1, ])

    # Render polygon directly via GeomPolygon panel method
    poly_grob <- ggplot2::GeomPolygon$draw_panel(newdata, panel_params, coord, ...)

    # Draw quantiles if specified
    q_target <- quantiles %||% draw_quantiles
    if (!is.null(q_target) && length(q_target) > 0 && !scales::zero_range(range(data$y))) {
      probs <- sort(q_target)
      qs <- stats::quantile(data$y, probs = probs, na.rm = TRUE)

      x_center <- data$x[1]
      x_span <- (data$xmax[1] - data$xmin[1]) / 2

      q_lines <- lapply(qs, function(q_val) {
        approx_w <- stats::approx(data$y, data$violinwidth, xout = q_val, rule = 2)$y
        if (actual_side == "left") {
          x_end <- x_center - approx_w * x_span
          data.frame(x = c(x_end, x_center), y = c(q_val, q_val))
        } else if (actual_side == "right") {
          x_end <- x_center + approx_w * x_span
          data.frame(x = c(x_center, x_end), y = c(q_val, q_val))
        } else {
          x_l <- x_center - approx_w * x_span
          x_r <- x_center + approx_w * x_span
          data.frame(x = c(x_l, x_r), y = c(q_val, q_val))
        }
      })
      q_df <- do.call(rbind, q_lines)
      q_df$group <- rep(seq_along(qs), each = 2)
      q_df$colour <- quantile_color
      q_df$linewidth <- quantile_linewidth
      q_df$linetype <- quantile_linetype
      q_df$alpha <- 0.9

      line_grob <- ggplot2::GeomPath$draw_panel(q_df, panel_params, coord, ...)
      grid::grobTree(poly_grob, line_grob)
    } else {
      poly_grob
    }
  }
)
