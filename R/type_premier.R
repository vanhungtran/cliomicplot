# ===========================================================================
# cliomicplot Premier: Raincloud, Dumbbell, Lollipop, Beeswarm
# Stunning visualization types beyond what tinyplot offers
# ===========================================================================

#' Raincloud Plot Type
#'
#' @description Creates elegant raincloud plots combining a split-half violin,
#'   boxplot, and jittered raw data points. Popular in neuroscience and
#'   psychology for transparent data visualization.
#'
#' @param alpha Raincloud transparency (default 0.7)
#' @param point_size Size of jittered points (default 1.5)
#' @param point_alpha Transparency of points (default 0.4)
#' @param box_width Width of boxplot relative to violin (default 0.2)
#' @param adjust Bandwidth adjustment for density (default 1)
#' @param side Which side to draw the raincloud: "right", "left", or "both"
#'
#' @return A cliplot_type object
#' @export
type_raincloud = function(alpha = 0.7, point_size = 1.5, point_alpha = 0.4,
                           box_width = 0.2, adjust = 1, side = "right") {
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
      settings$rc_data = df
    },
    draw = function(data, mapping, settings, ...) {
      df = settings$rc_data
      if (is.null(df)) df = data

      x_col = if (is.factor(df[[1]]) || is.character(df[[1]])) names(df)[1] else "x"
      y_col = if (is.numeric(df[[2]]) || (!is.null(df$y) && is.numeric(df$y))) "y" else names(df)[2]
      group_col = if (!is.null(settings$by) && "..by.." %in% names(df)) "..by.." else x_col

      df[[x_col]] = factor(df[[x_col]])

      p = ggplot2::ggplot(df, ggplot2::aes(
        x = .data[[x_col]], y = .data[[y_col]], fill = .data[[group_col]]
      ))

      # Half violin
      side_map = c(right = "r", left = "l", both = "full")
      v_side = side_map[side]

      if (requireNamespace("ggdist", quietly = TRUE)) {
        p = p + ggdist::stat_halfeye(
          adjust = adjust, width = 0.6, .width = 0,
          justification = if (side == "right") -0.2 else 0.2,
          point_colour = NA, alpha = alpha
        )
      } else {
        p = p + ggplot2::geom_violin(
          alpha = alpha, trim = TRUE,
          draw_quantiles = NULL,
          position = ggplot2::position_dodge(0.8)
        )
      }

      # Boxplot (use position_dodge only, no explicit width)
      p = p + ggplot2::geom_boxplot(
        width = box_width, alpha = 0.5,
        outlier.shape = NA
      )

      # Jittered points
      p = p + ggplot2::geom_jitter(
        width = 0.1, size = point_size, alpha = point_alpha,
        color = "grey30"
      )

      p = p + ggplot2::labs(x = x_col, y = y_col) +
        ggplot2::theme(legend.position = "none")

      p
    },
    name = "raincloud"
  )
}

#' Dumbbell Plot Type
#'
#' @description Creates elegant dumbbell charts for comparing two time points
#'   or conditions. Shows change with connected points and magnitude.
#'
#' @param point_size Point size (default 3)
#' @param line_width Connecting line width (default 0.8)
#' @param color_first Color for first time point
#' @param color_second Color for second time point
#'
#' @return A cliplot_type object
#' @export
type_dumbbell = function(point_size = 3, line_width = 0.8,
                          color_first = "#4DBBD5", color_second = "#E64B35") {
  cliplot_type(
    data = function(settings, ...) {
      df = settings$data
      if (is.null(df)) stop("Dumbbell plots need a data frame with x (label), y (first), y2 (second)")
      df = data.frame(
        label = settings$x %||% df[[1]],
        before = settings$y %||% df[[2]],
        after  = if (ncol(df) >= 3) df[[3]] else stop("Need 3 columns: label, before, after")
      )
      df$label = factor(df$label, levels = df$label[order(df$after)])
      settings$db_data = df
    },
    draw = function(data, mapping, settings, ...) {
      df = settings$db_data
      if (is.null(df)) df = data

      p = ggplot2::ggplot(df) +
        # Connecting segments
        ggplot2::geom_segment(
          ggplot2::aes(x = .data[["label"]], xend = .data[["label"]],
                       y = .data[["before"]], yend = .data[["after"]]),
          color = "grey60", linewidth = line_width
        ) +
        # First point
        ggplot2::geom_point(
          ggplot2::aes(x = .data[["label"]], y = .data[["before"]]),
          size = point_size, color = color_first
        ) +
        # Second point
        ggplot2::geom_point(
          ggplot2::aes(x = .data[["label"]], y = .data[["after"]]),
          size = point_size, color = color_second
        ) +
        ggplot2::coord_flip() +
        ggplot2::labs(x = "", y = "") +
        ggplot2::theme_minimal() +
        ggplot2::theme(
          panel.grid.major.y = ggplot2::element_blank(),
          panel.grid.minor = ggplot2::element_blank()
        )

      p
    },
    name = "dumbbell"
  )
}

#' Lollipop Chart Type
#'
#' @description Creates stylish lollipop charts — a modern alternative to bar
#'   charts with circle endpoints on thin stems.
#'
#' @param point_size Circle size (default 4)
#' @param stem_width Stem line width (default 0.8)
#' @param stem_color Stem line color (default "grey60")
#' @param point_color Circle fill color
#'
#' @return A cliplot_type object
#' @export
type_lollipop = function(point_size = 4, stem_width = 0.8,
                          stem_color = "grey60", point_color = NULL) {
  cliplot_type(
    data = function(settings, ...) {
      df = settings$data
      if (is.null(df)) df = data.frame(
        label = settings$x %||% as.character(seq_along(settings$y)),
        value = settings$y
      )
      else df = data.frame(
        label = settings$x %||% df[[1]],
        value = settings$y %||% df[[2]]
      )
      df$label = factor(df$label, levels = df$label[order(df$value)])
      settings$lolli_data = df
    },
    draw = function(data, mapping, settings, ...) {
      df = settings$lolli_data
      if (is.null(df)) df = data

      p = ggplot2::ggplot(df, ggplot2::aes(
        x = .data[["label"]], y = .data[["value"]]
      )) +
        ggplot2::geom_segment(
          ggplot2::aes(xend = .data[["label"]], yend = 0),
          color = stem_color, linewidth = stem_width
        ) +
        ggplot2::geom_point(
          size = point_size,
          color = point_color %||% settings$col.default %||% "#2B6CB0",
          fill = ggplot2::alpha(point_color %||% "#2B6CB0", 0.8)
        ) +
        ggplot2::coord_flip() +
        ggplot2::labs(x = "", y = "") +
        ggplot2::theme_minimal() +
        ggplot2::theme(
          panel.grid.major.y = ggplot2::element_blank(),
          panel.grid.minor = ggplot2::element_blank()
        )

      p
    },
    name = "lollipop"
  )
}

#' Beeswarm Plot Type
#'
#' @description Creates beautiful beeswarm plots — points arranged to avoid
#'   overlap while showing the full distribution. More informative than
#'   boxplots alone.
#'
#' @param point_size Point size (default 2)
#' @param point_alpha Point transparency (default 0.7)
#' @param spacing Point spacing (default 0.5)
#' @param method Beeswarm method: "swarm", "compactswarm", "hex", "square"
#'
#' @return A cliplot_type object
#' @export
type_beeswarm = function(point_size = 2, point_alpha = 0.7,
                          spacing = 0.5, method = "swarm") {
  cliplot_type(
    data = function(settings, ...) {
      df = settings$data
      if (is.null(df)) df = data.frame(
        x = settings$x, y = settings$y
      )
      else df = data.frame(
        x = settings$x %||% df[[1]],
        y = settings$y %||% df[[2]]
      )
      if (!is.null(settings$by)) df$..by.. = as.factor(settings$by)
      settings$bee_data = df
    },
    draw = function(data, mapping, settings, ...) {
      df = settings$bee_data
      if (is.null(df)) df = data

      x_col = if (!is.null(settings$by) && "..by.." %in% names(df)) "..by.."
              else names(df)[1]
      y_col = if (is.numeric(df[[2]])) names(df)[2] else "y"

      df[[x_col]] = factor(df[[x_col]])

      p = ggplot2::ggplot(df, ggplot2::aes(
        x = .data[[x_col]], y = .data[[y_col]], color = .data[[x_col]]
      ))

      if (requireNamespace("ggbeeswarm", quietly = TRUE)) {
        p = p + ggbeeswarm::geom_beeswarm(
          size = point_size, alpha = point_alpha,
          cex = spacing, method = method
        )
      } else {
        # Fallback: violin + jitter combo
        p = p + ggplot2::geom_violin(
          alpha = 0.1, color = NA, fill = "grey50"
        ) +
          ggplot2::geom_jitter(
            width = 0.15, size = point_size, alpha = point_alpha
          )
      }

      p = p + ggplot2::labs(x = x_col, y = y_col) +
        ggplot2::theme(legend.position = "none")

      p
    },
    name = "beeswarm"
  )
}
