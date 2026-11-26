# ===========================================================================
# cliomicplot Premier: Raincloud, Dumbbell, Lollipop, Beeswarm
# Stunning visualization types beyond what tinyplot offers
# ===========================================================================

#' Raincloud and Half-Boxed Ecology Plot Type
#'
#' @description Creates publication-ready raincloud and half-boxed distribution plots
#'   combining half-violins (density slabs), vertical boxplots, mean markers, and optional
#'   statistical summary confidence interval annotations.
#'
#' @param style Aesthetic style: `"default"` (classic neuroscience raincloud with jittered points)
#'   or `"ecology"` / `"half_boxed"` (clean ecology/demography style with white-filled boxplot,
#'   category-matched borders, open square outliers, prominent circular mean dot, right-facing
#'   half-violin density, horizontal reference line, and bottom-positioned mean + 95 percent CI text annotations).
#' @param alpha Violin transparency (default 0.7 for default, 0.98 for ecology).
#' @param point_size Size of jittered points in default style (default 1.5).
#' @param point_alpha Transparency of jittered points in default style (default 0.4).
#' @param box_width Width of boxplot (default 0.2 for default, 0.17 for ecology).
#' @param box_nudge Horizontal nudge for boxplot in ecology style (default -0.15).
#' @param box_fill Fill color of boxplot (default white `"#FFFFFF"` in ecology style, semi-transparent in default).
#' @param box_lwd Line width for boxplot borders and whiskers (default 1.1 for ecology style).
#' @param outlier_shape Outlier point shape (default `0` [open square] for ecology style, `NA` for default).
#' @param outlier_size Outlier point size (default 2.0 for ecology style).
#' @param outlier_stroke Outlier border stroke thickness (default 0.8 for ecology style).
#' @param show_mean Logical. Draw a solid circular mean marker between boxplot and violin (default TRUE for ecology style).
#' @param mean_nudge Horizontal nudge for mean point (default -0.01).
#' @param mean_shape Mean marker shape (default 16, solid circle).
#' @param mean_size Mean marker size (default 3.8).
#' @param violin_width Width of half-violin slab (default 0.52).
#' @param adjust Bandwidth adjustment for density estimation (default 1.1).
#' @param side Which side to draw the half-violin: `"right"`, `"left"`, or `"both"`.
#' @param ref_line Numeric y-intercept for horizontal dashed reference line (e.g. `1.0` for population replacement rate). `NULL` to disable.
#' @param ref_line_col Color for reference line (default `"grey70"`).
#' @param ref_line_lty Linetype for reference line (default `"dashed"`).
#' @param ref_line_lwd Linewidth for reference line (default 1.2).
#' @param show_stats Logical. Compute and display sample mean and confidence intervals below each category (default TRUE for ecology style).
#' @param stat_param Parameter symbol or name for stats label (default `"λ"`, or `"mean"`).
#' @param stat_ci_level Confidence interval level (default 0.95 for 95 percent CI).
#' @param stat_digits Number of decimal digits to format in stats label (default 3).
#' @param stat_y_pos Custom y-coordinate for bottom stats labels. `NULL` automatically calculates position below minimum data range.
#' @param stat_text_size Font size for bottom stats labels (default 3.4).
#' @param stat_labels Optional custom character vector to override calculated stats labels.
#'
#' @return A cliplot_type object.
#' @export
type_raincloud = function(
    style          = c("default", "ecology", "half_boxed"),
    alpha          = NULL,
    point_size     = 1.5,
    point_alpha    = 0.4,
    box_width      = NULL,
    box_nudge      = NULL,
    box_fill       = NULL,
    box_lwd        = NULL,
    outlier_shape  = NULL,
    outlier_size   = NULL,
    outlier_stroke = NULL,
    show_mean      = NULL,
    mean_nudge     = -0.01,
    mean_shape     = 16,
    mean_size      = 3.8,
    violin_width   = 0.52,
    adjust         = 1.1,
    side           = "right",
    ref_line       = NULL,
    ref_line_col   = "grey70",
    ref_line_lty   = "dashed",
    ref_line_lwd   = 1.2,
    show_stats     = NULL,
    stat_param     = "\u03bb",
    stat_ci_level  = 0.95,
    stat_digits    = 3,
    stat_y_pos     = NULL,
    stat_text_size = 3.4,
    stat_labels    = NULL
) {
  if (is.character(style)) {
    style <- match.arg(style)
  } else {
    style <- "default"
  }
  is_ecology <- style %in% c("ecology", "half_boxed")

  # Style-specific defaults
  if (is_ecology) {
    alpha          <- alpha %||% 0.98
    box_width      <- box_width %||% 0.17
    box_nudge      <- box_nudge %||% -0.15
    box_fill       <- box_fill %||% "#FFFFFF"
    box_lwd        <- box_lwd %||% 1.1
    outlier_shape  <- outlier_shape %||% 0
    outlier_size   <- outlier_size %||% 2.0
    outlier_stroke <- outlier_stroke %||% 0.8
    show_mean      <- show_mean %||% TRUE
    ref_line       <- ref_line %||% 1.0
    show_stats     <- show_stats %||% TRUE
  } else {
    alpha          <- alpha %||% 0.7
    box_width      <- box_width %||% 0.2
    box_nudge      <- box_nudge %||% 0
    box_fill       <- box_fill %||% NA
    box_lwd        <- box_lwd %||% 0.6
    outlier_shape  <- outlier_shape %||% NA
    outlier_size   <- outlier_size %||% 1.5
    outlier_stroke <- outlier_stroke %||% 0.5
    show_mean      <- show_mean %||% FALSE
    show_stats     <- show_stats %||% FALSE
  }

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
        x = .data[[x_col]], y = .data[[y_col]], fill = .data[[group_col]], color = .data[[group_col]]
      ))

      # Reference horizontal baseline (rendered behind distributions)
      if (!is.null(ref_line) && is.numeric(ref_line)) {
        p = p + ggplot2::geom_hline(
          yintercept = ref_line,
          linetype   = ref_line_lty,
          color      = ref_line_col,
          linewidth  = ref_line_lwd
        )
      }

      # Half violin / slab
      if (requireNamespace("ggdist", quietly = TRUE)) {
        just_val <- if (side == "right") -0.18 else 0.18
        p = p + ggdist::stat_halfeye(
          adjust        = adjust,
          width         = violin_width,
          .width        = 0,
          justification = just_val,
          point_colour  = NA,
          color         = NA,
          alpha         = alpha
        )
      } else {
        # Base R + ggplot2 density polygon fallback
        x_levels  <- levels(df[[x_col]])
        poly_rows <- list()
        for (i in seq_along(x_levels)) {
          lvl   <- x_levels[i]
          sub_y <- df[[y_col]][df[[x_col]] == lvl & !is.na(df[[y_col]])]
          if (length(sub_y) >= 2) {
            d <- stats::density(sub_y, adjust = adjust)
            max_d <- max(d$y, na.rm = TRUE)
            scaled_x <- if (max_d > 0) (d$y / max_d) * (violin_width * 0.72) else rep(0, length(d$y))
            poly_rows[[length(poly_rows) + 1]] <- data.frame(
              x     = c(rep(i + 0.04, length(d$x)), rev(i + 0.04 + scaled_x)),
              y     = c(d$x, rev(d$x)),
              group = lvl,
              stringsAsFactors = FALSE
            )
          }
        }
        if (length(poly_rows) > 0) {
          poly_df <- do.call(rbind, poly_rows)
          poly_df$group <- factor(poly_df$group, levels = x_levels)
          p = p + ggplot2::geom_polygon(
            data        = poly_df,
            mapping     = ggplot2::aes(x = .data[["x"]], y = .data[["y"]], fill = .data[["group"]]),
            color       = NA,
            alpha       = alpha,
            inherit.aes = FALSE
          )
        }
      }

      # Boxplot
      if (is_ecology) {
        p = p + ggplot2::geom_boxplot(
          width          = box_width,
          fill           = box_fill,
          linewidth      = box_lwd,
          outlier.shape  = outlier_shape,
          outlier.size   = outlier_size,
          outlier.stroke = outlier_stroke,
          position       = ggplot2::position_nudge(x = box_nudge)
        )
      } else {
        p = p + ggplot2::geom_boxplot(
          width         = box_width,
          alpha         = 0.5,
          color         = "black",
          outlier.shape = NA
        )
        # Jittered raw observations in default style
        p = p + ggplot2::geom_jitter(
          width = 0.1,
          size  = point_size,
          alpha = point_alpha,
          color = "grey30"
        )
      }

      # Mean point marker (ecology style)
      if (isTRUE(show_mean)) {
        p = p + ggplot2::stat_summary(
          fun      = mean,
          geom     = "point",
          shape    = mean_shape,
          size     = mean_size,
          position = ggplot2::position_nudge(x = mean_nudge)
        )
      }

      # Summary statistics & 95% CI text labels at bottom
      if (isTRUE(show_stats)) {
        x_levels   <- levels(df[[x_col]])
        stats_rows <- list()
        y_all      <- df[[y_col]]
        y_range    <- range(y_all, na.rm = TRUE)
        y_span     <- diff(y_range)
        text_y     <- stat_y_pos %||% (y_range[1] - y_span * 0.14)

        for (i in seq_along(x_levels)) {
          lvl   <- x_levels[i]
          vals  <- df[[y_col]][df[[x_col]] == lvl & !is.na(df[[y_col]])]
          n_obs <- length(vals)
          if (n_obs > 0) {
            m      <- mean(vals)
            se     <- if (n_obs > 1) stats::sd(vals) / sqrt(n_obs) else 0
            t_crit <- if (n_obs > 1) stats::qt(1 - (1 - stat_ci_level) / 2, df = n_obs - 1) else 1.96
            ci_low <- m - t_crit * se
            ci_high <- m + t_crit * se

            lbl <- if (!is.null(stat_labels) && length(stat_labels) >= i) {
              stat_labels[i]
            } else {
              sprintf(paste0("%s = %.", stat_digits, "f\n(%.0f%% CI: %.", stat_digits, "f - %.", stat_digits, "f)"),
                      stat_param, m, stat_ci_level * 100, ci_low, ci_high)
            }
            stats_rows[[length(stats_rows) + 1]] <- data.frame(
              x = lvl,
              y = text_y,
              label = lbl,
              stringsAsFactors = FALSE
            )
          }
        }
        if (length(stats_rows) > 0) {
          stats_df   <- do.call(rbind, stats_rows)
          stats_df$x <- factor(stats_df$x, levels = x_levels)
          p = p + ggplot2::geom_text(
            data        = stats_df,
            mapping     = ggplot2::aes(x = .data[["x"]], y = .data[["y"]], label = .data[["label"]]),
            color       = "black",
            size        = stat_text_size,
            lineheight  = 0.95,
            inherit.aes = FALSE
          )
        }
      }

      p = p + ggplot2::labs(x = x_col, y = y_col) +
        ggplot2::theme(legend.position = "none")

      p
    },
    name = if (is_ecology) "ecology_raincloud" else "raincloud"
  )
}

#' Ecology Half-Boxed Raincloud Plot Type
#'
#' @description Creates publication-ready ecology and demographic viability raincloud plots
#'   featuring a white-filled boxplot with colored outline, open square outliers, a prominent
#'   circular mean marker, a solid half-violin density slab, a dashed reference baseline,
#'   and bottom-positioned mean + 95 percent CI text annotations.
#'
#' @inheritParams type_raincloud
#'
#' @return A cliplot_type object.
#' @export
type_ecology_raincloud = function(
    alpha          = 0.98,
    box_width      = 0.17,
    box_nudge      = -0.15,
    box_fill       = "#FFFFFF",
    box_lwd        = 1.1,
    outlier_shape  = 0,
    outlier_size   = 2.0,
    outlier_stroke = 0.8,
    show_mean      = TRUE,
    mean_nudge     = -0.01,
    mean_shape     = 16,
    mean_size      = 3.8,
    violin_width   = 0.52,
    adjust         = 1.1,
    side           = "right",
    ref_line       = 1.0,
    ref_line_col   = "grey70",
    ref_line_lty   = "dashed",
    ref_line_lwd   = 1.2,
    show_stats     = TRUE,
    stat_param     = "\u03bb",
    stat_ci_level  = 0.95,
    stat_digits    = 3,
    stat_y_pos     = NULL,
    stat_text_size = 3.4,
    stat_labels    = NULL
) {
  type_raincloud(
    style          = "ecology",
    alpha          = alpha,
    box_width      = box_width,
    box_nudge      = box_nudge,
    box_fill       = box_fill,
    box_lwd        = box_lwd,
    outlier_shape  = outlier_shape,
    outlier_size   = outlier_size,
    outlier_stroke = outlier_stroke,
    show_mean      = show_mean,
    mean_nudge     = mean_nudge,
    mean_shape     = mean_shape,
    mean_size      = mean_size,
    violin_width   = violin_width,
    adjust         = adjust,
    side           = side,
    ref_line       = ref_line,
    ref_line_col   = ref_line_col,
    ref_line_lty   = ref_line_lty,
    ref_line_lwd   = ref_line_lwd,
    show_stats     = show_stats,
    stat_param     = stat_param,
    stat_ci_level  = stat_ci_level,
    stat_digits    = stat_digits,
    stat_y_pos     = stat_y_pos,
    stat_text_size = stat_text_size,
    stat_labels    = stat_labels
  )
}

#' @rdname type_ecology_raincloud
#' @export
type_half_boxed = type_ecology_raincloud

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
