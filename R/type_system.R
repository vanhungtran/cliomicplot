# ===========================================================================
# cliomicplot: Type System
# Each plot type is a `cliplot_type` S3 object with:
#   - draw: function that creates the ggplot
#   - data: function that preprocesses data
#   - name: string identifier
# Inspired by tinyplot's type system (type_points, type_boxplot, etc.)
# ===========================================================================

# Constructor for cliplot types
cliplot_type = function(draw, data = NULL, name = "custom") {
  out = list(
    draw = draw,
    data = data,
    name = name
  )
  class(out) = "cliplot_type"
  return(out)
}

# Default type dispatcher: choose based on data characteristics
# Inspired by tinyplot's sanitize_type() auto-detection logic
default_type = function(x, y) {
  # One-variable: x only
  if (is.null(y)) {
    if (is.numeric(x)) return("histogram")
    if (is.factor(x) || is.character(x)) {
      # Count unique values; few levels → barplot, many → (no default, use bar)
      if (is.factor(x) && nlevels(x) <= 20) return("barplot")
      return("barplot")
    }
    return("points")
  }

  # Two-variable: both x and y
  # Numeric vs factor → boxplot or jitter
  if ((is.factor(x) || is.character(x)) && is.numeric(y)) {
    return("boxplot")
  }
  if (is.numeric(x) && (is.factor(y) || is.character(y))) {
    return("boxplot")
  }
  # Both numeric → points
  if (is.numeric(x) && is.numeric(y)) {
    # If many tied values on x, jitter may be better
    if (is.numeric(x) && length(unique(x)) < 10 && length(x) > 50) {
      return("jitter")
    }
    return("points")
  }
  # Both factors → consider spineplot or barplot
  if ((is.factor(x) || is.character(x)) && (is.factor(y) || is.character(y))) {
    return("barplot")
  }
  # Fallback
  return("points")
}

# ==========================================================================
# Type: Points (Scatter)
# ==========================================================================

#' Scatter plot type
#' @param alpha Point transparency (0-1)
#' @param size Point size
#' @param stroke Point outline stroke width
#' @export
type_points = function(alpha = NULL, size = NULL, stroke = 0.35) {
  cliplot_type(
    draw = function(data, mapping, settings, ...) {
      p = ggplot2::ggplot(data, mapping)
      if (!is.null(settings$by)) {
        p = p + ggplot2::geom_point(
          ggplot2::aes(color = .data[["..by.."]]),
          alpha = alpha %||% 0.85,
          size  = size %||% 2.8,
          stroke = stroke
        )
      } else {
        p = p + ggplot2::geom_point(
          alpha = alpha %||% 0.85,
          size  = size %||% 2.8,
          color = settings$col.default %||% "#2B6CB0",
          stroke = stroke
        )
      }
      p
    },
    name = "points"
  )
}

# ==========================================================================
# Type: Barplot
# ==========================================================================

#' Bar plot type
#' @param stat Statistic to compute ("count", "identity")
#' @param position Bar position ("stack", "dodge", "fill")
#' @param alpha Fill transparency
#' @export
type_barplot = function(stat = "count", position = "stack", alpha = 0.9) {
  cliplot_type(
    draw = function(data, mapping, settings, ...) {
      p = ggplot2::ggplot(data, mapping)
      if (!is.null(settings$by)) {
        p = p + ggplot2::geom_bar(
          stat = stat, position = position,
          alpha = alpha,
          color = "white",
          linewidth = 0.25,
          ggplot2::aes(fill = .data[["..by.."]])
        )
      } else {
        p = p + ggplot2::geom_bar(
          stat = stat, position = position,
          alpha = alpha,
          fill = settings$col.default %||% "#2B6CB0",
          color = "white",
          linewidth = 0.25
        )
      }
      p
    },
    name = "barplot"
  )
}

# ==========================================================================
# Type: Lines
# ==========================================================================

#' Lines plot type
#'
#' Draws lines connecting data points. Supports individual subject/replicate
#' trajectories via \code{group}, automatic calculation of group mean trajectories
#' with standard error bars (\code{mean ± SE}) or shaded confidence bands (\code{show_ribbon}),
#' horizontal positioning dodge to prevent multi-group collisions, prominent
#' white-stroked mean point glyphs, and overlaid background observation points.
#'
#' @param linewidth Line width (default 1.15).
#' @param alpha Line transparency (default 0.95).
#' @param group Column name or aesthetic for individual grouping (e.g. \code{"Individual"}, \code{"Patient"}).
#' @param aggregate Aggregation mode when duplicate observations exist at the same x:
#'   \code{"auto"} (default, aggregates by mean if duplicates exist and no group is specified),
#'   \code{"mean"}, \code{"median"}, or \code{"none"}.
#' @param add_points Logical. Whether to overlay individual observation points (default \code{FALSE}).
#' @param point_size Size of overlaid individual points (default 2.0).
#' @param point_alpha Transparency of overlaid points (default 0.35).
#' @param show_se Logical. Whether to display standard error bars (\code{mean ± SE}) (default \code{FALSE}).
#' @param show_ribbon Logical. Whether to display a shaded \code{mean ± SE} ribbon (default \code{FALSE}).
#' @param ribbon_alpha Transparency of shaded ribbon (default 0.16).
#' @param dodge Horizontal position dodge to prevent error bars and points from overlapping across groups (default 0.45 when multiple groups exist, otherwise 0).
#' @param errorbar_width Width of error bar caps (default 0.35).
#' @param errorbar_linewidth Line width of error bars (default 0.75).
#' @param show_mean_points Logical. Whether to place prominent point markers on the mean line (default \code{TRUE} when aggregating).
#' @param mean_point_size Size of mean point markers (default 3.5).
#' @export
type_lines = function(
    linewidth          = 1.15,
    alpha              = 0.95,
    group              = NULL,
    aggregate          = c("auto", "mean", "median", "none"),
    add_points         = FALSE,
    point_size         = 2.0,
    point_alpha        = 0.35,
    show_se            = FALSE,
    show_ribbon        = FALSE,
    ribbon_alpha       = 0.16,
    dodge              = 0.45,
    errorbar_width     = 0.35,
    errorbar_linewidth = 0.75,
    show_mean_points   = TRUE,
    mean_point_size    = 3.5
) {
  aggregate = match.arg(aggregate)
  cliplot_type(
    draw = function(data, mapping, settings, ...) {
      dots = list(...)
      grp_col = group %||% settings$group %||% dots$group

      # Resolve group column if specified
      if (!is.null(grp_col) && is.character(grp_col) && length(grp_col) == 1L) {
        if (grp_col %in% names(data)) {
          data[["..group.."]] = as.factor(data[[grp_col]])
        } else if (!is.null(settings$data) && grp_col %in% names(settings$data)) {
          data[["..group.."]] = as.factor(settings$data[[grp_col]])
        }
      }

      has_group = "..group.." %in% names(data)
      has_by    = !is.null(settings$by) && "..by.." %in% names(data)

      # Check for duplicate observations at the same x coordinate
      has_dups = FALSE
      if (!is.null(data$x) && nrow(data) > 0) {
        by_vec = if (has_by) data[["..by.."]] else rep(1, nrow(data))
        has_dups = any(duplicated(paste(data$x, by_vec, sep = "_@_")))
      }

      should_agg = (aggregate == "mean" || aggregate == "median" || (aggregate == "auto" && has_dups && !has_group))

      if (should_agg) {
        agg_fun = if (aggregate == "median") stats::median else mean
        grp_formula = if (has_by) y ~ x + ..by.. else y ~ x
        agg_df = stats::aggregate(grp_formula, data = data, FUN = agg_fun, na.rm = TRUE)

        # Ensure correct sorting along x for clean line paths
        if (has_by) {
          agg_df = agg_df[order(agg_df[["..by.."]], agg_df$x), , drop = FALSE]
        } else {
          agg_df = agg_df[order(agg_df$x), , drop = FALSE]
        }

        # Position dodge if multiple groups exist
        actual_dodge = if (has_by && !is.null(dodge) && dodge > 0) dodge else 0
        pd = if (actual_dodge > 0) ggplot2::position_dodge(width = actual_dodge) else ggplot2::position_identity()

        p = ggplot2::ggplot(agg_df, ggplot2::aes(x = .data[["x"]], y = .data[["y"]]))

        # Calculate SE for errorbars or ribbons
        if ((show_se || show_ribbon) && has_dups) {
          se_fun = function(z) {
            z = stats::na.omit(z)
            if (length(z) > 1) stats::sd(z) / sqrt(length(z)) else 0
          }
          se_df = stats::aggregate(grp_formula, data = data, FUN = se_fun)
          agg_df$se = se_df$y
          agg_df$ymin = pmax(0, agg_df$y - agg_df$se)
          agg_df$ymax = agg_df$y + agg_df$se
        }

        # Shaded ribbon
        if (show_ribbon && has_dups) {
          if (has_by) {
            p = p + ggplot2::geom_ribbon(
              data = agg_df,
              ggplot2::aes(ymin = .data[["ymin"]], ymax = .data[["ymax"]], fill = .data[["..by.."]]),
              alpha = ribbon_alpha, color = NA
            )
          } else {
            p = p + ggplot2::geom_ribbon(
              data = agg_df,
              ggplot2::aes(ymin = .data[["ymin"]], ymax = .data[["ymax"]]),
              fill = settings$col.default %||% "black", alpha = ribbon_alpha, color = NA
            )
          }
        }

        # Individual background observations
        if (add_points || isTRUE(settings$add_points) || isTRUE(dots$add_points)) {
          if (has_by && actual_dodge > 0) {
            p = p + ggplot2::geom_point(
              data = data,
              ggplot2::aes(x = .data[["x"]], y = .data[["y"]], color = .data[["..by.."]]),
              position = ggplot2::position_jitterdodge(jitter.width = 0.12, dodge.width = actual_dodge, seed = 42),
              size = point_size, alpha = point_alpha
            )
          } else if (has_by) {
            p = p + ggplot2::geom_point(
              data = data,
              ggplot2::aes(x = .data[["x"]], y = .data[["y"]], color = .data[["..by.."]]),
              size = point_size, alpha = point_alpha
            )
          } else {
            p = p + ggplot2::geom_point(
              data = data,
              ggplot2::aes(x = .data[["x"]], y = .data[["y"]]),
              size = point_size, alpha = point_alpha,
              color = settings$col.default %||% "black"
            )
          }
        }

        # Error bars with clean caps and slight dodge
        if (show_se && has_dups) {
          if (has_by) {
            p = p + ggplot2::geom_errorbar(
              data = agg_df,
              ggplot2::aes(ymin = .data[["ymin"]], ymax = .data[["ymax"]], color = .data[["..by.."]]),
              position = pd, width = errorbar_width, linewidth = errorbar_linewidth, alpha = 0.9
            )
          } else {
            p = p + ggplot2::geom_errorbar(
              data = agg_df,
              ggplot2::aes(ymin = .data[["ymin"]], ymax = .data[["ymax"]]),
              position = pd, width = errorbar_width, linewidth = errorbar_linewidth, alpha = 0.9,
              color = settings$col.default %||% "black"
            )
          }
        }

        # Trajectory line
        if (has_by) {
          p = p + ggplot2::geom_line(
            data = agg_df,
            ggplot2::aes(color = .data[["..by.."]], group = .data[["..by.."]]),
            position = pd, linewidth = linewidth, alpha = alpha
          )
        } else {
          p = p + ggplot2::geom_line(
            data = agg_df,
            position = pd, linewidth = linewidth, alpha = alpha,
            color = settings$col.default %||% "black"
          )
        }

        # Prominent mean point glyphs
        if (show_mean_points) {
          if (has_by) {
            p = p + ggplot2::geom_point(
              data = agg_df,
              ggplot2::aes(fill = .data[["..by.."]], group = .data[["..by.."]]),
              position = pd, shape = 21, size = mean_point_size, stroke = 1.1, color = "white"
            )
          } else {
            p = p + ggplot2::geom_point(
              data = agg_df,
              shape = 21, size = mean_point_size, stroke = 1.1, color = "white",
              fill = settings$col.default %||% "black"
            )
          }
        }

      } else {
        # Individual lines or single series
        if (has_group) {
          data = data[order(data[["..group.."]], data$x), , drop = FALSE]
        } else if (has_by) {
          data = data[order(data[["..by.."]], data$x), , drop = FALSE]
        } else {
          data = data[order(data$x), , drop = FALSE]
        }

        p = ggplot2::ggplot(data, mapping)
        if (has_group) {
          if (has_by) {
            p = p + ggplot2::geom_line(
              ggplot2::aes(color = .data[["..by.."]], group = .data[["..group.."]]),
              linewidth = linewidth, alpha = alpha
            )
          } else {
            p = p + ggplot2::geom_line(
              ggplot2::aes(group = .data[["..group.."]]),
              linewidth = linewidth, alpha = alpha,
              color = settings$col.default %||% "black"
            )
          }
        } else if (has_by) {
          p = p + ggplot2::geom_line(
            ggplot2::aes(color = .data[["..by.."]]),
            linewidth = linewidth, alpha = alpha
          )
        } else {
          p = p + ggplot2::geom_line(
            linewidth = linewidth, alpha = alpha,
            color = settings$col.default %||% "black"
          )
        }

        if (add_points || isTRUE(settings$add_points) || isTRUE(dots$add_points)) {
          if (has_by) {
            p = p + ggplot2::geom_point(
              ggplot2::aes(color = .data[["..by.."]]),
              size = point_size, alpha = point_alpha
            )
          } else {
            p = p + ggplot2::geom_point(
              size = point_size, alpha = point_alpha,
              color = settings$col.default %||% "black"
            )
          }
        }
      }
      p
    },
    name = "lines"
  )
}

# ==========================================================================
# Type: Histogram
# ==========================================================================

#' Histogram type
#' @param bins Number of bins
#' @param alpha Fill transparency
#' @export
type_histogram = function(bins = 30, alpha = 0.85) {
  cliplot_type(
    draw = function(data, mapping, settings, ...) {
      p = ggplot2::ggplot(data, mapping)
      if (!is.null(settings$by)) {
        p = p + ggplot2::geom_histogram(
          bins = bins, alpha = alpha,
          position = "identity",
          color = "white",
          linewidth = 0.2,
          ggplot2::aes(fill = .data[["..by.."]])
        )
      } else {
        p = p + ggplot2::geom_histogram(
          bins = bins, alpha = alpha,
          fill = settings$col.default %||% "#2B6CB0",
          color = "white",
          linewidth = 0.2
        )
      }
      p
    },
    name = "histogram"
  )
}

# ==========================================================================
# Type: Density
# ==========================================================================

#' Density plot type
#' @param alpha Fill transparency
#' @param linewidth Line width
#' @export
type_density = function(alpha = 0.28, linewidth = 1) {
  cliplot_type(
    draw = function(data, mapping, settings, ...) {
      p = ggplot2::ggplot(data, mapping)
      if (!is.null(settings$by)) {
        p = p + ggplot2::geom_density(
          ggplot2::aes(color = .data[["..by.."]], fill = .data[["..by.."]]),
          alpha = alpha, linewidth = linewidth
        )
      } else {
        p = p + ggplot2::geom_density(
          alpha = alpha, linewidth = linewidth,
          fill = settings$col.default %||% "#0073C2",
          color = settings$col.default %||% "#003C67"
        )
      }
      p
    },
    name = "density"
  )
}

# ==========================================================================
# Type: Errorbar
# ==========================================================================

#' Error bar type (requires ymin and ymax)
#' @param width Error bar width
#' @param linewidth Line width
#' @export
type_errorbar = function(width = 0.2, linewidth = 0.6) {
  cliplot_type(
    data = function(settings, ...) {
      if (is.null(settings$ymin) || is.null(settings$ymax)) {
        stop("Error bars require `ymin` and `ymax` arguments")
      }
    },
    draw = function(data, mapping, settings, ...) {
      p = ggplot2::ggplot(data, mapping)
      if (!is.null(settings$by)) {
        p = p + ggplot2::geom_errorbar(
          ggplot2::aes(ymin = .data[["..ymin.."]],
                       ymax = .data[["..ymax.."]],
                       color = .data[["..by.."]]),
          width = width, linewidth = linewidth
        ) +
        ggplot2::geom_point(
          ggplot2::aes(color = .data[["..by.."]]),
          size = 2.5
        )
      } else {
        p = p + ggplot2::geom_errorbar(
          ggplot2::aes(ymin = .data[["..ymin.."]],
                       ymax = .data[["..ymax.."]]),
          width = width, linewidth = linewidth,
          color = settings$col.default %||% "black"
        ) +
        ggplot2::geom_point(
          color = settings$col.default %||% "black",
          size = 2.5
        )
      }
      p
    },
    name = "errorbar"
  )
}

# ==========================================================================
# Type: Ribbon / Area
# ==========================================================================

#' Ribbon/area plot type
#' @param alpha Fill transparency
#' @export
type_ribbon = function(alpha = NULL) {
  cliplot_type(
    draw = function(data, mapping, settings, ...) {
      ribbon_alpha = alpha %||% get_clipar("ribbon.alpha", 0.2)
      p = ggplot2::ggplot(data, mapping)
      if (!is.null(settings$by)) {
        p = p + ggplot2::geom_ribbon(
          ggplot2::aes(ymin = .data[["..ymin.."]],
                       ymax = .data[["..ymax.."]],
                       fill = .data[["..by.."]]),
          alpha = ribbon_alpha
        ) +
        ggplot2::geom_line(
          ggplot2::aes(color = .data[["..by.."]]),
          linewidth = 0.8
        )
      } else {
        p = p + ggplot2::geom_ribbon(
          ggplot2::aes(ymin = .data[["..ymin.."]],
                       ymax = .data[["..ymax.."]]),
          alpha = ribbon_alpha,
          fill = settings$col.default %||% "#0073C2"
        ) +
        ggplot2::geom_line(
          color = settings$col.default %||% "#003C67",
          linewidth = 0.8
        )
      }
      p
    },
    name = "ribbon"
  )
}
