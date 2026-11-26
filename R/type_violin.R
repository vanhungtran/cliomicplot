# ===========================================================================
# cliomicplot: Violin Plot Type
# ===========================================================================

#' Violin Plot Type
#'
#' @description Creates publication-ready violin plots with rich interior anatomy
#'   options (boxplots, quartile lines, mean-SE glyphs), optional single-cell
#'   jittered raw data points, split-half condition comparisons, and custom
#'   density scaling.
#'
#' @param inner Interior summary structure: \code{"boxplot"} (default, overlay
#'   mini-boxplot), \code{"quartiles"} (draw 25\%, median, and 75\% density quantile
#'   lines), \code{"mean_se"} (mean diamond glyph with standard error errorbars),
#'   \code{"median_iqr"} (median circle with interquartile range crossbar), or
#'   \code{"none"}.
#' @param quantiles Numeric vector of quantile lines to draw when
#'   \code{inner = "quartiles"} (default \code{c(0.25, 0.5, 0.75)}).
#' @param quantile_color Color for quantile lines (default \code{"grey25"}).
#' @param quantile_linewidth Line width for quantile lines (default 0.45).
#' @param quantile_linetype Line type for quantile lines (default \code{"solid"}).
#' @param split Logical. When \code{TRUE} and a 2-level grouping variable is
#'   present, renders split violins comparing both conditions back-to-back
#'   (default \code{FALSE}).
#' @param add_boxplot Backward-compatible flag to overlay boxplot (default \code{TRUE}).
#'   If set to \code{FALSE}, overrides \code{inner = "none"}.
#' @param add_jitter Overlay jittered raw data points (default \code{TRUE}).
#' @param jitter_width Width of horizontal jitter (default 0.15).
#' @param jitter_alpha Transparency of jittered points (default 0.45).
#' @param jitter_size Size of jittered points (default 1.5).
#' @param violin_alpha Violin fill transparency (default 0.78).
#' @param violin_color Outline color of violins (default \code{"#30343B"}).
#' @param linewidth Outline stroke width (default 0.3).
#' @param box_width Width of boxplot overlay (default 0.18).
#' @param box_fill Fill color of boxplot overlay (default \code{"white"}).
#' @param box_alpha Transparency of boxplot overlay (default 0.82).
#' @param trim Trim violin tails to observed data range (default \code{TRUE}).
#' @param scale Density scaling method: \code{"width"} (default, all categories
#'   have equal maximum width), \code{"area"}, or \code{"count"}.
#' @param orientation Plot orientation: \code{"vertical"} (default) or \code{"horizontal"}.
#' @param show_n Logical. Annotate sample sizes (\eqn{n = \dots}) below categories
#'   (default \code{FALSE}).
#'
#' @return A cliplot_type object for use with \code{\link{cliplot}}.
#'
#' @examples
#' \dontrun{
#' # Standard publication violin with inner boxplot and jitter
#' cliplot(len ~ dose, data = ToothGrowth, type = "violin")
#'
#' # Violin with interior quartile lines
#' cliplot(len ~ dose, data = ToothGrowth,
#'         type = type_violin(inner = "quartiles"))
#'
#' # Mean diamond glyph with standard error bars
#' cliplot(len ~ dose | supp, data = ToothGrowth,
#'         type = type_violin(inner = "mean_se", add_jitter = TRUE))
#'
#' # Split violin comparing two conditions back-to-back
#' cliplot(len ~ dose | supp, data = ToothGrowth,
#'         type = type_violin(split = TRUE, inner = "quartiles"))
#' }
#'
#' @export
type_violin = function(
    inner              = c("boxplot", "quartiles", "mean_se", "median_iqr", "none"),
    quantiles          = c(0.25, 0.5, 0.75),
    quantile_color     = "grey25",
    quantile_linewidth = 0.45,
    quantile_linetype  = "solid",
    split              = FALSE,
    add_boxplot        = TRUE,
    add_jitter         = TRUE,
    jitter_width       = 0.15,
    jitter_alpha       = 0.45,
    jitter_size        = 1.5,
    violin_alpha       = 0.78,
    violin_color       = "#30343B",
    linewidth          = 0.3,
    box_width          = 0.18,
    box_fill           = "white",
    box_alpha          = 0.82,
    trim               = TRUE,
    scale              = "width",
    orientation        = c("vertical", "horizontal"),
    show_n             = FALSE
) {
  inner       = match.arg(inner)
  orientation = match.arg(orientation)

  # Support legacy add_boxplot = FALSE
  if (!add_boxplot && inner == "boxplot") {
    inner = "none"
  }

  cliplot_type(
    draw = function(data, mapping, settings, ...) {
      # Ensure categorical x for violin plots so numeric levels dodge cleanly
      if ("x" %in% names(data) && is.numeric(data[["x"]])) {
        data[["x"]] <- as.factor(data[["x"]])
      }

      has_by <- !is.null(settings$by) && "..by.." %in% names(data)
      is_grouped_dodge <- has_by && !identical(as.character(data[["x"]]), as.character(data[["..by.."]]))
      dodge_width <- 0.8
      pos <- if (is_grouped_dodge) ggplot2::position_dodge(width = dodge_width) else ggplot2::position_identity()

      # If split is requested and grouping has 2 levels, route to split violin
      if (split && has_by) {
        return(type_split_violin(
          inner              = if (inner %in% c("quartiles", "boxplot", "mean_se", "none")) inner else "quartiles",
          quantiles          = quantiles,
          quantile_color     = quantile_color,
          quantile_linewidth = quantile_linewidth,
          quantile_linetype  = quantile_linetype,
          add_jitter         = add_jitter,
          jitter_width       = jitter_width * 0.6,
          jitter_alpha       = jitter_alpha,
          jitter_size        = jitter_size,
          violin_alpha       = violin_alpha,
          violin_color       = violin_color,
          linewidth          = linewidth,
          box_width          = box_width * 0.7,
          box_fill           = box_fill,
          box_alpha          = box_alpha,
          trim               = trim,
          scale              = scale,
          orientation        = orientation
        )$draw(data, mapping, settings, ...))
      }

      p = ggplot2::ggplot(data, mapping)

      # 1. Base Violin Layer
      if (has_by) {
        p = p + ggplot2::geom_violin(
          ggplot2::aes(fill = .data[["..by.."]]),
          alpha              = violin_alpha,
          trim               = trim,
          scale              = scale,
          color              = violin_color,
          linewidth          = linewidth,
          position           = pos,
          quantiles          = if (inner == "quartiles") quantiles else NULL,
          quantile.colour    = if (inner == "quartiles") quantile_color else NULL,
          quantile.linewidth = if (inner == "quartiles") quantile_linewidth else NULL,
          quantile.linetype  = if (inner == "quartiles") quantile_linetype else 0L
        )
      } else {
        p = p + ggplot2::geom_violin(
          alpha              = violin_alpha,
          trim               = trim,
          scale              = scale,
          fill               = settings$col.default %||% "#0073C2",
          color              = violin_color,
          linewidth          = linewidth,
          quantiles          = if (inner == "quartiles") quantiles else NULL,
          quantile.colour    = if (inner == "quartiles") quantile_color else NULL,
          quantile.linewidth = if (inner == "quartiles") quantile_linewidth else NULL,
          quantile.linetype  = if (inner == "quartiles") quantile_linetype else 0L
        )
      }

      # 2. Inner Summary Structure
      if (inner == "boxplot") {
        if (has_by) {
          p = p + ggplot2::geom_boxplot(
            ggplot2::aes(group = interaction(.data[["x"]], .data[["..by.."]])),
            width         = box_width,
            alpha         = box_alpha,
            color         = violin_color,
            fill          = box_fill,
            linewidth     = linewidth + 0.05,
            outlier.shape = NA,
            position      = pos,
            show.legend   = FALSE
          )
        } else {
          p = p + ggplot2::geom_boxplot(
            width         = box_width,
            alpha         = box_alpha,
            color         = violin_color,
            fill          = box_fill,
            linewidth     = linewidth + 0.05,
            outlier.shape = NA
          )
        }
      } else if (inner == "mean_se") {
        if (has_by) {
          p = p + ggplot2::stat_summary(
            ggplot2::aes(group = interaction(.data[["x"]], .data[["..by.."]])),
            fun.data  = ggplot2::mean_se,
            geom      = "errorbar",
            width     = box_width * 0.7,
            linewidth = linewidth + 0.35,
            color     = violin_color,
            position  = pos
          ) + ggplot2::stat_summary(
            ggplot2::aes(group = interaction(.data[["x"]], .data[["..by.."]])),
            fun       = mean,
            geom      = "point",
            shape     = 23,
            size      = 3.0,
            fill      = "white",
            color     = violin_color,
            stroke    = 0.9,
            position  = pos
          )
        } else {
          p = p + ggplot2::stat_summary(
            fun.data  = ggplot2::mean_se,
            geom      = "errorbar",
            width     = box_width * 0.7,
            linewidth = linewidth + 0.35,
            color     = violin_color
          ) + ggplot2::stat_summary(
            fun       = mean,
            geom      = "point",
            shape     = 23,
            size      = 3.0,
            fill      = "white",
            color     = violin_color,
            stroke    = 0.9
          )
        }
      } else if (inner == "median_iqr") {
        # Median circle + IQR range line
        iqr_fun <- function(y) {
          qs <- stats::quantile(y, probs = c(0.25, 0.5, 0.75), na.rm = TRUE)
          data.frame(y = qs[2], ymin = qs[1], ymax = qs[3])
        }
        if (has_by) {
          p = p + ggplot2::stat_summary(
            ggplot2::aes(group = interaction(.data[["x"]], .data[["..by.."]])),
            fun.data  = iqr_fun,
            geom      = "pointrange",
            size      = 0.6,
            linewidth = linewidth + 0.5,
            color     = violin_color,
            fill      = "white",
            shape     = 21,
            position  = pos
          )
        } else {
          p = p + ggplot2::stat_summary(
            fun.data  = iqr_fun,
            geom      = "pointrange",
            size      = 0.6,
            linewidth = linewidth + 0.5,
            color     = violin_color,
            fill      = "white",
            shape     = 21
          )
        }
      }

      # 3. Jittered Raw Points
      if (add_jitter) {
        if (has_by) {
          jit_pos <- if (is_grouped_dodge) {
            ggplot2::position_jitterdodge(
              jitter.width  = jitter_width,
              dodge.width   = dodge_width,
              jitter.height = 0
            )
          } else {
            ggplot2::position_jitter(
              width  = jitter_width,
              height = 0
            )
          }
          p = p + ggplot2::geom_jitter(
            ggplot2::aes(color = .data[["..by.."]]),
            alpha    = jitter_alpha,
            size     = jitter_size,
            position = jit_pos
          ) + ggplot2::guides(color = "none")
        } else {
          p = p + ggplot2::geom_jitter(
            width  = jitter_width,
            alpha  = jitter_alpha,
            size   = jitter_size,
            height = 0,
            color  = "grey30"
          )
        }
      }

      # 4. Sample size annotations (show_n = TRUE)
      if (show_n) {
        n_df <- if (has_by) {
          stats::aggregate(
            stats::as.formula(paste("y ~ x + ..by..")),
            data = data,
            FUN = length
          )
        } else {
          stats::aggregate(y ~ x, data = data, FUN = length)
        }
        names(n_df)[names(n_df) == "y"] <- "n"
        min_y <- min(data[["y"]], na.rm = TRUE)
        span_y <- diff(range(data[["y"]], na.rm = TRUE))
        y_pos <- min_y - 0.08 * span_y

        if (has_by) {
          p = p + ggplot2::geom_text(
            data = n_df,
            ggplot2::aes(
              x = .data[["x"]],
              y = y_pos,
              label = paste0("n=", .data[["n"]]),
              group = .data[["..by.."]]
            ),
            position = ggplot2::position_dodge(width = dodge_width),
            size = 2.6,
            color = "grey40"
          )
        } else {
          p = p + ggplot2::geom_text(
            data = n_df,
            ggplot2::aes(
              x = .data[["x"]],
              y = y_pos,
              label = paste0("n=", .data[["n"]])
            ),
            size = 2.8,
            color = "grey40"
          )
        }
      }

      # 5. Orientation
      if (orientation == "horizontal") {
        p = p + ggplot2::coord_flip()
      }

      p
    },
    name = "violin"
  )
}
