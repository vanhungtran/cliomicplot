# ===========================================================================
# cliomicplot: Attribution Plot Type
# For Shapley value attribution, stepwise ablation, and dual contrasts
# Reference: Liu et al. / multiomics_paper (Figures 2a-d)
# ===========================================================================

#' Attribution Plot Type (Shapley Values, Stepwise Ablation, and Dual Contrasts)
#'
#' @description Creates publication-grade attribution plots inspired by
#'   leakage-controlled machine learning and multi-omics drug target
#'   prioritization benchmarks. Supports three core styles:
#'   \itemize{
#'     \item \code{"shapley"}: Horizontal or vertical bars displaying exact
#'       Shapley value attributions or feature-level contributions across
#'       mechanisms or feature tiers, with optional grouping across learners.
#'     \item \code{"stepwise"}: Stepwise mechanism removal / ablation curves
#'       tracing metric deterioration (e.g. AUROC or Average Precision) with
#'       95\% confidence intervals and step callouts.
#'     \item \code{"contrast"}: Dual-context attribution contrast comparing
#'       marginal impact when removed from a fully specified model vs. when
#'       added to an unaugmented baseline.
#'   }
#'
#' @param style One of \code{"shapley"} (default), \code{"stepwise"}, or \code{"contrast"}.
#' @param reference_line Numeric. Reference value for baseline (default 0 for
#'   Shapley/contrast; 0.5 for stepwise AUROC).
#' @param annotate Logical. Whether to print numeric values on bars or points (default TRUE).
#' @param label_format Character format string for annotations (default "%.3f").
#' @param error_low Column name or vector for lower error bounds in stepwise/contrast plots.
#' @param error_high Column name or vector for upper error bounds in stepwise/contrast plots.
#' @param color_pos Color for positive contributions (default \code{"#B64342"}).
#' @param color_neg Color for negative contributions (default \code{"#0F4D92"}).
#' @param bar_width Numeric bar width (default 0.65).
#' @param point_size Numeric point size for stepwise/contrast (default 3.5).
#' @param text_size Numeric text size for value annotations (default 3.0).
#' @param learner_colors Named vector of colors for multi-learner contrasts
#'   (default: HGB = \code{"#0F4D92"}, LR = \code{"#B64342"}, RF = \code{"#42949E"}).
#' @param ... Additional arguments.
#'
#' @return A \code{cliplot_type} object for use with \code{\link{cliplot}}.
#'
#' @examples
#' \dontrun{
#' # Shapley values across leakage mechanisms
#' df_shap = data.frame(
#'   mechanism = c("Label", "Prior Trials", "Attention", "Zero-fill", "Ungrouped CV"),
#'   shapley   = c(0.032, 0.020, 0.028, -0.008, 0.003),
#'   learner   = rep("HGB", 5)
#' )
#' cliplot(shapley ~ mechanism, data = df_shap, type = type_attribution(style = "shapley"))
#' }
#'
#' @export
type_attribution = function(
    style         = c("shapley", "stepwise", "contrast"),
    reference_line = NULL,
    annotate      = TRUE,
    label_format  = "%.3f",
    error_low     = NULL,
    error_high    = NULL,
    color_pos     = "#B64342",
    color_neg     = "#0F4D92",
    bar_width     = 0.65,
    point_size    = 3.5,
    text_size     = 3.0,
    learner_colors = c(hgb = "#0F4D92", HGB = "#0F4D92",
                       lr  = "#B64342", LR  = "#B64342",
                       rf  = "#42949E", RF  = "#42949E"),
    ...
) {
  style = match.arg(style)
  ref_val = reference_line %||% (if (style == "stepwise") 0.5 else 0)

  cliplot_type(
    data = function(settings, ...) {
      x_vals = settings$x
      y_vals = settings$y
      by_vals = settings$by

      df = data.frame(
        x = x_vals,
        y = as.numeric(y_vals),
        stringsAsFactors = FALSE
      )
      if (!is.null(by_vals)) {
        df$by = as.factor(by_vals)
      }

      raw_data = settings$data
      if (!is.null(raw_data) && is.data.frame(raw_data)) {
        nms = names(raw_data)
        if (!is.null(error_low) && error_low %in% nms) {
          df$err_lo = raw_data[[error_low]]
        } else if ("auroc_lo" %in% nms) {
          df$err_lo = raw_data$auroc_lo
        } else if ("lo" %in% nms) {
          df$err_lo = raw_data$lo
        } else if ("ymin" %in% nms) {
          df$err_lo = raw_data$ymin
        }

        if (!is.null(error_high) && error_high %in% nms) {
          df$err_hi = raw_data[[error_high]]
        } else if ("auroc_hi" %in% nms) {
          df$err_hi = raw_data$auroc_hi
        } else if ("hi" %in% nms) {
          df$err_hi = raw_data$hi
        } else if ("ymax" %in% nms) {
          df$err_hi = raw_data$ymax
        }

        if ("design" %in% nms) {
          df$design = raw_data$design
        }
      }

      settings$attr_df         = df
      settings$attr_style      = style
      settings$attr_ref_val    = ref_val
      settings$attr_annotate   = annotate
      settings$attr_fmt        = label_format
      settings$attr_col_pos    = color_pos
      settings$attr_col_neg    = color_neg
      settings$attr_bar_width  = bar_width
      settings$attr_point_size = point_size
      settings$attr_text_size  = text_size
      settings$attr_learner_colors = learner_colors
    },
    draw = function(data, mapping, settings, ...) {
      df = settings$attr_df
      if (is.null(df) || nrow(df) == 0) return(ggplot2::ggplot())

      has_by = "by" %in% names(df)
      p = ggplot2::ggplot(df)

      if (settings$attr_style == "shapley") {
        # Style 1: Shapley values (horizontal bars)
        if (!is.factor(df$x)) {
          df$x = factor(df$x, levels = rev(unique(df$x)))
        }

        if (has_by) {
          p = ggplot2::ggplot(df, ggplot2::aes(x = .data[["y"]], y = .data[["x"]], fill = .data[["by"]])) +
            ggplot2::geom_col(
              position = ggplot2::position_dodge(width = 0.75),
              width = settings$attr_bar_width,
              color = "black", linewidth = 0.4
            )
          if (all(as.character(unique(df$by)) %in% names(settings$attr_learner_colors))) {
            p = p + ggplot2::scale_fill_manual(values = settings$attr_learner_colors)
          }
        } else {
          df$direction = ifelse(df$y >= 0, "pos", "neg")
          p = ggplot2::ggplot(df, ggplot2::aes(x = .data[["y"]], y = .data[["x"]], fill = .data[["direction"]])) +
            ggplot2::geom_col(
              width = settings$attr_bar_width,
              color = "black", linewidth = 0.4, show.legend = FALSE
            ) +
            ggplot2::scale_fill_manual(values = c(
              pos = settings$attr_col_pos,
              neg = settings$attr_col_neg
            ))
        }

        # Baseline vertical line at ref_val
        p = p + ggplot2::geom_vline(
          xintercept = settings$attr_ref_val,
          color = "black", linewidth = 0.55
        )

        # Annotations
        if (isTRUE(settings$attr_annotate)) {
          fmt = settings$attr_fmt
          df$val_str = sprintf(fmt, df$y)
          if (has_by) {
            p = p + ggplot2::geom_text(
              data = df,
              ggplot2::aes(
                x = .data[["y"]],
                y = .data[["x"]],
                group = .data[["by"]],
                label = .data[["val_str"]]
              ),
              position = ggplot2::position_dodge(width = 0.75),
              hjust = ifelse(df$y >= 0, -0.2, 1.2),
              size = settings$attr_text_size,
              color = "black"
            )
          } else {
            p = p + ggplot2::geom_text(
              data = df,
              ggplot2::aes(
                x = .data[["y"]],
                y = .data[["x"]],
                label = .data[["val_str"]]
              ),
              hjust = ifelse(df$y >= 0, -0.2, 1.2),
              size = settings$attr_text_size,
              color = "black"
            )
          }
        }

        p = p + ggplot2::labs(
          x = settings$ylab %||% "Attribution Score",
          y = settings$xlab %||% "Mechanism / Feature Tier"
        )

      } else if (settings$attr_style == "stepwise") {
        # Style 2: Stepwise sequential removal / ablation staircase
        if (!is.factor(df$x)) {
          df$x = factor(df$x, levels = unique(df$x))
        }

        has_err = ("err_lo" %in% names(df)) && ("err_hi" %in% names(df))

        p = ggplot2::ggplot(df, ggplot2::aes(x = .data[["x"]], y = .data[["y"]], group = 1))

        # Horizontal baseline line (e.g. chance AUROC 0.5)
        p = p + ggplot2::geom_hline(
          yintercept = settings$attr_ref_val,
          linetype = "dashed", color = "#767676", linewidth = 0.45
        )

        # Step line connecting points
        p = p + ggplot2::geom_line(
          color = "#4D4D4D", linewidth = 0.8
        )

        # Error whiskers if present
        if (has_err) {
          p = p + ggplot2::geom_errorbar(
            ggplot2::aes(ymin = .data[["err_lo"]], ymax = .data[["err_hi"]]),
            width = 0.2, linewidth = 0.55, color = "black"
          )
        }

        # Step points
        p = p + ggplot2::geom_point(
          size = settings$attr_point_size,
          shape = 21, fill = settings$attr_col_pos, stroke = 0.8,
          color = "black", show.legend = FALSE
        )

        # Annotate exact value at each step
        if (isTRUE(settings$attr_annotate)) {
          fmt = settings$attr_fmt
          df$val_str = sprintf(fmt, df$y)
          p = p + ggplot2::geom_text(
            data = df,
            ggplot2::aes(x = .data[["x"]], y = .data[["y"]], label = .data[["val_str"]]),
            vjust = -1.2, size = settings$attr_text_size, color = "black"
          )
        }

        p = p + ggplot2::labs(
          x = settings$xlab %||% "Removal Step",
          y = settings$ylab %||% "Performance Metric"
        )

      } else if (settings$attr_style == "contrast") {
        # Style 3: Dual attribution contrast
        if (!is.factor(df$x)) {
          df$x = factor(df$x, levels = rev(unique(df$x)))
        }

        shape_var = if ("design" %in% names(df)) "design" else NULL

        p = ggplot2::ggplot(df)

        # Baseline zero line
        p = p + ggplot2::geom_vline(
          xintercept = settings$attr_ref_val,
          color = "black", linewidth = 0.55
        )

        if (!is.null(shape_var) && has_by) {
          p = p + ggplot2::geom_point(
            ggplot2::aes(
              x = .data[["y"]],
              y = .data[["x"]],
              color = .data[["by"]],
              shape = .data[[shape_var]]
            ),
            size = settings$attr_point_size,
            stroke = 0.8,
            position = ggplot2::position_dodge(width = 0.4)
          ) +
          ggplot2::scale_shape_manual(values = c(16, 15, 17, 18))
        } else if (has_by) {
          p = p + ggplot2::geom_point(
            ggplot2::aes(
              x = .data[["y"]],
              y = .data[["x"]],
              color = .data[["by"]]
            ),
            size = settings$attr_point_size,
            position = ggplot2::position_dodge(width = 0.4)
          )
        } else {
          p = p + ggplot2::geom_point(
            ggplot2::aes(x = .data[["y"]], y = .data[["x"]]),
            size = settings$attr_point_size,
            color = settings$attr_col_pos
          )
        }

        if (has_by && all(as.character(unique(df$by)) %in% names(settings$attr_learner_colors))) {
          p = p + ggplot2::scale_color_manual(values = settings$attr_learner_colors)
        }

        p = p + ggplot2::labs(
          x = settings$ylab %||% "AUROC Inflation Attributable to Mechanism",
          y = settings$xlab %||% "Mechanism"
        )
      }

      p
    },
    name = "attribution"
  )
}

#' @rdname type_attribution
#' @export
type_shapley = function(...) type_attribution(style = "shapley", ...)

#' @rdname type_attribution
#' @export
type_stepwise = function(...) type_attribution(style = "stepwise", ...)
