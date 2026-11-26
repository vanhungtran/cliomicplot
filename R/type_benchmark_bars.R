# ===========================================================================
# cliomicplot: Benchmark Bars Plot Type
# For tiered model benchmarks, jackknife CIs, and permutation null bands
# Reference: Liu et al. / multiomics_paper (Figures 3a-b)
# ===========================================================================

#' Multi-Tier Model Benchmark Bar Charts
#'
#' @description Creates publication-quality tiered model benchmark bar charts
#'   with grouped-jackknife / bootstrap confidence intervals, print-safe borders,
#'   and permutation null distribution reference bands. Designed for rigorous
#'   multi-omics drug target prioritization and machine learning benchmarks
#'   (multiomics_paper / Liu et al.).
#'
#' @param null_band Numeric vector of length 2 giving the lower and upper bounds
#'   of the permutation null distribution (e.g., \code{c(0.505, 0.549)}), drawn as
#'   a neutral reference band behind the bars.
#' @param value_labels Logical. Whether to print exact performance values above
#'   the error whiskers (default TRUE).
#' @param error_low Column name or vector for lower confidence interval bounds.
#' @param error_high Column name or vector for upper confidence interval bounds.
#' @param bar_width Bar width (default 0.75).
#' @param cap_size Error bar whisker cap size (default 0.25).
#' @param border_color Bar border color (default \code{"black"}).
#' @param border_width Bar border line width (default 0.5).
#' @param label_format Character format string for annotations (default "\\\%.2f").
#' @param text_size Numeric text size for annotations (default 3.0).
#' @param group_sep_lines Vector of x-axis positions or category names where vertical
#'   dotted boundary lines should be drawn between governance tiers (default NULL).
#' @param log_scale Logical. Whether y-axis should be log10 transformed (useful for
#'   AP lift; default FALSE).
#' @param ... Additional arguments.
#'
#' @return A \code{cliplot_type} object for use with \code{\link{cliplot}}.
#'
#' @examples
#' \dontrun{
#' df_bench = data.frame(
#'   model     = c("Biology", "Druggability", "B+D", "Attention", "Clinical", "Label"),
#'   auroc     = c(0.77, 0.77, 0.86, 0.91, 0.89, 0.98),
#'   auroc_lo  = c(0.69, 0.68, 0.78, 0.84, 0.82, 0.94),
#'   auroc_hi  = c(0.85, 0.84, 0.92, 0.97, 0.96, 1.00),
#'   learner   = rep("HGB", 6)
#' )
#' cliplot(auroc ~ model, data = df_bench,
#'         type = type_benchmark_bars(null_band = c(0.505, 0.549)))
#' }
#'
#' @export
type_benchmark_bars = function(
    null_band       = NULL,
    value_labels    = TRUE,
    error_low       = NULL,
    error_high      = NULL,
    bar_width       = 0.75,
    cap_size        = 0.25,
    border_color    = "black",
    border_width    = 0.5,
    label_format    = "%.2f",
    text_size       = 3.0,
    group_sep_lines = NULL,
    log_scale       = FALSE,
    ...
) {
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
        } else if ("auroc_jack_lo" %in% nms) {
          df$err_lo = raw_data$auroc_jack_lo
        } else if ("ap_lift_lo" %in% nms) {
          df$err_lo = raw_data$ap_lift_lo
        } else if ("lo" %in% nms) {
          df$err_lo = raw_data$lo
        } else if ("ymin" %in% nms) {
          df$err_lo = raw_data$ymin
        }

        if (!is.null(error_high) && error_high %in% nms) {
          df$err_hi = raw_data[[error_high]]
        } else if ("auroc_hi" %in% nms) {
          df$err_hi = raw_data$auroc_hi
        } else if ("auroc_jack_hi" %in% nms) {
          df$err_hi = raw_data$auroc_jack_hi
        } else if ("ap_lift_hi" %in% nms) {
          df$err_hi = raw_data$ap_lift_hi
        } else if ("hi" %in% nms) {
          df$err_hi = raw_data$hi
        } else if ("ymax" %in% nms) {
          df$err_hi = raw_data$ymax
        }
      }

      settings$bench_df              = df
      settings$bench_null_band       = null_band
      settings$bench_value_labels    = value_labels
      settings$bench_bar_width       = bar_width
      settings$bench_cap_size        = cap_size
      settings$bench_border_color    = border_color
      settings$bench_border_width    = border_width
      settings$bench_label_format    = label_format
      settings$bench_text_size       = text_size
      settings$bench_group_sep_lines = group_sep_lines
      settings$bench_log_scale       = log_scale
    },
    draw = function(data, mapping, settings, ...) {
      df = settings$bench_df
      if (is.null(df) || nrow(df) == 0) return(ggplot2::ggplot())

      has_by = "by" %in% names(df)
      has_err = ("err_lo" %in% names(df)) && ("err_hi" %in% names(df))

      if (!is.factor(df$x)) {
        df$x = factor(df$x, levels = unique(df$x))
      }

      p = ggplot2::ggplot(df)

      # 1. Permutation null distribution reference band (drawn behind bars)
      nb = settings$bench_null_band
      if (!is.null(nb) && length(nb) == 2) {
        p = p + ggplot2::annotate(
          "rect",
          xmin = -Inf, xmax = Inf,
          ymin = nb[1], ymax = nb[2],
          fill = "#CFCECE", alpha = 0.45
        )
      }

      # 2. Benchmark bars
      pos = if (has_by) ggplot2::position_dodge(width = settings$bench_bar_width) else "stack"

      if (has_by) {
        p = p + ggplot2::geom_col(
          ggplot2::aes(x = .data[["x"]], y = .data[["y"]], fill = .data[["by"]]),
          position = pos,
          width = settings$bench_bar_width * 0.9,
          color = settings$bench_border_color,
          linewidth = settings$bench_border_width
        )
      } else {
        p = p + ggplot2::geom_col(
          ggplot2::aes(x = .data[["x"]], y = .data[["y"]], fill = .data[["x"]]),
          width = settings$bench_bar_width,
          color = settings$bench_border_color,
          linewidth = settings$bench_border_width
        )
      }

      # 3. Error bars (whiskers with caps)
      if (has_err) {
        if (has_by) {
          p = p + ggplot2::geom_errorbar(
            ggplot2::aes(
              x = .data[["x"]],
              ymin = .data[["err_lo"]],
              ymax = .data[["err_hi"]],
              group = .data[["by"]]
            ),
            position = pos,
            width = settings$bench_cap_size,
            linewidth = 0.55,
            color = "black"
          )
        } else {
          p = p + ggplot2::geom_errorbar(
            ggplot2::aes(
              x = .data[["x"]],
              ymin = .data[["err_lo"]],
              ymax = .data[["err_hi"]]
            ),
            width = settings$bench_cap_size,
            linewidth = 0.55,
            color = "black"
          )
        }
      }

      # 4. Group tier separation lines
      seps = settings$bench_group_sep_lines
      if (!is.null(seps)) {
        if (is.numeric(seps)) {
          for (s in seps) {
            p = p + ggplot2::geom_vline(xintercept = s, linetype = "dotted", color = "#767676", linewidth = 0.5)
          }
        } else if (is.character(seps)) {
          lvls = levels(df$x)
          idx = match(seps, lvls)
          idx = idx[!is.na(idx)]
          for (s in idx) {
            p = p + ggplot2::geom_vline(xintercept = s - 0.5, linetype = "dotted", color = "#767676", linewidth = 0.5)
          }
        }
      }

      # 5. Value labels
      if (isTRUE(settings$bench_value_labels)) {
        fmt = settings$bench_label_format
        df$val_str = sprintf(fmt, df$y)
        target_y = if (has_err) df$err_hi else df$y

        if (has_by) {
          p = p + ggplot2::geom_text(
            data = df,
            ggplot2::aes(
              x = .data[["x"]],
              y = target_y,
              group = .data[["by"]],
              label = .data[["val_str"]]
            ),
            position = pos,
            vjust = -0.7,
            size = settings$bench_text_size,
            color = "black"
          )
        } else {
          p = p + ggplot2::geom_text(
            data = df,
            ggplot2::aes(
              x = .data[["x"]],
              y = target_y,
              label = .data[["val_str"]]
            ),
            vjust = -0.7,
            size = settings$bench_text_size,
            color = "black"
          )
        }
      }

      # 6. Log scale for AP lift if requested
      if (isTRUE(settings$bench_log_scale)) {
        p = p + ggplot2::scale_y_log10()
      }

      p = p + ggplot2::labs(
        x = settings$xlab %||% "Model / Feature Tier",
        y = settings$ylab %||% "Metric Value"
      )
      p
    },
    name = "benchmark_bars"
  )
}

#' @rdname type_benchmark_bars
#' @export
type_benchmark = function(...) type_benchmark_bars(...)
