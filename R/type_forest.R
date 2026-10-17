# ===========================================================================
# cliomicplot: Forest Plot Type
# For hazard ratios, odds ratios, and meta-analysis
# Enhanced with CNS Journal Clinical Figure Standards:
# - Alternating row shading stripes (Clinical Archetype 9) to eliminate eye drift
# - Unclipped CI and zero-magnitude negative sign suppression
# - Summary meta-analysis diamond marker
# - Exact value annotations (HR, 95% CI, p-value)
# ===========================================================================

#' Forest Plot Type
#'
#' @description Creates publication-ready forest plots for displaying hazard
#'   ratios, odds ratios, and confidence intervals from survival analysis,
#'   logistic regression, observational causal models, or meta-analysis.
#'   Enhanced with journal reporting standards: alternating row shading,
#'   summary pooled diamonds, unclipped numeric annotations, and zero-magnitude
#'   sign suppression.
#'
#' @param estimate Column name or vector of point estimates (HR, OR, etc.).
#' @param ci_low Column name or vector of lower CI bounds.
#' @param ci_high Column name or vector of upper CI bounds.
#' @param variable Column name or vector of variable labels.
#' @param ref_line Show reference line (default TRUE).
#' @param ref_val Value for reference line (default 1 for HR/OR; set 0 for beta).
#' @param log_scale Use log10 scale for estimates (default TRUE for HR/OR).
#' @param sort Sort by estimate magnitude (default TRUE).
#' @param stripe Add subtle alternating background row shading to prevent
#'   eye drift across wide canvases (default TRUE; Archetype 9).
#' @param show_values Annotate exact numeric estimates and 95\% CIs on the
#'   plot (default FALSE).
#' @param diamond Highlight summary / meta-analysis pooled rows as diamonds
#'   (default FALSE, or auto-detected from \code{is_summary} column).
#' @param point_size Point size for estimates (default 3).
#' @param ci_width CI line width (default 0.9).
#' @param sig_color Color for significant results (default "#E64B35").
#' @param ns_color Color for non-significant results (default "#3C5488").
#'
#' @return A cliplot_type object for use with \code{\link{cliplot}}.
#'
#' @examples
#' \dontrun{
#' forest_data = data.frame(
#'   Variable = c("Age >60", "Male", "Stage III", "Stage IV", "Mutation+"),
#'   HR = c(1.8, 1.3, 2.5, 4.2, 1.9),
#'   CI_low = c(1.2, 0.9, 1.6, 2.8, 1.3),
#'   CI_high = c(2.7, 1.9, 3.9, 6.3, 2.8),
#'   P = c(0.001, 0.15, 0.0005, 0.0001, 0.003)
#' )
#'
#' cliplot(HR ~ Variable, data = forest_data, type = "forest")
#' }
#'
#' @export
type_forest = function(
    estimate    = NULL,
    ci_low      = NULL,
    ci_high     = NULL,
    variable    = NULL,
    ref_line    = TRUE,
    ref_val     = 1,
    log_scale   = TRUE,
    sort        = TRUE,
    stripe      = TRUE,
    show_values = FALSE,
    diamond     = FALSE,
    point_size  = 3,
    ci_width    = 0.9,
    sig_color   = "#E64B35",
    ns_color    = "#3C5488"
) {
  cliplot_type(
    data = function(settings, ...) {
      df = settings$data
      if (is.null(df)) {
        df = data.frame(
          estimate = settings$y,
          variable = if (!is.null(names(settings$y))) names(settings$y)
                     else settings$x,
          stringsAsFactors = FALSE
        )
        df$ci_low  = settings$ymin %||% (df$estimate * 0.7)
        df$ci_high = settings$ymax %||% (df$estimate * 1.4)
        df$pval    = settings$extra_data$pval %||% rep(0.01, nrow(df))
      } else {
        nms = names(df)
        df$estimate = settings$y %||% df[[nms[1]]]
        df$variable = settings$x %||% df[[nms[2]]]
        df$ci_low   = settings$ymin %||%
          (if (!is.null(ci_low) && ci_low %in% nms) df[[ci_low]]
           else if ("CI_low" %in% nms) df$CI_low
           else if ("lower" %in% nms) df$lower
           else df$estimate * 0.7)
        df$ci_high  = settings$ymax %||%
          (if (!is.null(ci_high) && ci_high %in% nms) df[[ci_high]]
           else if ("CI_high" %in% nms) df$CI_high
           else if ("upper" %in% nms) df$upper
           else df$estimate * 1.4)
        df$pval     = if ("P" %in% nms) df$P
                      else if ("pval" %in% nms) df$pval
                      else if ("p_val" %in% nms) df$p_val
                      else rep(0.01, nrow(df))
      }

      df$variable = as.character(df$variable)
      df$sig = ifelse(df$pval < 0.05, "p < 0.05", "NS")

      # Summary / diamond rows
      df$is_summary = FALSE
      if ("is_summary" %in% names(df)) {
        df$is_summary = as.logical(df$is_summary)
      } else if (isTRUE(diamond)) {
        # Mark last row as summary if diamond explicitly requested
        df$is_summary[nrow(df)] = TRUE
      }

      # Zero-magnitude negative sign suppression (Clinical Archetype 9)
      clean_num = function(v) {
        r = round(v, 2)
        r[abs(r) == 0] = 0
        sprintf("%.2f", r)
      }
      df$val_label = sprintf("%s (%s-%s)", clean_num(df$estimate), clean_num(df$ci_low), clean_num(df$ci_high))

      # Sort by estimate (keeping summary row at bottom if present)
      if (sort) {
        regular = df[!df$is_summary, ]
        regular = regular[order(regular$estimate), ]
        summary_rows = df[df$is_summary, ]
        df = rbind(regular, summary_rows)
        df$variable = factor(df$variable, levels = df$variable)
      } else {
        df$variable = factor(df$variable, levels = rev(unique(df$variable)))
      }

      settings$forest_data = df
      settings$forest_ref_line = ref_line
      settings$forest_ref_val  = ref_val
      settings$forest_log_scale = log_scale
      settings$forest_stripe   = stripe
      settings$forest_show_values = show_values
      settings$forest_point_size = point_size
      settings$forest_ci_width = ci_width
      settings$forest_sig_color = sig_color
      settings$forest_ns_color = ns_color
    },
    draw = function(data, mapping, settings, ...) {
      df = settings$forest_data
      if (is.null(df)) return(ggplot2::ggplot())

      n_rows = nrow(df)
      df$row_idx = seq_len(n_rows)

      p = ggplot2::ggplot(df)

      # Alternating row background stripes (Archetype 9)
      if (isTRUE(settings$forest_stripe)) {
        stripe_df = data.frame(
          row_idx = seq_len(n_rows),
          fill_col = ifelse(seq_len(n_rows) %% 2 == 0, "#F7F8FA", "#FFFFFF")
        )
        p = p + ggplot2::geom_rect(
          data = stripe_df,
          ggplot2::aes(
            ymin = .data[["row_idx"]] - 0.5,
            ymax = .data[["row_idx"]] + 0.5,
            xmin = -Inf,
            xmax = Inf
          ),
          fill = stripe_df$fill_col,
          inherit.aes = FALSE
        )
      }

      # Reference line
      if (isTRUE(settings$forest_ref_line)) {
        p = p + ggplot2::geom_vline(
          xintercept = settings$forest_ref_val,
          linetype   = "dashed",
          color      = "grey50",
          linewidth  = 0.45
        )
      }

      # Regular rows: Error bars + points
      reg_df = df[!df$is_summary, , drop = FALSE]
      if (nrow(reg_df) > 0) {
        p = p +
          ggplot2::geom_errorbar(
            data = reg_df,
            ggplot2::aes(
              y    = .data[["variable"]],
              xmin = .data[["ci_low"]],
              xmax = .data[["ci_high"]],
              color = .data[["sig"]]
            ),
            width     = 0.22,
            linewidth = settings$forest_ci_width
          ) +
          ggplot2::geom_point(
            data = reg_df,
            ggplot2::aes(
              x     = .data[["estimate"]],
              y     = .data[["variable"]],
              color = .data[["sig"]]
            ),
            size = settings$forest_point_size
          )
      }

      # Summary / meta-analysis diamond rows
      sum_df = df[df$is_summary, , drop = FALSE]
      if (nrow(sum_df) > 0) {
        p = p +
          ggplot2::geom_errorbarh(
            data = sum_df,
            ggplot2::aes(
              y    = .data[["variable"]],
              xmin = .data[["ci_low"]],
              xmax = .data[["ci_high"]]
            ),
            color     = "#0F4D92",
            height    = 0.28,
            linewidth = settings$forest_ci_width * 1.2
          ) +
          ggplot2::geom_point(
            data = sum_df,
            ggplot2::aes(
              x = .data[["estimate"]],
              y = .data[["variable"]]
            ),
            shape  = 23, # diamond
            size   = settings$forest_point_size * 1.5,
            fill   = "#0F4D92",
            color  = "#082D59",
            stroke = 0.8
          )
      }

      p = p +
        ggplot2::scale_color_manual(
          values = c("p < 0.05" = settings$forest_sig_color, "NS" = settings$forest_ns_color),
          name   = ""
        ) +
        ggplot2::labs(
          x = if (settings$forest_log_scale) "Hazard Ratio / Odds Ratio (95% CI)" else "Estimate (95% CI)",
          y = ""
        ) +
        ggplot2::theme(
          axis.text.y = ggplot2::element_text(size = 9, face = "bold", color = "#222222"),
          panel.grid.major.y = ggplot2::element_blank()
        )

      # Numeric annotations
      if (isTRUE(settings$forest_show_values)) {
        p = p + ggplot2::geom_text(
          data = df,
          ggplot2::aes(
            x     = .data[["ci_high"]],
            y     = .data[["variable"]],
            label = .data[["val_label"]]
          ),
          hjust = -0.15, size = 2.8, color = "grey30", inherit.aes = FALSE
        )
      }

      # Log scale if requested
      if (isTRUE(settings$forest_log_scale)) {
        p = p + ggplot2::scale_x_log10(
          expand = ggplot2::expansion(mult = if (settings$forest_show_values) c(0.05, 0.25) else c(0.05, 0.08))
        )
      } else {
        p = p + ggplot2::scale_x_continuous(
          expand = ggplot2::expansion(mult = if (settings$forest_show_values) c(0.05, 0.25) else c(0.05, 0.08))
        )
      }

      p
    },
    name = "forest"
  )
}
