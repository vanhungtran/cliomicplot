# ===========================================================================
# cliomicplot: Forest Plot Type
# For hazard ratios, odds ratios, and meta-analysis
# ===========================================================================

#' Forest Plot Type
#'
#' @description Creates publication-ready forest plots for displaying hazard
#'   ratios, odds ratios, and confidence intervals from survival analysis,
#'   logistic regression, or meta-analysis.
#'
#' @param estimate Column name or vector of point estimates (HR, OR, etc.)
#' @param ci_low Column name or vector of lower CI bounds
#' @param ci_high Column name or vector of upper CI bounds
#' @param variable Column name or vector of variable labels
#' @param ref_line Reference line at x = 1 (default TRUE)
#' @param sort Sort by estimate (default TRUE)
#' @param point_size Point size for estimates (default 3)
#' @param ci_width CI line width (default 1)
#' @param sig_color Color for significant results
#' @param ns_color Color for non-significant results
#'
#' @return A cliplot_type object for use with \code{\link{cliplot}}.
#'
#' @examples
#' \dontrun{
#' # From a data frame with columns: Variable, HR, CI_low, CI_high, P
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
    estimate   = NULL,
    ci_low     = NULL,
    ci_high    = NULL,
    variable   = NULL,
    ref_line   = TRUE,
    sort       = TRUE,
    point_size = 3,
    ci_width   = 1,
    sig_color  = "#E64B35",
    ns_color   = "#3C5488"
) {
  cliplot_type(
    data = function(settings, ...) {
      # Build forest plot data from settings
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
        # Try to detect columns
        nms = names(df)
        df$estimate = settings$y %||% df[[nms[1]]]
        df$variable = settings$x %||% df[[nms[2]]]
        df$ci_low   = settings$ymin %||%
          (if ("CI_low" %in% nms) df$CI_low
           else if ("lower" %in% nms) df$lower
           else df$estimate * 0.7)
        df$ci_high  = settings$ymax %||%
          (if ("CI_high" %in% nms) df$CI_high
           else if ("upper" %in% nms) df$upper
           else df$estimate * 1.4)
        df$pval     = if ("P" %in% nms) df$P
                      else if ("pval" %in% nms) df$pval
                      else rep(0.01, nrow(df))
      }

      df$variable = as.character(df$variable)
      df$sig = ifelse(df$pval < 0.05, "p < 0.05", "NS")

      # Sort by estimate
      if (sort) {
        df$variable = factor(df$variable, levels = df$variable[order(df$estimate)])
      } else {
        df$variable = factor(df$variable, levels = rev(unique(df$variable)))
      }

      settings$forest_data = df
    },
    draw = function(data, mapping, settings, ...) {
      df = settings$forest_data
      if (is.null(df)) return(ggplot2::ggplot())

      p = ggplot2::ggplot(df, ggplot2::aes(
        x     = .data[["estimate"]],
        y     = .data[["variable"]],
        color = .data[["sig"]]
      )) +
        ggplot2::geom_point(size = point_size) +
        ggplot2::geom_errorbarh(
          ggplot2::aes(xmin = .data[["ci_low"]], xmax = .data[["ci_high"]]),
          height   = 0.2,
          linewidth = ci_width
        ) +
        ggplot2::scale_color_manual(
          values = c("p < 0.05" = sig_color, "NS" = ns_color),
          name   = ""
        ) +
        ggplot2::labs(x = "Hazard Ratio (95% CI)", y = "") +
        ggplot2::theme(
          axis.text.y = ggplot2::element_text(size = 10)
        )

      # Reference line at 1
      if (ref_line) {
        p = p + ggplot2::geom_vline(
          xintercept = 1,
          linetype   = "dashed",
          color      = "grey50",
          linewidth  = 0.5
        )
      }

      # Log scale for HR/OR
      p = p + ggplot2::scale_x_log10()

      p
    },
    name = "forest"
  )
}
