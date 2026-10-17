# ===========================================================================
# cliomicplot: Kaplan-Meier Survival Curve Type
# Enhanced with CNS Journal Clinical Figure Standards:
# - Zero-dependency native ggplot2 engine (independent of survminer)
# - Cumulative incidence mode ((1 - S(t)) * 100%)
# - Landmark milestone callouts with exact percentages (Clinical Archetype 5)
# - Aligned numbers-at-risk table
# - Median survival crosshair drop lines
# ===========================================================================

#' Kaplan-Meier Survival Curve & Cumulative Incidence Type
#'
#' @description Creates publication-ready Kaplan-Meier survival curves or
#'   cumulative incidence plots with optional risk tables, landmark milestone
#'   callouts, confidence intervals, median survival lines, and log-rank
#'   p-values. Driven by a native ggplot2 + survival engine with zero external
#'   dependencies, and compatible with survminer when requested.
#'
#' @param time Column name or vector of survival times.
#' @param event Column name or vector of event indicators (1 = event, 0 = censored).
#' @param risk_table Show risk table below the plot (default TRUE).
#' @param pval Show log-rank p-value on the plot (default TRUE).
#' @param conf_int Show confidence interval ribbon (default FALSE).
#' @param median_line Show median survival drop lines (default FALSE).
#' @param cumulative_incidence Plot cumulative incidence ((1 - S(t)) * 100 percent)
#'   instead of survival probability (default FALSE; Archetype 5).
#' @param milestones Numeric vector of landmark milestone times (e.g.,
#'   \code{c(6, 12, 24, 60)}) where exact percentages and point markers
#'   are annotated directly on the curve (Archetype 5).
#' @param censor Show tick markers at censored observations (default TRUE).
#' @param censor_size Size of censoring cross glyphs (default 1.8).
#' @param palette Color palette for groups (default "jama").
#' @param linewidth Line width for survival step curves (default 0.8).
#' @param xlab Label for x-axis (default "Time").
#' @param ylab Label for y-axis (default "Survival Probability" or "Cumulative Incidence (\%)").
#' @param engine Engine to use: \code{"auto"} (default, uses native engine with
#'   survminer fallback if requested), \code{"native"}, or \code{"survminer"}.
#'
#' @return A cliplot_type object for use with \code{\link{cliplot}}.
#'
#' @examples
#' \dontrun{
#' library(survival)
#' data(lung)
#'
#' # Basic KM plot
#' cliplot(Surv(time, status) ~ sex, data = lung, type = "km")
#'
#' # Landmark milestone callouts at 180, 365, 730 days with CI
#' cliplot(Surv(time, status) ~ sex, data = lung,
#'         type = type_km(conf_int = TRUE, milestones = c(180, 365, 730)))
#'
#' # Cumulative incidence mode
#' cliplot(Surv(time, status) ~ sex, data = lung,
#'         type = type_km(cumulative_incidence = TRUE, milestones = c(180, 365)))
#' }
#'
#' @export
type_km = function(
    time                 = NULL,
    event                = NULL,
    risk_table           = TRUE,
    pval                 = TRUE,
    conf_int             = FALSE,
    median_line          = FALSE,
    cumulative_incidence = FALSE,
    milestones           = NULL,
    censor               = TRUE,
    censor_size          = 1.8,
    palette              = NULL,
    linewidth            = 0.8,
    xlab                 = "Time",
    ylab                 = NULL,
    engine               = c("auto", "native", "survminer")
) {
  engine = match.arg(engine)

  cliplot_type(
    data = function(settings, ...) {
      df = settings$data
      group_var = settings$by

      if (is.null(df)) {
        stop("Kaplan-Meier requires a data frame with time, event, and group columns")
      }

      # Determine time/event column names
      time_col  = time  %||% settings$km_time
      event_col = event %||% settings$km_event

      if (is.null(time_col) || is.null(event_col)) {
        stop("Cannot determine time/event columns. Provide `time` and `event` to type_km().")
      }

      # Group variable
      group_col = settings$km_group %||% "..by.."

      if (!is.null(group_var)) {
        df[[group_col]] = as.factor(group_var)
        settings$km_n_groups = nlevels(df[[group_col]])
      } else {
        settings$km_n_groups = 1L
      }

      # Build formula
      if (is.null(group_var)) {
        surv_formula_text = paste0("survival::Surv(", time_col, ", ", event_col, ") ~ 1")
      } else {
        surv_formula_text = paste0("survival::Surv(", time_col, ", ", event_col, ") ~ ", group_col)
      }
      surv_formula = stats::as.formula(surv_formula_text)

      settings$surv_formula       = surv_formula
      settings$surv_formula_text  = surv_formula_text
      settings$km_time            = time_col
      settings$km_event           = event_col
      settings$km_group_col       = group_col
      settings$risk_table         = risk_table
      settings$pval               = pval
      settings$conf_int           = conf_int
      settings$median_line        = median_line
      settings$cumulative_incidence = cumulative_incidence
      settings$milestones         = milestones
      settings$censor             = censor
      settings$censor_size        = censor_size
      settings$palette            = palette %||% settings$palette %||% "jama"
      settings$linewidth          = linewidth
      settings$xlab               = xlab
      settings$ylab               = ylab %||% if (cumulative_incidence) "Cumulative Incidence (%)" else "Survival Probability"
      settings$km_engine          = engine
      settings$km_data            = df
    },
    draw = function(data, mapping, settings, ...) {
      df = settings$km_data
      if (is.null(df)) return(ggplot2::ggplot())

      # Dispatch to survminer only when explicitly requested and installed
      if (identical(settings$km_engine, "survminer") && requireNamespace("survminer", quietly = TRUE)) {
        return(.draw_km_survminer(df, settings))
      }

      # Native publication-grade ggplot2 survival engine
      .draw_km_native(df, settings)
    },
    name = "km"
  )
}

# Native ggplot2 survival engine with aligned risk table
.draw_km_native = function(df, settings) {
  group_col = settings$km_group_col %||% "..by.."
  n_groups  = settings$km_n_groups %||% 1L
  is_cuminc = isTRUE(settings$cumulative_incidence)

  # Palette
  pal = get_cli_palette(settings$palette %||% "jama", max(n_groups, 1L))

  # Fit survival model
  fit = tryCatch(
    survival::survfit(settings$surv_formula, data = df),
    error = function(e) {
      # Fallback in case of scoping issue
      df_env = list2env(df, parent = globalenv())
      f = stats::as.formula(settings$surv_formula_text, env = df_env)
      survival::survfit(f, data = df)
    }
  )

  # Strata handling
  strata_names = names(fit$strata)
  if (is.null(strata_names)) {
    strata = rep("All", length(fit$time))
    group_levels = "All"
  } else {
    strata = rep(sub("^[^=]*=", "", strata_names), fit$strata)
    group_levels = unique(strata)
    if (group_col %in% names(df) && is.factor(df[[group_col]])) {
      group_levels = levels(df[[group_col]])
    }
  }

  td = data.frame(
    time     = fit$time,
    surv     = fit$surv,
    lower    = fit$lower,
    upper    = fit$upper,
    group    = strata,
    n_censor = fit$n.censor,
    n_event  = fit$n.event,
    stringsAsFactors = FALSE
  )

  # Clean bounds
  td$lower[is.na(td$lower)] = td$surv[is.na(td$lower)]
  td$upper[is.na(td$upper)] = td$surv[is.na(td$upper)]
  td$lower = pmin(pmax(td$lower, 0), 1)
  td$upper = pmin(pmax(td$upper, 0), 1)

  # Prepend origin at t = 0
  origin = do.call(rbind, lapply(group_levels, function(g) {
    data.frame(
      time = 0, surv = 1, lower = 1, upper = 1,
      group = g, n_censor = 0, n_event = 0,
      stringsAsFactors = FALSE
    )
  }))
  td = rbind(origin, td)
  td = td[order(td$group, td$time), ]
  td$group = factor(td$group, levels = group_levels)

  # Transform for cumulative incidence if requested
  if (is_cuminc) {
    td$y_val = (1 - td$surv) * 100
    td$y_lo  = (1 - td$upper) * 100
    td$y_hi  = (1 - td$lower) * 100
  } else {
    td$y_val = td$surv
    td$y_lo  = td$lower
    td$y_hi  = td$upper
  }

  names(pal) = group_levels

  # Base plot
  p = ggplot2::ggplot(td, ggplot2::aes(x = .data[["time"]], y = .data[["y_val"]],
                                       color = .data[["group"]], fill = .data[["group"]]))

  # Shaded Confidence Interval Ribbon (step vertices)
  if (isTRUE(settings$conf_int)) {
    step_ribbon = do.call(rbind, lapply(split(td, td$group), function(s) {
      n = nrow(s)
      if (n <= 1) return(data.frame(time = s$time, y_lo = s$y_lo, y_hi = s$y_hi, group = s$group[1]))
      t_step = c(s$time[1], rep(s$time[-1], each = 2))
      lo_step = c(rep(s$y_lo[-n], each = 2), s$y_lo[n])
      hi_step = c(rep(s$y_hi[-n], each = 2), s$y_hi[n])
      data.frame(time = t_step, y_lo = lo_step, y_hi = hi_step, group = s$group[1], stringsAsFactors = FALSE)
    }))
    step_ribbon$group = factor(step_ribbon$group, levels = group_levels)

    p = p + ggplot2::geom_ribbon(
      data = step_ribbon,
      ggplot2::aes(x = .data[["time"]], ymin = .data[["y_lo"]], ymax = .data[["y_hi"]], fill = .data[["group"]]),
      alpha = 0.16, color = NA, inherit.aes = FALSE
    )
  }

  # Step curve
  p = p + ggplot2::geom_step(linewidth = settings$linewidth)

  # Censoring points
  if (isTRUE(settings$censor)) {
    censor_pts = td[td$n_censor > 0, , drop = FALSE]
    if (nrow(censor_pts) > 0) {
      p = p + ggplot2::geom_point(
        data = censor_pts,
        ggplot2::aes(x = .data[["time"]], y = .data[["y_val"]]),
        shape = 3, size = settings$censor_size, stroke = 0.5, show.legend = FALSE
      )
    }
  }

  # Median survival crosshair drop lines
  if (isTRUE(settings$median_line) && !is_cuminc) {
    meds = do.call(rbind, lapply(split(td, td$group), function(s) {
      s = s[order(s$time), , drop = FALSE]
      hit = which(s$surv <= 0.5)
      if (length(hit) == 0) return(NULL)
      data.frame(x = s$time[hit[1]], y = 0.5, group = s$group[1])
    }))
    if (!is.null(meds) && nrow(meds) > 0) {
      p = p +
        ggplot2::geom_segment(
          data = meds,
          ggplot2::aes(x = 0, xend = .data[["x"]], y = .data[["y"]], yend = .data[["y"]]),
          linetype = "dotted", linewidth = 0.45, color = "#555555", inherit.aes = FALSE
        ) +
        ggplot2::geom_segment(
          data = meds,
          ggplot2::aes(x = .data[["x"]], xend = .data[["x"]], y = 0, yend = .data[["y"]]),
          linetype = "dotted", linewidth = 0.45, color = "#555555", inherit.aes = FALSE
        )
    }
  }

  # Landmark milestone callouts (Archetype 5)
  if (!is.null(settings$milestones) && length(settings$milestones) > 0) {
    ms = sort(unique(as.numeric(settings$milestones)))
    ms_summary = suppressWarnings(summary(fit, times = ms, extend = TRUE))
    if (!is.null(ms_summary$time) && length(ms_summary$time) > 0) {
      ms_strata = if (is.null(ms_summary$strata)) rep("All", length(ms_summary$time))
                  else sub("^[^=]*=", "", as.character(ms_summary$strata))

      ms_y = if (is_cuminc) (1 - ms_summary$surv) * 100 else ms_summary$surv
      ms_df = data.frame(
        time  = ms_summary$time,
        y_val = ms_y,
        group = factor(ms_strata, levels = group_levels),
        stringsAsFactors = FALSE
      )
      ms_df$lab = if (is_cuminc) sprintf("%.1f%%", ms_df$y_val)
                  else sprintf("%.2f", ms_df$y_val)

      p = p +
        ggplot2::geom_point(
          data = ms_df,
          ggplot2::aes(x = .data[["time"]], y = .data[["y_val"]], color = .data[["group"]]),
          size = 2.8, inherit.aes = FALSE
        ) +
        ggplot2::geom_text(
          data = ms_df,
          ggplot2::aes(x = .data[["time"]], y = .data[["y_val"]], label = .data[["lab"]], color = .data[["group"]]),
          vjust = if (is_cuminc) -1.2 else 1.8,
          fontface = "bold", size = 3.0, inherit.aes = FALSE, show.legend = FALSE
        )
    }
  }

  # Log-rank p-value
  if (isTRUE(settings$pval) && n_groups > 1) {
    diff = tryCatch(survival::survdiff(settings$surv_formula, data = df), error = function(e) NULL)
    if (!is.null(diff)) {
      pval_num = 1 - stats::pchisq(diff$chisq, length(diff$n) - 1)
      pval_txt = if (pval_num < 0.001) "Log-rank p < 0.001" else sprintf("Log-rank p = %.3f", pval_num)

      # Annotate at bottom-left for survival, top-left for cumulative incidence
      ann_x = max(td$time, na.rm = TRUE) * 0.04
      ann_y = if (is_cuminc) 92 else 0.08
      p = p + ggplot2::annotate("text", x = ann_x, y = ann_y, label = pval_txt,
                                hjust = 0, size = 3.2, fontface = "italic", color = "#222222")
    }
  }

  # Axis limits and labels
  if (is_cuminc) {
    p = p + ggplot2::scale_y_continuous(limits = c(0, NA), expand = ggplot2::expansion(mult = c(0.01, 0.12)))
  } else {
    p = p + ggplot2::scale_y_continuous(limits = c(0, 1.02), breaks = seq(0, 1, 0.2),
                                        labels = function(x) paste0(round(x * 100), "%"),
                                        expand = ggplot2::expansion(mult = c(0.01, 0.05)))
  }

  p = p +
    ggplot2::scale_color_manual(values = pal, name = "") +
    ggplot2::scale_fill_manual(values = pal, name = "") +
    ggplot2::labs(x = settings$xlab, y = settings$ylab)

  if (n_groups == 1) {
    p = p + ggplot2::theme(legend.position = "none")
  }

  # Numbers at risk table
  if (isTRUE(settings$risk_table)) {
    time_max = max(td$time, na.rm = TRUE)
    time_breaks = pretty(c(0, time_max), n = 5)
    time_breaks = sort(unique(time_breaks[time_breaks >= 0 & time_breaks <= time_max]))
    if (length(time_breaks) < 2) time_breaks = c(0, time_max)

    r_sum = suppressWarnings(summary(fit, times = time_breaks, extend = TRUE))
    if (!is.null(r_sum$time) && length(r_sum$time) > 0) {
      r_strata = if (is.null(r_sum$strata)) rep("All", length(r_sum$time))
                 else sub("^[^=]*=", "", as.character(r_sum$strata))
      rt_df = data.frame(
        time   = r_sum$time,
        n.risk = r_sum$n.risk,
        group  = factor(r_strata, levels = rev(group_levels)),
        stringsAsFactors = FALSE
      )

      rt_plot = ggplot2::ggplot(rt_df, ggplot2::aes(x = .data[["time"]], y = .data[["group"]],
                                                    label = .data[["n.risk"]])) +
        ggplot2::geom_text(size = 3.0, color = "#333333") +
        ggplot2::scale_x_continuous(limits = ggplot2::layer_scales(p)$x$get_limits(),
                                    breaks = time_breaks) +
        ggplot2::labs(x = settings$xlab, y = "") +
        theme_cli_base(base_size = 9) +
        ggplot2::theme(
          panel.grid       = ggplot2::element_blank(),
          axis.line        = ggplot2::element_blank(),
          axis.ticks       = ggplot2::element_blank(),
          axis.text.x      = ggplot2::element_blank(),
          axis.title.x     = ggplot2::element_blank(),
          axis.text.y      = ggplot2::element_text(face = "bold", size = 8),
          plot.margin      = ggplot2::margin(0, 5, 5, 5, "pt"),
          plot.title       = ggplot2::element_text(size = 8, face = "bold")
        ) +
        ggplot2::ggtitle("Number at risk")

      # Align time axis on main plot
      p = p + ggplot2::scale_x_continuous(breaks = time_breaks)

      if (requireNamespace("patchwork", quietly = TRUE)) {
        p = patchwork::wrap_plots(p, rt_plot, ncol = 1, heights = c(3.5, 1))
      }
    }
  }

  p
}

# Optional survminer wrapper for backwards compatibility
.draw_km_survminer = function(df, settings) {
  group_col = settings$km_group_col %||% "..by.."
  n_groups  = settings$km_n_groups %||% 1L
  pal       = get_cli_palette(settings$palette %||% "jco", n_groups)
  fit       = survival::survfit(settings$surv_formula, data = df)

  df_env = new.env(parent = globalenv())
  for (nm in names(df)) assign(nm, df[[nm]], envir = df_env)
  fit$call$formula = stats::as.formula(settings$surv_formula_text, env = df_env)

  legend_labs = if (n_groups > 1 && group_col %in% names(df)) levels(as.factor(df[[group_col]])) else NULL

  p = survminer::ggsurvplot(
    fit,
    data          = df,
    pval          = settings$pval,
    conf.int      = settings$conf_int,
    risk.table    = settings$risk_table,
    surv.median.line = if (settings$median_line) "hv" else "none",
    palette       = pal,
    size          = settings$linewidth,
    xlab          = settings$xlab,
    ylab          = settings$ylab,
    ggtheme       = theme_cli_base(),
    legend.title  = "",
    legend.labs   = legend_labs
  )

  if (settings$risk_table) {
    if (requireNamespace("patchwork", quietly = TRUE)) {
      p = patchwork::wrap_plots(p$plot, p$table, ncol = 1, heights = c(3, 1))
    } else {
      p = p$plot
    }
  } else {
    p = p$plot
  }
  p
}
