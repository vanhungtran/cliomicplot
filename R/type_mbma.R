# ===========================================================================
# cliomicplot: Model-Based Meta-Analysis (MBMA) and Placebo Kinetics Types
# Implements longitudinal trajectory modeling, trial design grid matrices,
# and benchmark model-versus-observed validation.
# Reference: Serrano et al. (2025/2026), CPT: Pharmacometrics & Systems Pharmacology
# ===========================================================================

#' MBMA Longitudinal Trajectory Type
#'
#' @description Creates publication-ready Model-Based Meta-Analysis (MBMA)
#'   trajectory plots. Displays non-linear longitudinal placebo response kinetics,
#'   confidence/credible interval ribbons, stratified covariate projections (e.g.
#'   baseline disease severity tertiles or concomitant therapy status), and
#'   overlaid clinical trial arm observations sized proportionally to sample size.
#'
#' @param style Plot aesthetic style: \code{"longitudinal"} (time/weeks on x-axis),
#'   \code{"continuous"} (baseline covariate on x-axis), or \code{"divergence"}
#'   (paired baseline contrast trajectories).
#' @param show_points Logical. Whether to overlay observed trial arm points
#'   (default \code{TRUE}).
#' @param point_size_var Column name for point sizing (e.g. sample size \code{"n"}
#'   or \code{"sample_size"}). Default \code{NULL}.
#' @param point_size_range Numeric vector of length 2 specifying min and max
#'   point sizes (default \code{c(1.5, 6.0)}).
#' @param show_ribbon Logical. Whether to draw shaded 95\% CI/CrI ribbon
#'   (default \code{TRUE}).
#' @param ribbon_alpha Transparency of confidence ribbon (default \code{0.18}).
#' @param line_width Linewidth of model trajectory curve (default \code{1.1}).
#' @param plateau_week Optional numeric. Draws a vertical dotted reference line
#'   and callout at the specified week where placebo response plateaus (e.g. \code{12}).
#' @param plateau_text Annotation label for the plateau line (default \code{"94\% plateau"}).
#' @param colors Named character vector or palette name for stratification groups.
#' @param ... Additional arguments.
#'
#' @return A \code{cliplot_type} object for use with \code{\link{cliplot}}.
#'
#' @export
type_mbma_trajectory = function(
    style            = c("longitudinal", "continuous", "divergence"),
    show_points      = TRUE,
    point_size_var   = NULL,
    point_size_range = c(1.5, 6.0),
    show_ribbon      = TRUE,
    ribbon_alpha     = 0.18,
    line_width       = 1.1,
    plateau_week     = NULL,
    plateau_text     = "94% plateau",
    colors           = NULL,
    ...
) {
  style = match.arg(style)

  cliplot_type(
    data = function(settings, ...) {
      df = data.frame(
        x = settings$x,
        y = as.numeric(settings$y),
        stringsAsFactors = FALSE
      )
      if (!is.null(settings$by)) {
        df$by = as.factor(settings$by)
      }

      raw_data = settings$data
      if (!is.null(raw_data) && is.data.frame(raw_data)) {
        nms = names(raw_data)
        if ("ymin" %in% nms) df$ymin = raw_data$ymin
        if ("ymax" %in% nms) df$ymax = raw_data$ymax
        if ("lower" %in% nms) df$ymin = raw_data$lower
        if ("upper" %in% nms) df$ymax = raw_data$upper
        if ("ci_lo" %in% nms) df$ymin = raw_data$ci_lo
        if ("ci_hi" %in% nms) df$ymax = raw_data$ci_hi

        if (!is.null(point_size_var) && point_size_var %in% nms) {
          df$pt_size = raw_data[[point_size_var]]
        } else if ("n" %in% nms) {
          df$pt_size = raw_data$n
        } else if ("sample_size" %in% nms) {
          df$pt_size = raw_data$sample_size
        } else if ("n_patients" %in% nms) {
          df$pt_size = raw_data$n_patients
        }

        if ("is_model" %in% nms) {
          df$is_model = raw_data$is_model
        }
      }

      settings$mbma_df         = df
      settings$mbma_style      = style
      settings$mbma_show_pts   = show_points
      settings$mbma_pt_range   = point_size_range
      settings$mbma_show_rib   = show_ribbon
      settings$mbma_rib_alpha  = ribbon_alpha
      settings$mbma_line_width = line_width
      settings$mbma_plateau    = plateau_week
      settings$mbma_plat_txt   = plateau_text
      settings$mbma_colors     = colors
    },
    draw = function(data, mapping, settings, ...) {
      df = settings$mbma_df
      if (is.null(df) || nrow(df) == 0) return(ggplot2::ggplot())

      has_by     = "by" %in% names(df)
      has_ribbon = settings$mbma_show_rib && ("ymin" %in% names(df)) && ("ymax" %in% names(df))
      has_pts    = settings$mbma_show_pts && ("pt_size" %in% names(df))

      # Split model curves from observed points if flagged
      if ("is_model" %in% names(df)) {
        model_df = df[df$is_model == TRUE | df$is_model == 1, , drop = FALSE]
        pts_df   = df[df$is_model == FALSE | df$is_model == 0, , drop = FALSE]
      } else {
        model_df = df
        pts_df   = df
      }

      p = ggplot2::ggplot()

      # 1. Shaded confidence ribbon
      if (has_ribbon && nrow(model_df) > 0) {
        if (has_by) {
          p = p + ggplot2::geom_ribbon(
            data = model_df,
            ggplot2::aes(x = .data[["x"]], ymin = .data[["ymin"]], ymax = .data[["ymax"]], fill = .data[["by"]]),
            alpha = settings$mbma_rib_alpha, color = NA, inherit.aes = FALSE
          )
        } else {
          p = p + ggplot2::geom_ribbon(
            data = model_df,
            ggplot2::aes(x = .data[["x"]], ymin = .data[["ymin"]], ymax = .data[["ymax"]]),
            fill = "#2980B9", alpha = settings$mbma_rib_alpha, color = NA, inherit.aes = FALSE
          )
        }
      }

      # 2. Model trajectory lines
      if (nrow(model_df) > 0) {
        if (has_by) {
          p = p + ggplot2::geom_line(
            data = model_df,
            ggplot2::aes(x = .data[["x"]], y = .data[["y"]], color = .data[["by"]]),
            linewidth = settings$mbma_line_width, inherit.aes = FALSE
          )
        } else {
          p = p + ggplot2::geom_line(
            data = model_df,
            ggplot2::aes(x = .data[["x"]], y = .data[["y"]]),
            color = "#2980B9", linewidth = settings$mbma_line_width, inherit.aes = FALSE
          )
        }
      }

      # 3. Observed clinical trial arm points
      if (settings$mbma_show_pts && nrow(pts_df) > 0) {
        if (has_pts) {
          if (has_by) {
            p = p + ggplot2::geom_point(
              data = pts_df,
              ggplot2::aes(x = .data[["x"]], y = .data[["y"]], size = .data[["pt_size"]], color = .data[["by"]]),
              alpha = 0.65, inherit.aes = FALSE
            )
          } else {
            p = p + ggplot2::geom_point(
              data = pts_df,
              ggplot2::aes(x = .data[["x"]], y = .data[["y"]], size = .data[["pt_size"]]),
              color = "#2980B9", alpha = 0.65, inherit.aes = FALSE
            )
          }
          p = p + ggplot2::scale_size_continuous(
            range = settings$mbma_pt_range,
            name = "Sample size (N)"
          )
        } else {
          if (has_by) {
            p = p + ggplot2::geom_point(
              data = pts_df,
              ggplot2::aes(x = .data[["x"]], y = .data[["y"]], color = .data[["by"]]),
              size = 2.5, alpha = 0.65, inherit.aes = FALSE
            )
          } else {
            p = p + ggplot2::geom_point(
              data = pts_df,
              ggplot2::aes(x = .data[["x"]], y = .data[["y"]]),
              color = "#2980B9", size = 2.5, alpha = 0.65, inherit.aes = FALSE
            )
          }
        }
      }

      # 4. Plateau line and text callout
      if (!is.null(settings$mbma_plateau)) {
        p = p + ggplot2::geom_vline(
          xintercept = settings$mbma_plateau,
          linetype = "dotted", color = "#7F8C8D", linewidth = 0.7
        )
        y_min_val = min(df$y, na.rm = TRUE)
        p = p + ggplot2::annotate(
          "text",
          x = settings$mbma_plateau + 0.35,
          y = y_min_val + 2.0,
          label = settings$mbma_plat_txt,
          size = 3.2, color = "#555555", hjust = 0, fontface = "italic"
        )
      }

      # 5. Palettes
      if (!is.null(settings$mbma_colors)) {
        p = p + ggplot2::scale_color_manual(values = settings$mbma_colors)
        p = p + ggplot2::scale_fill_manual(values = settings$mbma_colors)
      }

      p
    }
  )
}

#' MBMA Trial Design Grid Heatmap Type
#'
#' @description Creates simulation matrix heatmaps for clinical trial design
#'   exploration (Figure 3A in Serrano et al. 2025). Displays predicted response
#'   rates across combinations of baseline covariates (e.g. baseline severity
#'   scores), assessment timepoints (e.g. Weeks 8, 12, 16), and trial design
#'   policies (e.g. Monotherapy vs. Concomitant TCS). Each cell displays the
#'   central percentage in bold with its corresponding \code{± 95\% CI} margin
#'   underneath.
#'
#' @param ci_margin Column name for confidence interval half-width (margin), or
#'   \code{NULL} if already formatted or not shown.
#' @param show_labels Logical. Whether to print two-line text annotations inside
#'   cells (default \code{TRUE}).
#' @param label_digits Number of decimal digits for percentage values (default \code{1}).
#' @param palette Color gradient palette name: \code{"blues"} (default),
#'   \code{"teal"}, \code{"viridis"}, or \code{"sunset"}.
#' @param tile_border Color for tile dividing gridlines (default \code{"#FFFFFF"}).
#' @param tile_border_lwd Linewidth for tile borders (default \code{0.75}).
#' @param text_size Numeric text size for cell estimates (default \code{3.4}).
#' @param ci_size Numeric text size for CI margin text (default \code{2.4}).
#' @param ... Additional arguments.
#'
#' @return A \code{cliplot_type} object for use with \code{\link{cliplot}}.
#'
#' @export
type_mbma_grid = function(
    ci_margin       = NULL,
    show_labels     = TRUE,
    label_digits    = 1,
    palette         = c("blues", "teal", "viridis", "sunset"),
    tile_border     = "#FFFFFF",
    tile_border_lwd = 0.75,
    text_size       = 3.4,
    ci_size         = 2.4,
    ...
) {
  palette = match.arg(palette)

  cliplot_type(
    data = function(settings, ...) {
      x_vals = settings$x
      y_vals = settings$y

      df = data.frame(
        x = as.factor(x_vals),
        y = as.factor(y_vals),
        stringsAsFactors = FALSE
      )

      raw_data = settings$data
      if (!is.null(raw_data) && is.data.frame(raw_data)) {
        nms = names(raw_data)
        if ("rate" %in% nms) {
          df$value = as.numeric(raw_data$rate)
        } else if ("pct" %in% nms) {
          df$value = as.numeric(raw_data$pct)
        } else if ("response" %in% nms) {
          df$value = as.numeric(raw_data$response)
        } else if ("estimate" %in% nms) {
          df$value = as.numeric(raw_data$estimate)
        } else {
          df$value = as.numeric(y_vals)
        }

        if (!is.null(ci_margin) && ci_margin %in% nms) {
          df$ci_margin = as.numeric(raw_data[[ci_margin]])
        } else if ("margin" %in% nms) {
          df$ci_margin = as.numeric(raw_data$margin)
        } else if ("ci" %in% nms) {
          df$ci_margin = as.numeric(raw_data$ci)
        } else if ("se" %in% nms) {
          df$ci_margin = as.numeric(raw_data$se) * 1.96
        }

        if ("facet" %in% nms) df$facet = as.factor(raw_data$facet)
        if ("stratum" %in% nms) df$facet = as.factor(raw_data$stratum)
        if ("design" %in% nms) df$facet = as.factor(raw_data$design)
      } else {
        df$value = as.numeric(y_vals)
      }

      if (!is.null(settings$by)) {
        df$facet = as.factor(settings$by)
      }

      if ("ci_margin" %in% names(df) && !all(is.na(df$ci_margin))) {
        df$label_main = sprintf(paste0("%.", label_digits, "f%%"), df$value)
        df$label_sub  = sprintf(paste0("\u00B1 %.", label_digits, "f%%"), df$ci_margin)
      } else {
        df$label_main = sprintf(paste0("%.", label_digits, "f%%"), df$value)
        df$label_sub  = ""
      }

      settings$grid_df        = df
      settings$grid_show_lbl  = show_labels
      settings$grid_pal       = palette
      settings$grid_border    = tile_border
      settings$grid_bwd       = tile_border_lwd
      settings$grid_txt_size  = text_size
      settings$grid_ci_size   = ci_size
    },
    draw = function(data, mapping, settings, ...) {
      df = settings$grid_df
      if (is.null(df) || nrow(df) == 0) return(ggplot2::ggplot())

      p = ggplot2::ggplot(df, ggplot2::aes(x = .data[["x"]], y = .data[["y"]], fill = .data[["value"]])) +
        ggplot2::geom_tile(color = settings$grid_border, linewidth = settings$grid_bwd)

      if (settings$grid_pal == "blues") {
        p = p + ggplot2::scale_fill_gradient(
          low = "#132B43", high = "#56B1F7",
          name = "Response (%)",
          guide = ggplot2::guide_colorbar(barwidth = 9, barheight = 0.5)
        )
      } else if (settings$grid_pal == "teal") {
        p = p + ggplot2::scale_fill_gradient(
          low = "#0E3B3A", high = "#4AD9D9",
          name = "Response (%)",
          guide = ggplot2::guide_colorbar(barwidth = 9, barheight = 0.5)
        )
      } else if (settings$grid_pal == "viridis") {
        p = p + ggplot2::scale_fill_viridis_c(
          option = "D",
          name = "Response (%)",
          guide = ggplot2::guide_colorbar(barwidth = 9, barheight = 0.5)
        )
      } else {
        p = p + ggplot2::scale_fill_gradient2(
          low = "#2980B9", mid = "#F8F9FA", high = "#E74C3C",
          midpoint = mean(df$value, na.rm = TRUE),
          name = "Response (%)",
          guide = ggplot2::guide_colorbar(barwidth = 9, barheight = 0.5)
        )
      }

      if (settings$grid_show_lbl) {
        val_range = range(df$value, na.rm = TRUE)
        mid_val   = mean(val_range)
        text_colors = ifelse(df$value < mid_val, "#FFFFFF", "#111111")

        p = p + ggplot2::geom_text(
          ggplot2::aes(label = .data[["label_main"]]),
          color = text_colors,
          fontface = "bold",
          size = settings$grid_txt_size,
          vjust = -0.3
        )

        has_sub = any(nzchar(df$label_sub))
        if (has_sub) {
          p = p + ggplot2::geom_text(
            ggplot2::aes(label = .data[["label_sub"]]),
            color = text_colors,
            size = settings$grid_ci_size,
            vjust = 1.3
          )
        }
      }

      if ("facet" %in% names(df)) {
        p = p + ggplot2::facet_wrap(~ .data[["facet"]])
      }

      p
    }
  )
}

#' @rdname type_mbma_grid
#' @export
type_trial_design_grid = type_mbma_grid

#' MBMA Model-versus-Observed Benchmark Validation Type
#'
#' @description Creates benchmark validation plots comparing clinical trial
#'   distributions against Model-Based Meta-Analysis projections (Figure 3C in
#'   Serrano et al. 2025). Overlays trial-level boxplots and sample-size-scaled
#'   observations onto horizontal model prediction bands (mean line + shaded 95\% CI).
#'
#' @param model_mean Numeric scalar. Expected response rate from the model.
#' @param model_ci_low Numeric scalar. Lower bound of the model 95\% CI.
#' @param model_ci_high Numeric scalar. Upper bound of the model 95\% CI.
#' @param point_size_var Column name for point sizing (default \code{"n"}).
#' @param box_width Numeric boxplot width (default \code{0.45}).
#' @param jitter_width Numeric jitter width (default \code{0.14}).
#' @param band_col Color of model projected mean line (default \code{"#2C3E50"}).
#' @param band_fill Fill color of model projected CI band (default \code{"#BDC3C7"}).
#' @param band_alpha Transparency of model projected CI band (default \code{0.35}).
#' @param ... Additional arguments.
#'
#' @return A \code{cliplot_type} object for use with \code{\link{cliplot}}.
#'
#' @export
type_mbma_benchmark = function(
    model_mean      = NULL,
    model_ci_low    = NULL,
    model_ci_high   = NULL,
    point_size_var  = NULL,
    box_width       = 0.45,
    jitter_width    = 0.14,
    band_col        = "#2C3E50",
    band_fill       = "#BDC3C7",
    band_alpha      = 0.35,
    ...
) {
  cliplot_type(
    data = function(settings, ...) {
      df = data.frame(
        x = as.factor(settings$x),
        y = as.numeric(settings$y),
        stringsAsFactors = FALSE
      )
      if (!is.null(settings$by)) {
        df$by = as.factor(settings$by)
      }

      raw_data = settings$data
      if (!is.null(raw_data) && is.data.frame(raw_data)) {
        nms = names(raw_data)
        if (!is.null(point_size_var) && point_size_var %in% nms) {
          df$pt_size = raw_data[[point_size_var]]
        } else if ("n" %in% nms) {
          df$pt_size = raw_data$n
        } else if ("sample_size" %in% nms) {
          df$pt_size = raw_data$sample_size
        } else if ("n_patients" %in% nms) {
          df$pt_size = raw_data$n_patients
        }
      }

      settings$bench_df        = df
      settings$bench_mean      = model_mean
      settings$bench_ci_lo     = model_ci_low
      settings$bench_ci_hi     = model_ci_high
      settings$bench_bwidth    = box_width
      settings$bench_jwidth    = jitter_width
      settings$bench_bcol      = band_col
      settings$bench_bfill     = band_fill
      settings$bench_balpha    = band_alpha
    },
    draw = function(data, mapping, settings, ...) {
      df = settings$bench_df
      if (is.null(df) || nrow(df) == 0) return(ggplot2::ggplot())

      has_pts = "pt_size" %in% names(df)

      p = ggplot2::ggplot(df, ggplot2::aes(x = .data[["x"]], y = .data[["y"]]))

      # 1. Model forecast band if provided
      if (!is.null(settings$bench_ci_lo) && !is.null(settings$bench_ci_hi)) {
        p = p + ggplot2::geom_rect(
          ggplot2::aes(
            xmin = -Inf, xmax = Inf,
            ymin = settings$bench_ci_lo, ymax = settings$bench_ci_hi
          ),
          fill = settings$bench_bfill, alpha = settings$bench_balpha,
          inherit.aes = FALSE
        )
      }
      if (!is.null(settings$bench_mean)) {
        p = p + ggplot2::geom_hline(
          yintercept = settings$bench_mean,
          color = settings$bench_bcol, linewidth = 0.9, linetype = "solid"
        )
      }

      # 2. Boxplots
      p = p + ggplot2::geom_boxplot(
        width = settings$bench_bwidth,
        outlier.shape = NA,
        fill = "#FFFFFF",
        color = "#2C3E50",
        linewidth = 0.65,
        alpha = 0.9
      )

      # 3. Jittered points
      if (has_pts) {
        p = p + ggplot2::geom_jitter(
          ggplot2::aes(size = .data[["pt_size"]]),
          width = settings$bench_jwidth,
          color = "#2980B9", alpha = 0.65
        ) +
          ggplot2::scale_size_continuous(range = c(2.0, 6.5), name = "Sample size (N)")
      } else {
        p = p + ggplot2::geom_jitter(
          width = settings$bench_jwidth,
          color = "#2980B9", size = 2.8, alpha = 0.65
        )
      }

      p
    }
  )
}
