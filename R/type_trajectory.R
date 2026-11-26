# ===========================================================================
# cliomicplot: Longitudinal Trajectory with Clinical Severity Bands
# Clinical Figure Archetype 4 (clinical and cohort figure standards)
# ===========================================================================

#' Longitudinal Clinical Trajectory Plot Type
#'
#' @description Creates publication-ready longitudinal clinical trajectories
#'   (Clinical Archetype 4). Overlays semi-transparent individual patient
#'   spaghetti trajectories onto shaded background clinical severity bands
#'   (e.g., Mild, Moderate, Severe) or clean clinical frames, anchored by
#'   bold group mean trajectories with standard error bars (or 95\% CI ribbons),
#'   distinct markers across recovery time points, and sample-size annotations.
#'
#' @param patient_id Column name for individual patient/participant IDs.
#' @param time Column name for longitudinal visit/time values.
#' @param score Column name for clinical score/measurement.
#' @param group Optional column name for treatment arm or cohort stratification.
#' @param severity_bands Named list defining intervals for background severity
#'   shading (default \code{list(Mild = c(0, 25), Moderate = c(25, 50), Severe = c(50, 100))}).
#'   Set to \code{NULL} for clean background without bands.
#' @param severity_colors Named vector of fill colors for severity bands
#'   (default \code{c(Mild = "#D9EEF7", Moderate = "#FDE8D7", Severe = "#FADBD8")}).
#' @param spaghetti_alpha Opacity of individual patient trajectories (default 0.18).
#' @param spaghetti_width Line width of individual trajectories (default 0.35).
#' @param spaghetti_color Color of individual trajectories (default "grey45").
#' @param show_mean Overlay bold group mean trajectory (default TRUE).
#' @param mean_color Color of group summary line when unstratified (default "#005EB8").
#' @param mean_width Line width of group summary line (default 1.2).
#' @param show_ci Add standard error or 95\% CI ribbon around mean (default FALSE when errorbar is shown).
#' @param show_errorbar Display vertical error bars with crossbar caps (default TRUE).
#' @param errorbar Type of error bar to compute: \code{"se"} (standard error of the mean),
#'   \code{"ci"} (Student's t / 95\% CI), \code{"sd"} (standard deviation), or \code{"none"}.
#' @param errorbar_width Width of the horizontal error bar cap (default 0.14).
#' @param errorbar_lwd Line width of the error bar stroke (default 0.85).
#' @param point_shape Shape of the mean glyph (default 21, filled circle with colored border).
#' @param point_size Size of the mean glyph (default 3.2).
#' @param point_stroke Stroke width of the mean glyph border (default 1.1).
#' @param point_fill Interior fill of the mean glyph (\code{"white"} for crisp journal look, or \code{"color"}).
#' @param dodge Horizontal dodging distance for stratified comparison arms (default 0.15).
#' @param show_n Display visit sample sizes ($n = \dots$) above plot (default TRUE).
#' @param xlab Axis label for time/visit (default "Study Visit / Time").
#' @param ylab Axis label for clinical score (default "Clinical Severity Score").
#'
#' @return A cliplot_type object for use with \code{\link{cliplot}}.
#'
#' @examples
#' \dontrun{
#' set.seed(42)
#' n_pts = 30
#' visits = c("BL", "M3", "M6", "M12")
#' traj_df = expand.grid(Patient = paste0("Pt_", 1:n_pts), Visit = visits)
#' traj_df$Score = pmax(0, pmin(80, rnorm(nrow(traj_df), mean = 40, sd = 15)))
#'
#' cliplot(Score ~ Visit | Patient, data = traj_df, type = "trajectory")
#' }
#'
#' @export
type_trajectory = function(
    patient_id       = NULL,
    time             = NULL,
    score            = NULL,
    group            = NULL,
    severity_bands   = list(Mild = c(0, 25), Moderate = c(25, 50), Severe = c(50, 100)),
    severity_colors  = c(Mild = "#D9EEF7", Moderate = "#FDE8D7", Severe = "#FADBD8"),
    spaghetti_alpha  = 0.18,
    spaghetti_width  = 0.35,
    spaghetti_color  = "grey45",
    show_mean        = TRUE,
    mean_color       = "#005EB8",
    mean_width       = 1.2,
    show_ci          = FALSE,
    show_errorbar    = TRUE,
    errorbar         = c("se", "ci", "sd", "none"),
    errorbar_width   = 0.14,
    errorbar_lwd     = 0.85,
    point_shape      = 21,
    point_size       = 3.2,
    point_stroke     = 1.1,
    point_fill       = "white",
    dodge            = 0.15,
    show_n           = TRUE,
    xlab             = "Study Visit / Time",
    ylab             = "Clinical Severity Score"
) {
  errorbar = match.arg(errorbar)

  cliplot_type(
    data = function(settings, ...) {
      df = settings$data
      nms = if (!is.null(df)) names(df) else character(0)
      group_val = NULL

      if (is.null(df)) {
        df = data.frame(
          time       = settings$x,
          score      = as.numeric(settings$y),
          patient_id = settings$by %||% paste0("Pt_", seq_along(settings$y)),
          stringsAsFactors = FALSE
        )
      } else {
        df$score = as.numeric(settings$y %||% (if (!is.null(score) && score %in% nms) df[[score]] else df[[1]]))
        df$time  = settings$x %||% (if (!is.null(time) && time %in% nms) df[[time]] else df[[2]])

        # Detect group vs patient_id
        if (!is.null(group) && group %in% nms) {
          group_val = as.factor(df[[group]])
          df$patient_id = if (!is.null(patient_id) && patient_id %in% nms) df[[patient_id]]
                          else if ("patient" %in% tolower(nms)) df[[nms[tolower(nms) == "patient"][1]]]
                          else if ("subject" %in% tolower(nms)) df[[nms[tolower(nms) == "subject"][1]]]
                          else if ("donor" %in% tolower(nms)) df[[nms[tolower(nms) == "donor"][1]]]
                          else if ("id" %in% tolower(nms)) df[[nms[tolower(nms) == "id"][1]]]
                          else paste0("Pt_", seq_len(nrow(df)))
        } else if (!is.null(settings$by)) {
          by_vec = settings$by
          u_by = length(unique(by_vec))
          pt_col = if (!is.null(patient_id) && patient_id %in% nms) patient_id
                   else if ("patient" %in% tolower(nms)) nms[tolower(nms) == "patient"][1]
                   else if ("subject" %in% tolower(nms)) nms[tolower(nms) == "subject"][1]
                   else if ("donor" %in% tolower(nms)) nms[tolower(nms) == "donor"][1]
                   else if ("id" %in% tolower(nms)) nms[tolower(nms) == "id"][1]
                   else NULL
          if (!is.null(pt_col) && pt_col %in% nms && u_by <= 12) {
            group_val = as.factor(by_vec)
            df$patient_id = df[[pt_col]]
          } else {
            df$patient_id = by_vec
          }
        } else {
          df$patient_id = if (!is.null(patient_id) && patient_id %in% nms) df[[patient_id]]
                          else if ("patient" %in% tolower(nms)) df[[nms[tolower(nms) == "patient"][1]]]
                          else if ("subject" %in% tolower(nms)) df[[nms[tolower(nms) == "subject"][1]]]
                          else if ("donor" %in% tolower(nms)) df[[nms[tolower(nms) == "donor"][1]]]
                          else if ("id" %in% tolower(nms)) df[[nms[tolower(nms) == "id"][1]]]
                          else paste0("Pt_", seq_len(nrow(df)))
        }
      }

      df$patient_id = as.character(df$patient_id)
      if (!is.null(group_val)) df$group = group_val

      # Convert time to ordered factor if discrete
      if (is.character(df$time) || is.factor(df$time)) {
        levs = if (is.factor(df$time)) levels(df$time) else unique(df$time)
        df$time_fac = factor(df$time, levels = levs)
        df$time_num = as.numeric(df$time_fac)
      } else {
        df$time_num = as.numeric(df$time)
        df$time_fac = factor(df$time_num)
      }

      # Compute visit summary statistics (mean, se, sd, 95% ci, n)
      has_group = !is.null(df$group)
      split_key = if (has_group) interaction(df$time_fac, df$group, drop = TRUE) else df$time_fac

      time_splits = split(df, split_key, drop = TRUE)
      summary_rows = lapply(time_splits, function(chunk) {
        vals = stats::na.omit(chunk$score)
        n = length(vals)
        m = if (n > 0) mean(vals) else NA_real_
        s = if (n > 1) stats::sd(vals) / sqrt(n) else 0
        sd_val = if (n > 1) stats::sd(vals) else 0
        ci_lo = m - 1.96 * s
        ci_hi = m + 1.96 * s
        err_lo = switch(errorbar,
          se   = m - s,
          ci   = ci_lo,
          sd   = m - sd_val,
          none = m
        )
        err_hi = switch(errorbar,
          se   = m + s,
          ci   = ci_hi,
          sd   = m + sd_val,
          none = m
        )
        row_res = data.frame(
          time_fac = chunk$time_fac[1],
          time_num = chunk$time_num[1],
          mean     = m,
          se       = s,
          sd       = sd_val,
          ci_lo    = ci_lo,
          ci_hi    = ci_hi,
          err_lo   = err_lo,
          err_hi   = err_hi,
          n        = n,
          stringsAsFactors = FALSE
        )
        if (has_group) row_res$group = chunk$group[1]
        row_res
      })
      sum_df = do.call(rbind, summary_rows)
      if (has_group) {
        sum_df$group = factor(sum_df$group, levels = levels(df$group))
        sum_df = sum_df[order(sum_df$group, sum_df$time_num), ]
      }

      settings$traj_data        = df
      settings$traj_summary     = sum_df
      settings$has_group        = has_group
      settings$severity_bands   = severity_bands
      settings$severity_colors  = severity_colors
      settings$spaghetti_alpha  = spaghetti_alpha
      settings$spaghetti_width  = spaghetti_width
      settings$spaghetti_color  = spaghetti_color
      settings$show_mean        = show_mean
      settings$mean_color       = mean_color
      settings$mean_width       = mean_width
      settings$show_ci          = show_ci
      settings$show_errorbar    = show_errorbar
      settings$errorbar         = errorbar
      settings$errorbar_width   = errorbar_width
      settings$errorbar_lwd     = errorbar_lwd
      settings$point_shape      = point_shape
      settings$point_size       = point_size
      settings$point_stroke     = point_stroke
      settings$point_fill       = point_fill
      settings$dodge            = dodge
      settings$show_n           = show_n
      settings$xlab             = xlab
      settings$ylab             = ylab
    },
    draw = function(data, mapping, settings, ...) {
      df        = settings$traj_data
      sum_df    = settings$traj_summary
      has_group = isTRUE(settings$has_group)
      if (is.null(df)) return(ggplot2::ggplot())

      p = ggplot2::ggplot()
      pos = if (has_group && settings$dodge > 0) ggplot2::position_dodge(width = settings$dodge) else "identity"

      # Shaded Background Severity Bands
      if (!is.null(settings$severity_bands) && length(settings$severity_bands) > 0) {
        bands = settings$severity_bands
        b_cols = settings$severity_colors
        band_pieces = list()
        for (b_name in names(bands)) {
          rng = bands[[b_name]]
          col = if (b_name %in% names(b_cols)) b_cols[[b_name]] else "#F0F0F0"
          band_pieces[[b_name]] = data.frame(
            ymin = rng[1], ymax = rng[2],
            band = b_name, col = col,
            stringsAsFactors = FALSE
          )
        }
        band_df = do.call(rbind, band_pieces)

        p = p +
          ggplot2::geom_rect(
            data = band_df,
            ggplot2::aes(
              ymin = .data[["ymin"]],
              ymax = .data[["ymax"]],
              xmin = -Inf,
              xmax = Inf
            ),
            fill = band_df$col,
            alpha = 0.45,
            inherit.aes = FALSE
          ) +
          ggplot2::geom_text(
            data = band_df,
            ggplot2::aes(
              x = Inf,
              y = (.data[["ymin"]] + .data[["ymax"]]) / 2,
              label = .data[["band"]]
            ),
            hjust = 1.1, size = 2.8, fontface = "italic", color = "grey40",
            inherit.aes = FALSE
          )
      }

      # Semi-transparent Individual Patient Spaghetti Paths
      if (settings$spaghetti_alpha > 0) {
        if (has_group) {
          p = p + ggplot2::geom_line(
            data = df,
            ggplot2::aes(
              x = .data[["time_num"]],
              y = .data[["score"]],
              group = .data[["patient_id"]],
              color = .data[["group"]]
            ),
            alpha = settings$spaghetti_alpha,
            linewidth = settings$spaghetti_width,
            inherit.aes = FALSE
          )
        } else {
          p = p + ggplot2::geom_line(
            data = df,
            ggplot2::aes(
              x = .data[["time_num"]],
              y = .data[["score"]],
              group = .data[["patient_id"]]
            ),
            alpha = settings$spaghetti_alpha,
            linewidth = settings$spaghetti_width,
            color = settings$spaghetti_color,
            inherit.aes = FALSE
          )
        }
      }

      # Confidence Interval Ribbon
      if (isTRUE(settings$show_ci) && !is.null(sum_df) && nrow(sum_df) > 1) {
        if (has_group) {
          p = p + ggplot2::geom_ribbon(
            data = sum_df,
            ggplot2::aes(
              x = .data[["time_num"]],
              ymin = .data[["ci_lo"]],
              ymax = .data[["ci_hi"]],
              fill = .data[["group"]],
              group = .data[["group"]]
            ),
            alpha = 0.16,
            inherit.aes = FALSE
          )
        } else {
          p = p + ggplot2::geom_ribbon(
            data = sum_df,
            ggplot2::aes(
              x = .data[["time_num"]],
              ymin = .data[["ci_lo"]],
              ymax = .data[["ci_hi"]]
            ),
            fill = settings$mean_color,
            alpha = 0.22,
            inherit.aes = FALSE
          )
        }
      }

      # Error bars
      if (isTRUE(settings$show_errorbar) && !identical(settings$errorbar, "none") && !is.null(sum_df)) {
        if (has_group) {
          p = p + ggplot2::geom_errorbar(
            data = sum_df,
            ggplot2::aes(
              x = .data[["time_num"]],
              ymin = .data[["err_lo"]],
              ymax = .data[["err_hi"]],
              color = .data[["group"]],
              group = .data[["group"]]
            ),
            width = settings$errorbar_width,
            linewidth = settings$errorbar_lwd,
            position = pos,
            inherit.aes = FALSE
          )
        } else {
          p = p + ggplot2::geom_errorbar(
            data = sum_df,
            ggplot2::aes(
              x = .data[["time_num"]],
              ymin = .data[["err_lo"]],
              ymax = .data[["err_hi"]]
            ),
            color = settings$mean_color,
            width = settings$errorbar_width,
            linewidth = settings$errorbar_lwd,
            inherit.aes = FALSE
          )
        }
      }

      # Group Mean Bold Trajectory Line
      if (isTRUE(settings$show_mean) && !is.null(sum_df)) {
        if (has_group) {
          p = p + ggplot2::geom_line(
            data = sum_df,
            ggplot2::aes(
              x = .data[["time_num"]],
              y = .data[["mean"]],
              color = .data[["group"]],
              group = .data[["group"]]
            ),
            linewidth = settings$mean_width,
            position = pos,
            inherit.aes = FALSE
          )
        } else {
          p = p + ggplot2::geom_line(
            data = sum_df,
            ggplot2::aes(x = .data[["time_num"]], y = .data[["mean"]]),
            color = settings$mean_color,
            linewidth = settings$mean_width,
            inherit.aes = FALSE
          )
        }
      }

      # Mean Points
      if (isTRUE(settings$show_mean) && !is.null(sum_df)) {
        pt_fill_white = identical(settings$point_fill, "white")
        if (has_group) {
          if (pt_fill_white) {
            p = p + ggplot2::geom_point(
              data = sum_df,
              ggplot2::aes(
                x = .data[["time_num"]],
                y = .data[["mean"]],
                color = .data[["group"]],
                group = .data[["group"]]
              ),
              shape = settings$point_shape,
              size = settings$point_size,
              stroke = settings$point_stroke,
              fill = "white",
              position = pos,
              inherit.aes = FALSE
            )
          } else {
            p = p + ggplot2::geom_point(
              data = sum_df,
              ggplot2::aes(
                x = .data[["time_num"]],
                y = .data[["mean"]],
                color = .data[["group"]],
                fill = .data[["group"]],
                group = .data[["group"]]
              ),
              shape = settings$point_shape,
              size = settings$point_size,
              stroke = settings$point_stroke,
              position = pos,
              inherit.aes = FALSE
            )
          }
        } else {
          if (pt_fill_white) {
            p = p + ggplot2::geom_point(
              data = sum_df,
              ggplot2::aes(x = .data[["time_num"]], y = .data[["mean"]]),
              color = settings$mean_color,
              shape = settings$point_shape,
              size = settings$point_size,
              stroke = settings$point_stroke,
              fill = "white",
              inherit.aes = FALSE
            )
          } else {
            p = p + ggplot2::geom_point(
              data = sum_df,
              ggplot2::aes(x = .data[["time_num"]], y = .data[["mean"]]),
              color = settings$mean_color,
              fill = settings$mean_color,
              shape = settings$point_shape,
              size = settings$point_size,
              stroke = settings$point_stroke,
              inherit.aes = FALSE
            )
          }
        }
      }

      # Visit Sample Size Text Annotations at top
      if (isTRUE(settings$show_n) && !is.null(sum_df)) {
        max_y = max(c(df$score, unlist(settings$severity_bands)), na.rm = TRUE)
        if (!has_group) {
          n_labels = paste0("n=", sum_df$n)
          p = p + ggplot2::annotate(
            "text",
            x = sum_df$time_num,
            y = max_y * 1.04,
            label = n_labels,
            size = 3.0,
            fontface = "bold",
            color = "#555555"
          )
        }
      }

      # X-axis formatting matching discrete visits
      unique_times = sum_df[!duplicated(sum_df$time_num), ]
      unique_times = unique_times[order(unique_times$time_num), ]
      p = p +
        ggplot2::scale_x_continuous(
          breaks = unique_times$time_num,
          labels = unique_times$time_fac,
          expand = ggplot2::expansion(mult = c(0.06, 0.08))
        ) +
        ggplot2::labs(x = settings$xlab, y = settings$ylab) +
        ggplot2::theme(
          panel.grid.major.x = ggplot2::element_blank(),
          panel.grid.minor   = ggplot2::element_blank()
        )

      p
    },
    name = "trajectory"
  )
}

