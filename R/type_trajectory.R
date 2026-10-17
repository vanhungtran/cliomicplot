# ===========================================================================
# cliomicplot: Longitudinal Trajectory with Clinical Severity Bands
# Clinical Figure Archetype 4 (clinical and cohort figure standards)
# ===========================================================================

#' Longitudinal Clinical Trajectory Plot Type
#'
#' @description Creates publication-ready longitudinal clinical trajectories
#'   (Clinical Archetype 4). Overlays semi-transparent individual patient
#'   spaghetti trajectories onto shaded background clinical severity bands
#'   (e.g., Mild, Moderate, Severe), anchored by a bold group mean trajectory
#'   with standard error or 95\% CI ribbons and visit sample-size annotations.
#'
#' @param patient_id Column name for individual patient/participant IDs.
#' @param time Column name for longitudinal visit/time values.
#' @param score Column name for clinical score/measurement.
#' @param severity_bands Named list defining intervals for background severity
#'   shading (default \code{list(Mild = c(0, 25), Moderate = c(25, 50), Severe = c(50, 100))}).
#' @param severity_colors Named vector of fill colors for severity bands
#'   (default \code{c(Mild = "#D9EEF7", Moderate = "#FDE8D7", Severe = "#FADBD8")}).
#' @param spaghetti_alpha Opacity of individual patient trajectories (default 0.18).
#' @param spaghetti_width Line width of individual trajectories (default 0.35).
#' @param spaghetti_color Color of individual trajectories (default "grey45").
#' @param show_mean Overlay bold group mean trajectory (default TRUE).
#' @param mean_color Color of group summary line (default "#005EB8").
#' @param mean_width Line width of group summary line (default 1.2).
#' @param show_ci Add standard error or 95\% CI ribbon around mean (default TRUE).
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
    severity_bands   = list(Mild = c(0, 25), Moderate = c(25, 50), Severe = c(50, 100)),
    severity_colors  = c(Mild = "#D9EEF7", Moderate = "#FDE8D7", Severe = "#FADBD8"),
    spaghetti_alpha  = 0.18,
    spaghetti_width  = 0.35,
    spaghetti_color  = "grey45",
    show_mean        = TRUE,
    mean_color       = "#005EB8",
    mean_width       = 1.2,
    show_ci          = TRUE,
    show_n           = TRUE,
    xlab             = "Study Visit / Time",
    ylab             = "Clinical Severity Score"
) {
  cliplot_type(
    data = function(settings, ...) {
      df = settings$data
      if (is.null(df)) {
        df = data.frame(
          time       = settings$x,
          score      = as.numeric(settings$y),
          patient_id = settings$by %||% paste0("Pt_", seq_along(settings$y)),
          stringsAsFactors = FALSE
        )
      } else {
        nms = names(df)
        df$score = as.numeric(settings$y %||% (if (!is.null(score) && score %in% nms) df[[score]] else df[[1]]))
        df$time  = settings$x %||% (if (!is.null(time) && time %in% nms) df[[time]] else df[[2]])
        df$patient_id = settings$by %||% (if (!is.null(patient_id) && patient_id %in% nms) df[[patient_id]]
                                          else if ("patient" %in% tolower(nms)) df[[nms[tolower(nms) == "patient"][1]]]
                                          else if ("id" %in% tolower(nms)) df[[nms[tolower(nms) == "id"][1]]]
                                          else paste0("Pt_", seq_len(nrow(df))))
      }

      df$patient_id = as.character(df$patient_id)

      # Convert time to ordered factor if discrete
      if (is.character(df$time) || is.factor(df$time)) {
        levs = if (is.factor(df$time)) levels(df$time) else unique(df$time)
        df$time_fac = factor(df$time, levels = levs)
        df$time_num = as.numeric(df$time_fac)
      } else {
        df$time_num = as.numeric(df$time)
        df$time_fac = factor(df$time_num)
      }

      # Compute visit summary statistics (mean, se, 95% ci, n)
      time_splits = split(df, df$time_fac, drop = TRUE)
      summary_rows = lapply(time_splits, function(chunk) {
        vals = stats::na.omit(chunk$score)
        n = length(vals)
        m = mean(vals)
        s = if (n > 1) stats::sd(vals) / sqrt(n) else 0
        data.frame(
          time_fac = chunk$time_fac[1],
          time_num = chunk$time_num[1],
          mean     = m,
          se       = s,
          ci_lo    = m - 1.96 * s,
          ci_hi    = m + 1.96 * s,
          n        = n,
          stringsAsFactors = FALSE
        )
      })
      sum_df = do.call(rbind, summary_rows)

      settings$traj_data        = df
      settings$traj_summary     = sum_df
      settings$severity_bands   = severity_bands
      settings$severity_colors  = severity_colors
      settings$spaghetti_alpha  = spaghetti_alpha
      settings$spaghetti_width  = spaghetti_width
      settings$spaghetti_color  = spaghetti_color
      settings$show_mean        = show_mean
      settings$mean_color       = mean_color
      settings$mean_width       = mean_width
      settings$show_ci          = show_ci
      settings$show_n           = show_n
      settings$xlab             = xlab
      settings$ylab             = ylab
    },
    draw = function(data, mapping, settings, ...) {
      df     = settings$traj_data
      sum_df = settings$traj_summary
      if (is.null(df)) return(ggplot2::ggplot())

      p = ggplot2::ggplot()

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

      # 95% Confidence Interval Ribbon
      if (isTRUE(settings$show_ci) && !is.null(sum_df) && nrow(sum_df) > 1) {
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

      # Group Mean Bold Trajectory
      if (isTRUE(settings$show_mean) && !is.null(sum_df)) {
        p = p +
          ggplot2::geom_line(
            data = sum_df,
            ggplot2::aes(x = .data[["time_num"]], y = .data[["mean"]]),
            color = settings$mean_color,
            linewidth = settings$mean_width,
            inherit.aes = FALSE
          ) +
          ggplot2::geom_point(
            data = sum_df,
            ggplot2::aes(x = .data[["time_num"]], y = .data[["mean"]]),
            color = settings$mean_color,
            size = 3.0,
            inherit.aes = FALSE
          )
      }

      # Visit Sample Size Text Annotations at top
      if (isTRUE(settings$show_n) && !is.null(sum_df)) {
        max_y = max(c(df$score, unlist(settings$severity_bands)), na.rm = TRUE)
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

      # X-axis formatting matching discrete visits
      unique_times = sum_df[order(sum_df$time_num), ]
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
