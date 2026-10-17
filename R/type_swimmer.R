# ===========================================================================
# cliomicplot: Swimmer Plot Type
# For patient timelines and treatment course visualization
# Enhanced with CNS Journal Clinical Figure Standards (Clinical Archetype 13):
# - Ongoing therapy arrowheads at bar ends
# - RECIST clinical event milestones and color/shape contracts
# - Unified event legend and duration annotations
# ===========================================================================

#' Swimmer Plot Type
#'
#' @description Creates publication-ready swimmer plots for visualizing
#'   individual patient timelines, including treatment duration, response,
#'   key clinical events, and ongoing therapy status at database lock.
#'   Enhanced with Clinical Archetype 13 conventions.
#'
#' @param id Column name for patient IDs.
#' @param time_start Column name for start times (default created from data).
#' @param time_end Column name for end/duration times.
#' @param bar_fill Column name for bar coloring (e.g., response category).
#' @param event_times Optional data frame of event markers with columns:
#'   \code{id}, \code{time}, \code{event} (event type).
#' @param ongoing Optional column name or vector indicating whether patient
#'   treatment was ongoing at database lock. Ongoing patients receive an
#'   arrowhead indicator at their timeline bar tip (Archetype 13).
#' @param show_duration Display numeric duration at bar ends (default FALSE).
#' @param bar_alpha Bar transparency (default 0.9).
#' @param sort_by Column to sort patients by (default "time_end").
#' @param event_shape_map Named vector mapping event types to pch values.
#' @param event_color_map Named vector mapping event types to colors.
#' @param bar_linewidth Timeline bar thickness (default 7.5).
#' @param bar_palette Palette used for response/treatment bars (default "jama").
#'
#' @return A cliplot_type object for use with \code{\link{cliplot}}.
#'
#' @examples
#' \dontrun{
#' # Patient timeline data
#' pt_data = data.frame(
#'   ID       = paste0("Pt", 1:20),
#'   Duration = round(runif(20, 3, 24), 1),
#'   Response = sample(c("CR","PR","SD","PD"), 20, replace=TRUE),
#'   Ongoing  = sample(c(TRUE, FALSE), 20, replace=TRUE)
#' )
#'
#' # Event markers
#' events = data.frame(
#'   ID    = rep(paste0("Pt", 1:20), each = 2),
#'   Time  = round(runif(40, 0, 24), 1),
#'   Event = sample(c("Progression","AE","Dose Reduction"), 40, replace=TRUE)
#' )
#'
#' cliplot(Duration ~ ID, data = pt_data,
#'         type = type_swimmer(bar_fill = "Response", event_times = events, ongoing = "Ongoing"))
#' }
#'
#' @export
type_swimmer = function(
    id              = NULL,
    time_start      = NULL,
    time_end        = NULL,
    bar_fill        = NULL,
    event_times     = NULL,
    ongoing         = NULL,
    show_duration   = FALSE,
    bar_alpha       = 0.9,
    sort_by         = "time_end",
    event_shape_map = NULL,
    event_color_map = NULL,
    bar_linewidth   = 7.5,
    bar_palette     = "jama"
) {
  cliplot_type(
    data = function(settings, ...) {
      df = settings$data
      if (is.null(df)) {
        df = data.frame(
          id       = settings$x,
          duration = settings$y,
          stringsAsFactors = FALSE
        )
        df$start = 0
        df$fill_var = "Treatment"
      } else {
        df$id = if (!is.null(id) && id %in% names(df)) {
          df[[id]]
        } else {
          settings$x %||% df[[1]]
        }
        df$start = if (!is.null(time_start) && time_start %in% names(df)) {
          df[[time_start]]
        } else {
          settings$xmin %||% 0
        }
        df$duration = if (!is.null(time_end) && time_end %in% names(df)) {
          df[[time_end]]
        } else {
          settings$y %||% df[[2]]
        }
        if (!is.null(bar_fill) && bar_fill %in% names(df)) {
          df$fill_var = df[[bar_fill]]
        } else {
          df$fill_var = "Treatment"
        }
      }

      # Ongoing therapy detection
      df$ongoing = FALSE
      if (!is.null(ongoing)) {
        if (is.character(ongoing) && length(ongoing) == 1 && ongoing %in% names(df)) {
          df$ongoing = as.logical(df[[ongoing]])
        } else if (is.logical(ongoing) && length(ongoing) == nrow(df)) {
          df$ongoing = ongoing
        }
      } else if ("ongoing" %in% tolower(names(df))) {
        col_match = names(df)[tolower(names(df)) == "ongoing"][1]
        df$ongoing = as.logical(df[[col_match]])
      }

      # Sort patients
      if (identical(sort_by, "time_end")) {
        df = df[order(df$duration), ]
      }
      df$id = factor(df$id, levels = df$id)

      # Standard RECIST event shape/color maps (Clinical Archetype 13)
      if (is.null(event_shape_map)) {
        event_shape_map = c(
          "Progression"    = 21,
          "Death"          = 22,
          "AE"             = 23,
          "Dose Reduction" = 24,
          "Surgery"        = 25,
          "Response"       = 21
        )
      }
      if (is.null(event_color_map)) {
        event_color_map = c(
          "Progression"    = "#E64B35", # coral red
          "Death"          = "#000000", # black
          "AE"             = "#E18727", # amber
          "Dose Reduction" = "#7E6148", # brown
          "Surgery"        = "#3C5488", # slate blue
          "Response"       = "#20854E"  # emerald green
        )
      }

      settings$swimmer_data     = df
      settings$event_times      = event_times
      settings$show_duration    = show_duration
      settings$bar_alpha        = bar_alpha
      settings$bar_linewidth    = bar_linewidth
      settings$bar_palette      = bar_palette
      settings$event_shape_map  = event_shape_map
      settings$event_color_map  = event_color_map
    },
    draw = function(data, mapping, settings, ...) {
      df = settings$swimmer_data
      if (is.null(df)) return(ggplot2::ggplot())

      bar_levels = unique(as.character(df$fill_var))
      bar_colors = get_cli_palette(settings$bar_palette, length(bar_levels))
      names(bar_colors) = bar_levels

      max_dur = max(df$duration, na.rm = TRUE)

      p = ggplot2::ggplot(df) +
        ggplot2::geom_segment(
          ggplot2::aes(
            x = .data[["start"]],
            xend = .data[["duration"]],
            y = .data[["id"]],
            yend = .data[["id"]],
            color = .data[["fill_var"]]
          ),
          linewidth = settings$bar_linewidth,
          alpha = settings$bar_alpha,
          lineend = "round"
        ) +
        ggplot2::labs(x = "Time on Treatment (months)", y = "", color = "Response / Arm") +
        ggplot2::scale_color_manual(values = bar_colors) +
        ggplot2::theme(
          panel.grid.major.y = ggplot2::element_blank(),
          panel.grid.minor.y = ggplot2::element_blank(),
          axis.text.y = ggplot2::element_text(face = "bold", size = 8.5)
        )

      # Ongoing therapy arrows at bar tip (Archetype 13)
      df_ong = df[isTRUE(df$ongoing) | df$ongoing %in% c(TRUE, 1), , drop = FALSE]
      if (nrow(df_ong) > 0) {
        arrow_delta = max_dur * 0.035
        p = p + ggplot2::geom_segment(
          data = df_ong,
          ggplot2::aes(
            x = .data[["duration"]],
            xend = .data[["duration"]] + arrow_delta,
            y = .data[["id"]],
            yend = .data[["id"]],
            color = .data[["fill_var"]]
          ),
          linewidth = settings$bar_linewidth * 0.45,
          arrow = grid::arrow(length = ggplot2::unit(2.5, "mm"), type = "closed"),
          show.legend = FALSE
        )
      }

      # Optional duration labels at right margin
      if (isTRUE(settings$show_duration)) {
        p = p + ggplot2::geom_text(
          ggplot2::aes(
            x = .data[["duration"]] + max_dur * 0.05,
            y = .data[["id"]],
            label = sprintf("%.1f", .data[["duration"]])
          ),
          hjust = 0, size = 2.8, color = "grey30", fontface = "bold"
        ) +
        ggplot2::scale_x_continuous(expand = ggplot2::expansion(mult = c(0.02, 0.12)))
      }

      # Add event markers if provided
      if (!is.null(settings$event_times)) {
        events = settings$event_times
        names(events)[tolower(names(events)) == "id"] = "ID"
        names(events)[tolower(names(events)) == "time"] = "Time"
        names(events)[tolower(names(events)) == "event"] = "Event"
        events = events[events$ID %in% as.character(df$id), , drop = FALSE]
        events$ID = factor(events$ID, levels = levels(df$id))

        # Filter shapes & colors to present events
        ev_types = unique(as.character(events$Event))
        sh_map = settings$event_shape_map
        col_map = settings$event_color_map

        for (ev in ev_types) {
          if (!ev %in% names(sh_map)) sh_map[ev] = 21
          if (!ev %in% names(col_map)) col_map[ev] = "#7F7F7F"
        }
        sh_map = sh_map[ev_types]
        col_map = col_map[ev_types]
        events$Event = factor(events$Event, levels = ev_types)

        p = p + ggplot2::geom_point(
          data    = events,
          ggplot2::aes(
            x     = .data[["Time"]],
            y     = .data[["ID"]],
            shape = .data[["Event"]],
            fill  = .data[["Event"]]
          ),
          size = 3.4,
          color = "#20242A",
          stroke = 0.45,
          inherit.aes = FALSE
        ) +
        ggplot2::scale_shape_manual(
          values = sh_map,
          name   = "Clinical Event"
        ) +
        ggplot2::scale_fill_manual(
          values = col_map,
          name   = "Clinical Event"
        ) +
        ggplot2::guides(
          shape = ggplot2::guide_legend(
            override.aes = list(fill = unname(col_map), size = 3.2)
          )
        )
      }

      p
    },
    name = "swimmer"
  )
}
