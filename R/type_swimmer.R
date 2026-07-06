# ===========================================================================
# cliomicplot: Swimmer Plot Type
# For patient timelines and treatment course visualization
# ===========================================================================

#' Swimmer Plot Type
#'
#' @description Creates publication-ready swimmer plots for visualizing
#'   individual patient timelines, including treatment duration, response,
#'   and key clinical events.
#'
#' @param id Column name for patient IDs
#' @param time_start Column name for start times (default created from data)
#' @param time_end Column name for end/duration times
#' @param bar_fill Column name for bar coloring (e.g., response category)
#' @param event_times Optional data frame of event markers with columns:
#'   \code{id}, \code{time}, \code{event} (event type)
#' @param bar_alpha Bar transparency (default 0.9)
#' @param sort_by Column to sort patients by (default "time_end")
#' @param event_shape_map Named vector mapping event types to pch values
#' @param event_color_map Named vector mapping event types to colors
#' @param bar_linewidth Timeline bar thickness.
#' @param bar_palette Palette used for response/treatment bars.
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
#'   Arm      = sample(c("Treatment","Control"), 20, replace=TRUE)
#' )
#'
#' # Event markers
#' events = data.frame(
#'   ID    = rep(paste0("Pt", 1:20), each = 2),
#'   Time  = round(runif(40, 0, 24), 1),
#'   Event = sample(c("Progression","AE","Dose Reduction"), 40, replace=TRUE)
#' )
#'
#' cliplot(Duration ~ ID, data = pt_data, type = "swimmer")
#' }
#'
#' @export
type_swimmer = function(
    id              = NULL,
    time_start      = NULL,
    time_end        = NULL,
    bar_fill        = NULL,
    event_times     = NULL,
    bar_alpha       = 0.9,
    sort_by         = "time_end",
    event_shape_map = NULL,
    event_color_map = NULL,
    bar_linewidth   = 8,
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
        if (!is.null(bar_fill)) {
          df$fill_var = "Treatment"
        } else {
          df$fill_var = "Treatment"
        }
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

      # Sort patients
      if (sort_by == "time_end") {
        df = df[order(df$duration), ]
      }
      df$id = factor(df$id, levels = df$id)

      # Default event shape/color maps
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
          "Progression"    = "#E64B35",
          "Death"          = "#000000",
          "AE"             = "#E18727",
          "Dose Reduction" = "#7E6148",
          "Surgery"        = "#3C5488",
          "Response"       = "#20854E"
        )
      }

      settings$swimmer_data     = df
      settings$event_times      = event_times
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
        ggplot2::labs(x = "Time (months)", y = "", color = "") +
        ggplot2::scale_color_manual(values = bar_colors) +
        ggplot2::theme(
          panel.grid.major.y = ggplot2::element_blank(),
          panel.grid.minor.y = ggplot2::element_blank(),
          axis.text.y = ggplot2::element_text(face = "bold")
        )

      # Add event markers if provided
      if (!is.null(settings$event_times)) {
        events = settings$event_times
        names(events)[tolower(names(events)) == "id"] = "ID"
        names(events)[tolower(names(events)) == "time"] = "Time"
        names(events)[tolower(names(events)) == "event"] = "Event"
        events = events[events$ID %in% as.character(df$id), , drop = FALSE]
        events$ID = factor(events$ID, levels = levels(df$id))
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
          values = settings$event_shape_map,
          name   = "Event"
        ) +
        ggplot2::scale_fill_manual(
          values = settings$event_color_map,
          name   = "Event"
        )
      }

      p
    },
    name = "swimmer"
  )
}
