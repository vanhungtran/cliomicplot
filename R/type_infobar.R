# ===========================================================================
# cliomicplot: Infographic Bar Chart
# ===========================================================================

#' Infographic Bar Chart
#'
#' @description
#' Creates a publication or media-ready bar chart with per-bar custom colours,
#' optional footer boxes with labels, reference lines, and value annotations
#' above each bar. Designed for polling, survey, and any comparison where each
#' category carries its own brand or semantic colour.
#'
#' @param bar_colors Named character vector mapping category labels to bar fill
#'   colours. If \code{NULL}, defaults to the active qualitative palette.
#' @param box_colors Named character vector mapping category labels to footer
#'   box fill colours. If \code{NULL}, no footer boxes are drawn.
#' @param box_labels Character vector of labels to draw inside each footer box,
#'   in the same order as the categories on the x-axis (default: the category
#'   names themselves). Pass \code{NULL} to suppress box text.
#' @param box_sub_labels Character vector of secondary labels to draw below
#'   each footer box (default \code{NULL}).
#' @param reference_line Numeric y-axis value at which to draw a horizontal
#'   reference line (default \code{NULL} = no line).
#' @param reference_label Optional label to place next to the reference line.
#' @param value_labels Logical. Whether to draw the bar value as a label above
#'   each bar (default \code{TRUE}).
#' @param value_suffix A string appended to each value label, e.g. \code{"\%"}
#'   (default \code{""}).
#' @param change_labels Character vector of change annotations to draw below
#'   the value labels, in category order (default \code{NULL}).
#' @param bar_width Width of bars as a fraction of the category spacing
#'   (default 0.85).
#' @param label_offset Distance above each bar for value/change labels,
#'   expressed as a fraction of the maximum bar value (default 0.06).
#' @param font_family Base font family for all text (default \code{"sans"}).
#' @param bg_color Background colour for the plot (default \code{"white"}).
#'
#' @return A \code{cliplot_type} object for use with \code{\link{cliplot}}.
#'
#' @section Formula convention:
#' Call as \code{cliplot(value ~ category, data = df, type = "infobar",
#' bar_colors = c(CatA = "#FF0000", CatB = "#00FF00"))}. Colours and labels
#' are matched to categories by name (for named vectors) or by position
#' (for unnamed vectors).
#'
#' @examples
#' \dontrun{
#' polls <- data.frame(party = c("CDU", "SPD", "AfD"), poll = c(17, 13, 18))
#' cliplot(poll ~ party, data = polls, type = "infobar",
#'         bar_colors = c(CDU = "#55B1BE", SPD = "#EC0016", AfD = "#119CD2"),
#'         value_suffix = "%", reference_line = 5)
#' }
#'
#' @export
type_infobar = function(
    bar_colors      = NULL,
    box_colors      = NULL,
    box_labels      = NULL,
    box_sub_labels  = NULL,
    reference_line  = NULL,
    reference_label = NULL,
    value_labels    = TRUE,
    value_suffix    = "",
    change_labels   = NULL,
    bar_width       = 0.85,
    label_offset    = 0.06,
    font_family     = "sans",
    bg_color        = "white"
) {
  cliplot_type(
    data = function(settings, ...) {
      category <- settings$x
      value    <- settings$y

      if (is.null(category) || is.null(value)) {
        stop("type_infobar requires both a category variable (x) and a ",
             "value variable (y). Use: cliplot(value ~ category, data=df, ",
             "type='infobar')", call. = FALSE)
      }

      df <- data.frame(
        category = as.character(category),
        value    = as.numeric(value),
        stringsAsFactors = FALSE
      )

      # Preserve input category order
      seen <- character(0)
      cats <- character(0)
      for (cc in df$category) {
        if (!cc %in% seen) {
          seen <- c(seen, cc); cats <- c(cats, cc)
        }
      }
      df <- df[!duplicated(df$category), ]
      df$category <- factor(df$category, levels = cats)
      df <- df[order(df$category), ]
      n <- nrow(df)

      # ---- Bar colours ----
      if (is.null(bar_colors)) {
        bar_colors <- stats::setNames(
          get_cli_palette(settings$palette %||% "jco", n),
          as.character(df$category)
        )
      }
      if (!is.null(names(bar_colors))) {
        df$bar_color <- unname(bar_colors[as.character(df$category)])
      } else if (length(bar_colors) == n) {
        df$bar_color <- unname(bar_colors)
      } else {
        stop("bar_colors must be a named vector or have one entry per category",
             call. = FALSE)
      }
      # Fill NAs from failed name lookup with a fallback grey
      df$bar_color[is.na(df$bar_color)] <- "#888888"

      # ---- Box colours ----
      if (!is.null(box_colors)) {
        if (!is.null(names(box_colors))) {
          df$box_color <- unname(box_colors[as.character(df$category)])
        } else if (length(box_colors) == n) {
          df$box_color <- unname(box_colors)
        } else {
          stop("box_colors must be a named vector or have one entry per category",
               call. = FALSE)
        }
        df$box_color[is.na(df$box_color)] <- "#888888"
      }

      # ---- Labels (by position in category order) ----
      resolve_vec <- function(v, fallback) {
        if (is.null(v)) return(fallback)
        if (length(v) == nrow(df)) return(as.character(v))
        stop(sprintf("Label vector must have length %d (one per category)",
                     nrow(df)), call. = FALSE)
      }

      df$box_lbl     <- resolve_vec(box_labels,     as.character(df$category))
      df$box_sub_lbl <- resolve_vec(box_sub_labels,  rep("", n))
      df$change_lbl  <- resolve_vec(change_labels,   rep("", n))

      # ---- Positional helpers ----
      max_val <- max(df$value, na.rm = TRUE)
      df$x <- as.numeric(df$category)

      # Stamp into settings
      settings$ifo_df              <- df
      settings$ifo_has_boxes       <- !is.null(box_colors)
      settings$ifo_reference_line  <- reference_line
      settings$ifo_reference_label <- reference_label
      settings$ifo_value_labels    <- value_labels
      settings$ifo_value_suffix    <- value_suffix
      settings$ifo_bar_width       <- bar_width
      settings$ifo_label_offset    <- label_offset * max(max_val, 1)
      settings$ifo_max_val         <- max_val
      settings$ifo_font_family     <- font_family
      settings$ifo_bg_color        <- bg_color
    },

    draw = function(data, mapping, settings, ...) {
      df <- settings$ifo_df
      if (is.null(df)) return(ggplot2::ggplot())

      bar_w     <- settings$ifo_bar_width
      ref_line  <- settings$ifo_reference_line
      ref_lbl   <- settings$ifo_reference_label
      lo        <- settings$ifo_label_offset
      max_val   <- settings$ifo_max_val
      ff        <- settings$ifo_font_family
      bg        <- settings$ifo_bg_color
      has_boxes <- isTRUE(settings$ifo_has_boxes)

      p <- ggplot2::ggplot(df)

      # ---- Background ----
      p <- p + ggplot2::annotate(
        "rect", xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf,
        fill = bg
      )

      # ---- Reference line ----
      if (!is.null(ref_line)) {
        p <- p +
          ggplot2::geom_hline(
            yintercept = ref_line, linewidth = 0.5, colour = "#9EAA98"
          )
        if (!is.null(ref_lbl)) {
          p <- p +
            ggplot2::annotate(
              "text",
              x      = min(df$x) - 0.52,
              y      = ref_line * 1.04,
              label  = ref_lbl,
              hjust  = 1,
              size   = 3.5,
              colour = "#52605A",
              family = ff
            )
        }
      }

      # ---- Main bars ----
      p <- p +
        ggplot2::geom_col(
          ggplot2::aes(x = .data[["x"]], y = .data[["value"]]),
          fill  = df$bar_color,
          width = bar_w
        )

      # ---- Footer boxes ----
      if (has_boxes) {
        box_bot <- -max_val * 0.12
        box_top <- -max_val * 0.02
        p <- p +
          ggplot2::geom_rect(
            ggplot2::aes(
              xmin = .data[["x"]] - bar_w / 2,
              xmax = .data[["x"]] + bar_w / 2,
              ymin = box_bot, ymax = box_top
            ),
            fill = df$box_color
          ) +
          ggplot2::geom_text(
            ggplot2::aes(
              x = .data[["x"]],
              y = (box_bot + box_top) / 2,
              label = .data[["box_lbl"]]
            ),
            colour   = "white",
            fontface = "bold",
            size     = 4.5,
            family   = ff
          )

        # Sub-labels below boxes
        if (any(nzchar(df$box_sub_lbl))) {
          p <- p +
            ggplot2::geom_text(
              ggplot2::aes(
                x     = .data[["x"]],
                y     = box_bot - max_val * 0.08,
                label = .data[["box_sub_lbl"]]
              ),
              size   = 3.2,
              colour = "#52605A",
              family = ff
            )
        }
      }

      # ---- Value labels above bars ----
      if (isTRUE(settings$ifo_value_labels)) {
        suffix <- settings$ifo_value_suffix
        p <- p +
          ggplot2::geom_text(
            ggplot2::aes(
              x     = .data[["x"]],
              y     = .data[["value"]] + lo,
              label = paste0(.data[["value"]], suffix)
            ),
            fontface = "bold",
            size     = 5,
            colour   = "black",
            family   = ff
          )
      }

      # ---- Change labels ----
      if (any(nzchar(df$change_lbl))) {
        p <- p +
          ggplot2::geom_text(
            ggplot2::aes(
              x     = .data[["x"]],
              y     = .data[["value"]] + lo * 0.35,
              label = .data[["change_lbl"]]
            ),
            size   = 3.8,
            colour = "black",
            family = ff
          )
      }

      # ---- Axis & theme ----
      y_floor <- if (has_boxes) -max_val * 0.30 else -max_val * 0.01
      y_ceil  <- max_val * 1.22

      p <- p +
        ggplot2::scale_x_continuous(
          limits = c(min(df$x) - 0.6, max(df$x) + 0.6),
          breaks = NULL, expand = c(0, 0)
        ) +
        ggplot2::scale_y_continuous(
          limits = c(y_floor, y_ceil),
          breaks = NULL, expand = c(0, 0)
        ) +
        ggplot2::coord_cartesian(clip = "off") +
        theme_cli_base() +
        ggplot2::theme(
          axis.title        = ggplot2::element_blank(),
          axis.text         = ggplot2::element_blank(),
          axis.ticks        = ggplot2::element_blank(),
          panel.grid        = ggplot2::element_blank(),
          panel.border      = ggplot2::element_blank(),
          panel.background  = ggplot2::element_rect(fill = NA, colour = NA),
          plot.background   = ggplot2::element_rect(fill = bg, colour = NA),
          strip.background  = ggplot2::element_blank(),
          legend.position   = "none"
        )

      p
    },

    name = "infobar"
  )
}
