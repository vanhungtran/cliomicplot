# ===========================================================================
# cliomicplot: Clinical Trials Pipeline Horizontal Stacked Bar Chart
# ===========================================================================

#' Clinical Trials Pipeline Stacked Bar Chart
#'
#' @description
#' Creates a horizontal stacked bar chart showing the pipeline of clinical
#' trials, projects, or any portfolio broken down by category segments within
#' each row. Segments are color-coded and labelled; total counts appear on the
#' right.
#'
#' @param segment_colors Named character vector mapping segment labels to fill
#'   colours. Defaults to the active qualitative palette.
#' @param segment_text_colors Named character vector mapping segment labels to
#'   label colours for text drawn inside segments. Defaults to dark-on-light
#'   heuristics.
#' @param bar_height Height of each bar as a fraction of the row spacing
#'   (default 0.72).
#' @param segment_border Colour for the border drawn around each segment
#'   rectangle (default \code{"#EEE9D4"}).
#' @param segment_border_width Line width for segment borders (default 0.5).
#' @param label_threshold Minimum segment width for which an internal label is
#'   drawn (default 3).
#' @param show_totals Logical. Whether to show a total-count label to the right
#'   of each row (default \code{TRUE}).
#' @param total_nudge Fraction of the maximum total used as padding for the
#'   x-axis limit when totals are shown (default 0.15).
#'
#' @return A \code{cliplot_type} object for use with \code{\link{cliplot}}.
#'
#' @section Formula convention:
#' Call as \code{cliplot(value ~ category, data = df, by = segment,
#' type = "trials")} — the value variable lands on the stacked x-axis,
#' categories form the rows, and the \code{by} variable defines the colour
#' segments within each bar.
#'
#' @examples
#' \dontrun{
#' trials <- data.frame(
#'   company = rep(c("AstraZeneca", "Roche", "Pfizer"), each = 5),
#'   phase   = rep(c("I", "I/II", "II", "II/III", "III"), 3),
#'   n       = c(4, 3, 24, 0, 32, 10, 8, 21, 0, 19, 9, 2, 10, 0, 6)
#' )
#'
#' cliplot(n ~ company, data = trials, by = phase, type = "trials",
#'         title = "Clinical Trial Pipeline by Phase")
#' }
#'
#' @export
type_trials = function(
    segment_colors      = NULL,
    segment_text_colors = NULL,
    bar_height          = 0.72,
    segment_border      = "#EEE9D4",
    segment_border_width = 0.5,
    label_threshold     = 3,
    show_totals         = TRUE,
    total_nudge         = 0.15
) {
  cliplot_type(
    data = function(settings, ...) {
      # ---- Gather raw inputs ----
      category <- settings$x
      seg_val  <- settings$y
      segment  <- settings$by

      if (is.null(category) || is.null(seg_val)) {
        stop("type_trials requires both a category variable (x) and a ",
             "value variable (y). Use: cliplot(value ~ category, data=df, ",
             "by=segment, type='trials')", call. = FALSE)
      }

      df <- data.frame(
        category = as.character(category),
        value    = as.numeric(seg_val),
        segment  = if (!is.null(segment)) as.character(segment) else "All",
        stringsAsFactors = FALSE
      )

      # ---- Order categories by descending total ----
      totals <- tapply(df$value, df$category, sum, na.rm = TRUE)
      cat_order <- names(sort(totals, decreasing = TRUE))
      df$category <- factor(df$category, levels = rev(cat_order))

      # ---- Stable segment order ----
      seg_levels <- unique(df$segment)
      df$segment <- factor(df$segment, levels = seg_levels)

      # ---- Stack segments left-to-right within each category ----
      df <- df[order(df$category, df$segment), ]
      rownames(df) <- NULL

      cats <- levels(df$category)
      xmin_vec <- numeric(nrow(df))
      xmax_vec <- numeric(nrow(df))
      xmid_vec <- numeric(nrow(df))

      for (i in seq_along(cats)) {
        idx <- which(df$category == cats[i])
        cum <- cumsum(df$value[idx])
        xmin_vec[idx] <- c(0, utils::head(cum, -1))
        xmax_vec[idx] <- cum
        xmid_vec[idx] <- (xmin_vec[idx] + xmax_vec[idx]) / 2
      }

      df$xmin <- xmin_vec
      df$xmax <- xmax_vec
      df$xmid <- xmid_vec

      # ---- Totals data frame ----
      totals_df <- data.frame(
        category = factor(names(totals), levels = levels(df$category)),
        total    = as.numeric(totals),
        stringsAsFactors = FALSE
      )

      # ---- Segment colours ----
      if (is.null(segment_colors)) {
        n_seg <- length(seg_levels)
        segment_colors <- stats::setNames(
          get_cli_palette(settings$palette %||% "jco", n_seg),
          seg_levels
        )
      }

      # ---- Segment text colours (dark-on-light heuristic) ----
      if (is.null(segment_text_colors)) {
        segment_text_colors <- stats::setNames(
          rep("#173E2B", length(seg_levels)),
          seg_levels
        )
      }

      # ---- Stamp everything into settings ----
      settings$trials_df              <- df
      settings$trials_totals          <- totals_df
      settings$trials_seg_colors      <- segment_colors
      settings$trials_seg_text_colors <- segment_text_colors
      settings$trials_bar_h           <- bar_height
      settings$trials_seg_border      <- segment_border
      settings$trials_seg_border_w    <- segment_border_width
      settings$trials_label_threshold <- label_threshold
      settings$trials_show_totals     <- show_totals
      settings$trials_total_nudge     <- total_nudge

      # Tell the engine not to auto-apply a palette — this type colours
      # everything itself via scale_fill_manual / scale_colour_manual.
      settings$by <- NULL
    },

    draw = function(data, mapping, settings, ...) {
      df     <- settings$trials_df
      if (is.null(df)) return(ggplot2::ggplot())

      totals   <- settings$trials_totals
      seg_col  <- settings$trials_seg_colors
      txt_col  <- settings$trials_seg_text_colors
      bar_h    <- settings$trials_bar_h
      border   <- settings$trials_seg_border
      border_w <- settings$trials_seg_border_w
      thresh   <- settings$trials_label_threshold

      max_total <- max(totals$total, na.rm = TRUE)
      xlim <- max_total * (1 + settings$trials_total_nudge)

      p <- ggplot2::ggplot(df) +
        ggplot2::geom_rect(
          ggplot2::aes(
            xmin = .data[["xmin"]],
            xmax = .data[["xmax"]],
            ymin = as.numeric(.data[["category"]]) - bar_h / 2,
            ymax = as.numeric(.data[["category"]]) + bar_h / 2,
            fill = .data[["segment"]]
          ),
          colour = border,
          linewidth = border_w
        ) +
        ggplot2::scale_fill_manual(values = seg_col, drop = FALSE)

      # ---- Labels inside segments that are wide enough ----
      label_df <- df[df$value >= thresh, , drop = FALSE]
      if (nrow(label_df) > 0) {
        p <- p +
          ggplot2::geom_text(
            data = label_df,
            ggplot2::aes(
              x     = .data[["xmid"]],
              y     = as.numeric(.data[["category"]]),
              label = .data[["value"]],
              colour = .data[["segment"]]
            ),
            size     = 4,
            fontface = "bold"
          ) +
          ggplot2::scale_colour_manual(values = txt_col, guide = "none")
      }

      # ---- Total labels ----
      if (isTRUE(settings$trials_show_totals)) {
        p <- p +
          ggplot2::geom_text(
            data    = totals,
            ggplot2::aes(
              x     = .data[["total"]] + max_total * 0.03,
              y     = as.numeric(.data[["category"]]),
              label = .data[["total"]]
            ),
            hjust    = 0,
            size     = 5,
            fontface = "bold",
            colour   = "#173E2B"
          )
      }

      # ---- Styling ----
      p <- p +
        ggplot2::scale_x_continuous(
          limits  = c(0, xlim),
          expand  = ggplot2::expansion(mult = c(0, 0))
        ) +
        ggplot2::scale_y_discrete(
          labels = function(x) gsub("&", "&\n", x)
        ) +
        ggplot2::labs(x = NULL, y = NULL, fill = NULL) +
        ggplot2::guides(
          fill = ggplot2::guide_legend(
            direction     = "horizontal",
            title.position = "top",
            keywidth      = grid::unit(1.1, "cm"),
            keyheight     = grid::unit(0.55, "cm")
          )
        ) +
        theme_cli_base() +
        ggplot2::theme(
          axis.title          = ggplot2::element_blank(),
          panel.grid.major.y  = ggplot2::element_blank(),
          panel.grid.minor    = ggplot2::element_blank(),
          legend.position     = "top",
          legend.justification = "left"
        )

      p
    },

    name = "trials"
  )
}
