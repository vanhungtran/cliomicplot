# ===========================================================================
# cliomicplot: Scientific Reproduction Scorecard and Distribution Shift Types
# Inspired by Makdissi et al., Nat Metab 2026 (doi: 10.1038/s42255-026-01615-8)
# ===========================================================================

#' Scientific Reproduction Scorecard Type
#'
#' @description Creates publication-grade scientific reproduction and audit
#'   scorecard tables (Figure R6 in Nature Metabolism MNP reproduction).
#'   Renders categorized audit rows with colored verdict status glyphs,
#'   published statement summaries, and re-analysis findings.
#'
#' @param verdict_colors Named character vector of colors for verdict categories:
#'   \code{"reproduced"} (default \code{"#0F4D92"}), \code{"partly"} (default \code{"#8FB4DC"}),
#'   \code{"not reproduced"} (default \code{"#555555"}), \code{"no reference"} (default \code{"#C9C9C9"}),
#'   \code{"not attempted"} (default \code{"#FFFFFF"}).
#' @param border_colors Named character vector of border colors for verdict points.
#' @param dot_size Numeric size of verdict dots (default \code{2.6}).
#' @param text_size Numeric size of row labels (default \code{3.0}).
#' @param header_size Numeric size of group section headers (default \code{3.4}).
#' @param column_x Named numeric vector with x-positions for columns:
#'   \code{c(dot = 1.2, what = 3.5, pub = 45, rep = 64)}.
#' @param ... Additional arguments.
#'
#' @return A \code{cliplot_type} object for use with \code{\link{cliplot}}.
#'
#' @export
type_scorecard = function(
    verdict_colors = c(
      reproduced        = "#0F4D92",
      partly            = "#8FB4DC",
      `not reproduced`  = "#555555",
      `no reference`    = "#C9C9C9",
      `not attempted`   = "#FFFFFF"
    ),
    border_colors  = c(
      reproduced        = "#0F4D92",
      partly            = "#8FB4DC",
      `not reproduced`  = "#555555",
      `no reference`    = "#777777",
      `not attempted`   = "#777777"
    ),
    dot_size       = 2.6,
    text_size      = 3.0,
    header_size    = 3.4,
    column_x       = c(dot = 1.2, what = 3.5, pub = 45, rep = 64),
    ...
) {
  cliplot_type(
    data = function(settings, ...) {
      raw_data = settings$data
      if (is.null(raw_data) || !is.data.frame(raw_data)) {
        stop("type_scorecard requires a data.frame containing 'what', 'pub', 'rep', and 'verdict'")
      }

      df = as.data.frame(raw_data)
      if (!"group" %in% names(df)) df$group = "General"
      if (!"what" %in% names(df))  df$what  = as.character(settings$x)
      if (!"verdict" %in% names(df)) df$verdict = as.character(settings$y)
      if (!"pub" %in% names(df))   df$pub   = ""
      if (!"rep" %in% names(df))   df$rep   = ""

      df$group   = factor(df$group, levels = unique(df$group))
      df$verdict = factor(df$verdict, levels = names(verdict_colors))
      df$idx     = seq_len(nrow(df))

      # Layout calculation: assign vertical y coordinates
      df$y = NA_real_
      yy = 0
      hy = numeric(0)
      for (g in levels(df$group)) {
        yy = yy - 1.55
        hy[g] = yy
        g_idxs = df$idx[df$group == g]
        for (k in g_idxs) {
          yy = yy - 1.0
          df$y[df$idx == k] = yy
        }
      }

      hdr = data.frame(
        group = levels(df$group),
        y     = hy[levels(df$group)],
        stringsAsFactors = FALSE
      )

      settings$sc_df       = df
      settings$sc_hdr      = hdr
      settings$sc_vcol     = verdict_colors
      settings$sc_vbord    = border_colors
      settings$sc_dsize    = dot_size
      settings$sc_tsize    = text_size
      settings$sc_hsize    = header_size
      settings$sc_col_x    = column_x
    },
    draw = function(data, mapping, settings, ...) {
      df  = settings$sc_df
      hdr = settings$sc_hdr
      if (is.null(df) || nrow(df) == 0) return(ggplot2::ggplot())

      X = settings$sc_col_x

      p = ggplot2::ggplot() +
        # Section divider lines
        ggplot2::geom_segment(
          data = hdr,
          ggplot2::aes(x = 0, xend = 100, y = .data[["y"]] - 0.5, yend = .data[["y"]] - 0.5),
          colour = "grey75", linewidth = 0.35
        ) +
        # Section headers
        ggplot2::geom_text(
          data = hdr,
          ggplot2::aes(x = 0, y = .data[["y"]], label = .data[["group"]]),
          hjust = 0, vjust = 0.2, size = settings$sc_hsize, fontface = "bold", colour = "grey10"
        ) +
        # Verdict dots
        ggplot2::geom_point(
          data = df,
          ggplot2::aes(x = X[["dot"]], y = .data[["y"]], fill = .data[["verdict"]], colour = .data[["verdict"]]),
          shape = 21, size = settings$sc_dsize, stroke = 0.6
        ) +
        # Text columns
        ggplot2::geom_text(
          data = df,
          ggplot2::aes(x = X[["what"]], y = .data[["y"]], label = .data[["what"]]),
          hjust = 0, size = settings$sc_tsize, colour = "grey10"
        ) +
        ggplot2::geom_text(
          data = df,
          ggplot2::aes(x = X[["pub"]], y = .data[["y"]], label = .data[["pub"]]),
          hjust = 0, size = settings$sc_tsize, colour = "grey30"
        ) +
        ggplot2::geom_text(
          data = df,
          ggplot2::aes(x = X[["rep"]], y = .data[["y"]], label = .data[["rep"]]),
          hjust = 0, size = settings$sc_tsize, colour = "grey10"
        ) +
        # Column headers
        ggplot2::annotate("text", x = X[["what"]], y = 0.3, label = "Published quantity",
                          hjust = 0, size = settings$sc_tsize * 1.05, fontface = "bold", colour = "grey35") +
        ggplot2::annotate("text", x = X[["pub"]], y = 0.3, label = "Paper reports",
                          hjust = 0, size = settings$sc_tsize * 1.05, fontface = "bold", colour = "grey35") +
        ggplot2::annotate("text", x = X[["rep"]], y = 0.3, label = "Re-analysis finding",
                          hjust = 0, size = settings$sc_tsize * 1.05, fontface = "bold", colour = "grey35") +
        # Scales & theme
        ggplot2::scale_fill_manual(values = settings$sc_vcol, name = NULL, drop = FALSE) +
        ggplot2::scale_colour_manual(values = settings$sc_vbord, name = NULL, drop = FALSE) +
        ggplot2::guides(
          fill = ggplot2::guide_legend(nrow = 1, override.aes = list(size = 3.0)),
          colour = "none"
        ) +
        ggplot2::scale_x_continuous(limits = c(0, 100), expand = ggplot2::expansion(mult = c(0, 0))) +
        ggplot2::scale_y_continuous(limits = c(min(df$y) - 0.8, 1.2), expand = ggplot2::expansion(mult = c(0, 0))) +
        clitheme(preset = "nature") +
        ggplot2::theme(
          axis.line = ggplot2::element_blank(),
          axis.text = ggplot2::element_blank(),
          axis.ticks = ggplot2::element_blank(),
          axis.title = ggplot2::element_blank(),
          panel.grid = ggplot2::element_blank(),
          legend.position = "top",
          legend.justification = "left",
          legend.margin = ggplot2::margin(0, 0, 0, 0)
        )

      p
    }
  )
}

#' Binned Distribution Shift Type
#'
#' @description Creates binned distribution and size shift profiles
#'   (Figure R10 in Nature Metabolism MNP reproduction). Plots binned frequencies
#'   across treatment cohorts alongside difference-to-control shift curves.
#'
#' @param mode One of \code{"distribution"} (raw binned fractions) or \code{"difference"}
#'   (difference to reference group).
#' @param ref_group Name of control/reference cohort (default \code{"Ctrl"}).
#' @param line_width Linewidth for trajectory lines (default \code{0.9}).
#' @param point_size Size of bin points (default \code{2.6}).
#' @param ... Additional arguments.
#'
#' @return A \code{cliplot_type} object for use with \code{\link{cliplot}}.
#'
#' @export
type_binned_shift = function(
    mode       = c("distribution", "difference"),
    ref_group  = "Ctrl",
    line_width = 0.9,
    point_size = 2.6,
    ...
) {
  mode = match.arg(mode)

  cliplot_type(
    data = function(settings, ...) {
      df = data.frame(
        bin   = as.factor(settings$x),
        value = as.numeric(settings$y),
        stringsAsFactors = FALSE
      )
      if (!is.null(settings$by)) {
        df$group = as.factor(settings$by)
      } else {
        df$group = as.factor("Group")
      }

      if (mode == "difference" && ref_group %in% levels(df$group)) {
        # Subtract reference group per bin
        ref_vals = df[df$group == ref_group, , drop = FALSE]
        ref_map  = setNames(ref_vals$value, as.character(ref_vals$bin))

        df$diff = df$value - ref_map[as.character(df$bin)]
        df$plot_val = df$diff
        df = df[df$group != ref_group, , drop = FALSE]
      } else {
        df$plot_val = df$value
      }

      settings$shift_df     = df
      settings$shift_mode   = mode
      settings$shift_lwidth = line_width
      settings$shift_psize  = point_size
      settings$shift_ref    = ref_group
    },
    draw = function(data, mapping, settings, ...) {
      df = settings$shift_df
      if (is.null(df) || nrow(df) == 0) return(ggplot2::ggplot())

      p = ggplot2::ggplot(
        df,
        ggplot2::aes(
          x = .data[["bin"]], y = .data[["plot_val"]],
          color = .data[["group"]], group = .data[["group"]]
        )
      )

      if (settings$shift_mode == "difference") {
        p = p + ggplot2::geom_hline(yintercept = 0, linetype = "dashed", color = "grey65", linewidth = 0.5)
      }

      p = p +
        ggplot2::geom_line(linewidth = settings$shift_lwidth) +
        ggplot2::geom_point(size = settings$shift_psize)

      p
    }
  )
}
