# ===========================================================================
# cliomicplot: Venn Diagram Type
# Inspired by cnsplots vennplot() - set overlap visualization (2-3 sets)
# ===========================================================================

# Internal helpers -----------------------------------------------------------

.circle_pts = function(cx, cy, r, n = 120) {
  th = seq(0, 2 * pi, length.out = n)
  data.frame(x = cx + r * cos(th), y = cy + r * sin(th))
}

# 2-set layout: region counts + label positions
.venn2_regions = function(a, b) {
  nA  = length(a)
  nB  = length(b)
  nAB = length(intersect(a, b))
  data.frame(
    label = c(nA - nAB, nAB, nB - nAB),
    x = c(-1.02, 0, 1.02),
    y = c(0, 0, 0),
    stringsAsFactors = FALSE
  )
}

# 3-set layout using an equilateral arrangement
.venn3_regions = function(a, b, c) {
  nA   = length(a)
  nB   = length(b)
  nC   = length(c)
  nAB  = length(Reduce(intersect, list(a, b)))
  nAC  = length(Reduce(intersect, list(a, c)))
  nBC  = length(Reduce(intersect, list(b, c)))
  nABC = length(Reduce(intersect, list(a, b, c)))
  data.frame(
    label = c(
      nA - nAB - nAC + nABC,       # A only
      nB - nAB - nBC + nABC,       # B only
      nC - nAC - nBC + nABC,       # C only
      nAB - nABC,                  # AB only
      nAC - nABC,                  # AC only
      nBC - nABC,                  # BC only
      nABC                         # ABC
    ),
    x = c(-0.61, -1.42, 1.42, -0.49, 0.49, 0, 0),
    y = c(0.94, -0.56, -0.56, 0.18, 0.18, -0.95, -0.18),
    stringsAsFactors = FALSE
  )
}

#' Venn Diagram Type
#'
#' @description Creates publication-ready Venn diagrams for 2-3 sets, showing
#'   unique and shared element counts. Built entirely with ggplot2 so the
#'   result integrates with the journal themes and palettes of cliomicplot.
#'   Inspired by cnsplots' \code{vennplot()}.
#'
#' @param sets Optional named list of 1-3 sets. When \code{NULL}, sets are
#'   resolved from the data (a named list passed as \code{x}).
#' @param colors Optional vector of colors for the sets. Defaults to the active
#'   cliomicplot palette.
#' @param fill_alpha Fill transparency of the circles (default 0.42).
#' @param linewidth Outline width of the circles (default 0.8).
#' @param label_size Size of the region-count labels (default 3.4).
#' @param set_label_size Size of the set-name labels (default 3.8).
#' @param show_counts Show region counts (default TRUE).
#'
#' @return A \code{cliplot_type} object for use with \code{\link{cliplot}}.
#'
#' @details
#' Supports 1-3 sets. For more than 3 sets use \code{\link{type_upset}}.
#'
#' @examples
#' \dontrun{
#' sets = list(Up = c("TP53", "BRCA1", "MYC"),
#'             Down = c("TP53", "KRAS", "APC"),
#'             Mut = c("TP53", "EGFR", "PTEN"))
#' cliplot(sets, type = "venn", theme = "nature")
#' }
#'
#' @export
type_venn = function(
    sets           = NULL,
    colors         = NULL,
    fill_alpha     = 0.42,
    linewidth      = 0.8,
    label_size     = 3.4,
    set_label_size = 3.8,
    show_counts    = TRUE
) {
  cliplot_type(
    data = function(settings, ...) {
      s = sets
      if (is.null(s)) {
        if (is.list(settings$x) && !is.data.frame(settings$x)) {
          s = settings$x
        } else if (!is.null(settings$data) && is.data.frame(settings$data)) {
          num_cols = names(settings$data)[vapply(settings$data, function(col) {
            is.numeric(col) && all(unique(col) %in% c(0, 1, NA))
          }, logical(1))]
          if (length(num_cols) > 0) {
            s = lapply(num_cols, function(nm) {
              rownames(settings$data)[as.logical(settings$data[[nm]])]
            })
            names(s) = num_cols
          }
        }
      }
      if (is.null(s)) {
        stop("type_venn needs a named list of 1-3 sets.", call. = FALSE)
      }
      s = lapply(s, function(v) unique(as.character(v)))
      if (length(s) > 3) {
        stop("type_venn supports at most 3 sets; use type_upset for more.",
             call. = FALSE)
      }
      settings$venn_sets = s
      settings$venn_colors = colors
      settings$venn_fill_alpha = fill_alpha
      settings$venn_linewidth = linewidth
      settings$venn_label_size = label_size
      settings$venn_set_label_size = set_label_size
      settings$venn_show_counts = show_counts
    },
    draw = function(data, mapping, settings, ...) {
      s = settings$venn_sets
      if (is.null(s)) return(ggplot2::ggplot())
      k = length(s)
      nms = names(s)
      if (is.null(nms)) nms = paste0("Set", seq_len(k))

      cols = settings$venn_colors %||%
        get_cli_palette(settings$palette %||% "npg", k)

      p = ggplot2::ggplot()

      if (k == 1) {
        circ = .circle_pts(0, 0, 1)
        p = p +
          ggplot2::geom_polygon(
            data = circ,
            ggplot2::aes(x = .data[["x"]], y = .data[["y"]]),
            fill = cols[1], alpha = settings$venn_fill_alpha,
            color = cols[1], linewidth = settings$venn_linewidth
          )
        if (isTRUE(settings$venn_show_counts)) {
          p = p + ggplot2::annotate(
            "text", x = 0, y = 0, label = length(s[[1]]),
            size = settings$venn_label_size, fontface = "bold"
          )
        }
        p = p + ggplot2::annotate(
          "text", x = 0, y = 1.22, label = nms[1],
          size = settings$venn_set_label_size, fontface = "bold"
        )
      } else if (k == 2) {
        circA = .circle_pts(-0.5, 0, 1)
        circB = .circle_pts(0.5, 0, 1)
        p = p +
          ggplot2::geom_polygon(
            data = circA,
            ggplot2::aes(x = .data[["x"]], y = .data[["y"]]),
            fill = cols[1], alpha = settings$venn_fill_alpha,
            color = cols[1], linewidth = settings$venn_linewidth
          ) +
          ggplot2::geom_polygon(
            data = circB,
            ggplot2::aes(x = .data[["x"]], y = .data[["y"]]),
            fill = cols[2], alpha = settings$venn_fill_alpha,
            color = cols[2], linewidth = settings$venn_linewidth
          )
        if (isTRUE(settings$venn_show_counts)) {
          regs = .venn2_regions(s[[1]], s[[2]])
          p = p + ggplot2::geom_text(
            data = regs,
            ggplot2::aes(x = .data[["x"]], y = .data[["y"]],
                         label = .data[["label"]]),
            size = settings$venn_label_size, fontface = "bold"
          )
        }
        p = p +
          ggplot2::annotate("text", x = -0.5, y = 1.22, label = nms[1],
                            size = settings$venn_set_label_size, fontface = "bold") +
          ggplot2::annotate("text", x = 0.5, y = 1.22, label = nms[2],
                            size = settings$venn_set_label_size, fontface = "bold")
      } else {
        centers = data.frame(
          x = c(0, -0.61, 0.61),
          y = c(0.35, -0.175, -0.175)
        )
        for (j in seq_len(3)) {
          circ = .circle_pts(centers$x[j], centers$y[j], 1)
          p = p + ggplot2::geom_polygon(
            data = circ,
            ggplot2::aes(x = .data[["x"]], y = .data[["y"]]),
            fill = cols[j], alpha = settings$venn_fill_alpha,
            color = cols[j], linewidth = settings$venn_linewidth
          )
        }
        if (isTRUE(settings$venn_show_counts)) {
          regs = .venn3_regions(s[[1]], s[[2]], s[[3]])
          p = p + ggplot2::geom_text(
            data = regs,
            ggplot2::aes(x = .data[["x"]], y = .data[["y"]],
                         label = .data[["label"]]),
            size = settings$venn_label_size, fontface = "bold"
          )
        }
        p = p +
          ggplot2::annotate("text", x = 0, y = 1.3, label = nms[1],
                            size = settings$venn_set_label_size, fontface = "bold") +
          ggplot2::annotate("text", x = -1.32, y = -0.5, label = nms[2],
                            size = settings$venn_set_label_size, fontface = "bold") +
          ggplot2::annotate("text", x = 1.32, y = -0.5, label = nms[3],
                            size = settings$venn_set_label_size, fontface = "bold")
      }

      p = p +
        ggplot2::coord_fixed() +
        ggplot2::theme_void() +
        ggplot2::theme(plot.margin = ggplot2::margin(8, 8, 8, 8))

      p
    },
    name = "venn"
  )
}
