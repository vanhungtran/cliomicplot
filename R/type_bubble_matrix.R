# ===========================================================================
# cliomicplot: Differential Abundance Bubble Matrix
# Clinical & Omics Figure Archetype 29
# ===========================================================================

#' Differential Abundance Bubble Matrix Dot Plot
#'
#' @description
#' Creates publication-ready bubble matrix dot plots for differential abundance,
#' gene expression, or biomarker screening across clinical subgroups, anatomical
#' sites, or study visits. Encodes statistical significance (e.g.
#' \eqn{-\log_{10}(q)}) as point size and directional effect size (e.g.
#' \eqn{\log_2(\text{FC})}) as a diverging color gradient.
#'
#' @param size_col Column name for significance or point size (e.g. "padj",
#'   "qvalue", or "-log10q").
#' @param fill_col Column name for effect size or direction (e.g. "log2FC" or
#'   "estimate").
#' @param min_size Minimum point radius in mm (default 1.5).
#' @param max_size Maximum point radius in mm (default 6.5).
#' @param midpoint Value representing no directional change (default 0).
#' @param palette Character string specifying publication diverging palette:
#'   \code{"nature"} (default: Navy to Crimson), \code{"rdbu"}, \code{"bipolar"},
#'   \code{"lancet"}, \code{"dark"}, \code{"spectral"}, \code{"piyg"}, or \code{"prgn"}.
#' @param colors Diverging color gradient vector of length 3 (low, mid, high).
#'   If supplied, overrides \code{palette}. Default uses \code{palette = "nature"}
#'   (\code{c("#3B4992", "#F7F7F7", "#EE0000")}).
#' @param limits Numeric vector of length 2 specifying color scale limits.
#'   Default \code{c(-2, 2)}, automatically expanded symmetrically if effect sizes exceed 2.
#' @param border_color Border stroke color for points (default \code{"grey30"}).
#' @param stroke Border stroke width for points (default 0.35).
#' @param size_name Legend title for point size (default \code{expression(-log[10]~italic(q))}).
#' @param fill_name Legend title for color gradient (default \code{expression(log[2]~"Fold Change")}).
#' @param reorder_by Optional column or vector to reorder features along the y-axis.
#' @param grid_color Color for background grid lines (default \code{"#EEEEEE"}).
#' @param xlab,ylab Axis labels.
#'
#' @return A \code{cliplot_type} object for use with \code{\link{cliplot}}.
#'
#' @examples
#' \dontrun{
#' # Simulated differential abundance results
#' set.seed(42)
#' da_df <- expand.grid(
#'   Species = c("S. epidermidis", "S. aureus", "C. acnes", "M. restricta", "P. melaninogenica"),
#'   Comparison = c("Forehead (T1)", "Cheek (T1)", "Antecubital (T1)", "Antecubital (T3)")
#' )
#' da_df$log2FC <- rnorm(nrow(da_df), mean = 0, sd = 1.2)
#' da_df$qval <- runif(nrow(da_df), 0.001, 0.2)
#'
#' cliplot(Species ~ Comparison, data = da_df,
#'         type = type_bubble_matrix(size_col = "qval", fill_col = "log2FC", palette = "nature"))
#' }
#'
#' @export
type_bubble_matrix = function(
    size_col     = NULL,
    fill_col     = NULL,
    min_size     = 1.5,
    max_size     = 6.5,
    midpoint     = 0,
    palette      = "nature",
    colors       = NULL,
    limits       = c(-2, 2),
    border_color = "grey30",
    stroke       = 0.35,
    size_name    = expression(-log[10]~italic(q)),
    fill_name    = expression(log[2]~"Fold Change"),
    reorder_by   = NULL,
    grid_color   = "#EEEEEE",
    xlab         = NULL,
    ylab         = NULL
) {
  pal_map <- list(
    nature   = c("#3B4992", "#F7F7F7", "#EE0000"),
    rdbu     = c("#2166AC", "#F7F7F7", "#B2182B"),
    bipolar  = c("#1F77B4", "#F0F0F0", "#D62728"),
    lancet   = c("#00468B", "#F5F5F5", "#ED0000"),
    dark     = c("#00F2FE", "#1E2638", "#FF007F"),
    cell     = c("#00F2FE", "#1E2638", "#FF007F"),
    spectral = c("#2B83BA", "#FFFFBF", "#D7191C"),
    piyg     = c("#C51B7D", "#F7F7F7", "#4D9221"),
    prgn     = c("#762A83", "#F7F7F7", "#1B7837")
  )

  if (is.null(colors)) {
    pal_key <- if (!is.null(palette)) tolower(sub("[-_]", "", palette)) else "nature"
    colors <- pal_map[[pal_key]] %||% pal_map$nature
  }
  cliplot_type(
    data = function(settings, ...) {
      df <- settings$data
      if (is.null(df)) {
        df <- data.frame(
          x = as.character(settings$x),
          y = as.character(settings$y),
          stringsAsFactors = FALSE
        )
      } else {
        nms <- names(df)
        candidates_x <- intersect(c("comparison", "condition", "Comparison", "Condition", "site", "visit", "sample"), nms)
        x_col <- if (is.character(settings$x) && length(settings$x) == 1 && settings$x %in% nms) {
          settings$x
        } else if (length(candidates_x) > 0) {
          candidates_x[1]
        } else {
          nms[1]
        }

        candidates_y <- intersect(c("feature", "species", "gene", "Species", "Gene", "Feature", "taxon", "Taxon"), nms)
        y_col <- if (is.character(settings$y) && length(settings$y) == 1 && settings$y %in% nms) {
          settings$y
        } else if (length(candidates_y) > 0) {
          candidates_y[1]
        } else {
          nms[min(2, length(nms))]
        }

        if (!is.null(settings$x) && length(settings$x) == nrow(df)) {
          df$.x <- as.character(settings$x)
        } else if (!is.null(x_col) && !is.na(x_col) && x_col %in% nms) {
          df$.x <- as.character(df[[x_col]])
        } else {
          df$.x <- as.character(df[[1]])
        }

        if (!is.null(settings$y) && length(settings$y) == nrow(df)) {
          df$.y <- as.character(settings$y)
        } else if (!is.null(y_col) && !is.na(y_col) && y_col %in% nms) {
          df$.y <- as.character(df[[y_col]])
        } else {
          df$.y <- as.character(df[[min(2, ncol(df))]])
        }

        # Size metric
        s_col <- size_col %||% intersect(c("qval", "qvalue", "padj", "pvalue", "fdr", "log10q", "size"), nms)[1]
        if (!is.null(s_col) && s_col %in% nms) {
          raw_s <- as.numeric(df[[s_col]])
          if (grepl("log10", tolower(s_col))) {
            df$.size <- raw_s
          } else {
            df$.size <- -log10(pmax(raw_s, .Machine$double.xmin))
          }
        } else {
          df$.size <- rep(3, nrow(df))
        }

        # Fill metric
        f_col <- fill_col %||% intersect(c("log2fc", "log2FC", "fc", "logfc", "estimate", "coef", "beta", "fill"), nms)[1]
        if (!is.null(f_col) && f_col %in% nms) {
          df$.fill <- as.numeric(df[[f_col]])
        } else {
          df$.fill <- rep(0, nrow(df))
        }
      }

      # Optional reordering of y
      if (!is.null(reorder_by) && is.character(reorder_by) && reorder_by %in% names(df)) {
        agg_val <- tapply(df[[reorder_by]], df$.y, median, na.rm = TRUE)
        df$.y <- factor(df$.y, levels = names(sort(agg_val)))
      } else {
        df$.y <- factor(df$.y, levels = rev(unique(df$.y)))
      }
      df$.x <- factor(df$.x, levels = unique(df$.x))

      settings$bubble_df <- df
    },

    draw = function(data, mapping, settings, ...) {
      df <- settings$bubble_df
      if (is.null(df)) return(ggplot2::ggplot())

      # Dynamic symmetric limits if fold changes exceed limits
      act_limits <- limits
      max_obs <- if (nrow(df) > 0) max(abs(df$.fill), na.rm = TRUE) else 2
      if (is.null(act_limits)) {
        max_ceil <- ceiling(max_obs * 10) / 10
        act_limits <- c(-max_ceil, max_ceil)
      } else if (identical(act_limits, c(-2, 2)) && max_obs > 2) {
        max_ceil <- ceiling(max_obs * 10) / 10
        act_limits <- c(-max_ceil, max_ceil)
      }

      is_dark <- (is.character(palette) && palette %in% c("dark", "cell")) ||
                 identical(colors, pal_map$dark) ||
                 identical(colors, pal_map$cell)

      bg_col     <- if (is_dark) "#0F141C" else "white"
      border_box <- if (is_dark) "#334155" else "grey75"
      grid_col   <- if (is_dark) "#1E293B" else grid_color
      pt_border  <- if (is_dark) "#0F141C" else (border_color %||% "grey30")
      txt_col    <- if (is_dark) "#F1F5F9" else "grey20"

      p <- ggplot2::ggplot(df, ggplot2::aes(x = .data[[".x"]], y = .data[[".y"]])) +
        ggplot2::geom_point(
          ggplot2::aes(size = .data[[".size"]], fill = .data[[".fill"]]),
          shape = 21,
          color = pt_border,
          stroke = stroke
        ) +
        ggplot2::scale_fill_gradient2(
          low = colors[1],
          mid = colors[2],
          high = colors[3],
          midpoint = midpoint,
          limits = act_limits,
          oob = scales::squish,
          name = fill_name
        ) +
        ggplot2::scale_size_continuous(
          range = c(min_size, max_size),
          name = size_name
        ) +
        ggplot2::guides(
          size = ggplot2::guide_legend(
            override.aes = list(fill = "grey65", color = "grey20", shape = 21, stroke = stroke)
          ),
          fill = ggplot2::guide_colorbar(
            barwidth = ggplot2::unit(4, "mm"),
            barheight = ggplot2::unit(28, "mm"),
            frame.colour = if (is_dark) "#475569" else "grey40",
            frame.linewidth = 0.3,
            ticks.colour = if (is_dark) "#475569" else "grey40"
          )
        ) +
        ggplot2::labs(
          x = xlab %||% "",
          y = ylab %||% ""
        ) +
        ggplot2::theme(
          plot.background  = ggplot2::element_rect(fill = bg_col, color = NA),
          panel.background = ggplot2::element_rect(fill = bg_col, color = NA),
          panel.border     = ggplot2::element_rect(color = border_box, fill = NA, linewidth = 0.5),
          panel.grid.major = ggplot2::element_line(color = grid_col, linewidth = 0.35),
          panel.grid.minor = ggplot2::element_blank(),
          axis.text.x      = ggplot2::element_text(angle = 45, hjust = 1, vjust = 1, color = txt_col),
          axis.text.y      = ggplot2::element_text(face = "italic", color = txt_col)
        )

      p
    },
    name = "bubble_matrix"
  )
}
