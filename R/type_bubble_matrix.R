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
#' @param colors Diverging color gradient vector of length 3 (low, mid, high).
#'   Default \code{c("#4575B4", "grey92", "#D73027")}.
#' @param limits Numeric vector of length 2 specifying color scale limits.
#'   Default \code{c(-2, 2)}.
#' @param size_name Legend title for point size (default \code{expression(-log[10]~italic(q))}).
#' @param fill_name Legend title for color gradient (default \code{expression(log[2]~"Fold Change")}).
#' @param reorder_by Optional column or vector to reorder features along the y-axis.
#' @param grid_color Color for background grid lines (default \code{"#EBEBEB"}).
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
#'         type = type_bubble_matrix(size_col = "qval", fill_col = "log2FC"))
#' }
#'
#' @export
type_bubble_matrix = function(
    size_col     = NULL,
    fill_col     = NULL,
    min_size     = 1.5,
    max_size     = 6.5,
    midpoint     = 0,
    colors       = c("#4575B4", "grey92", "#D73027"),
    limits       = c(-2, 2),
    size_name    = expression(-log[10]~italic(q)),
    fill_name    = expression(log[2]~"Fold Change"),
    reorder_by   = NULL,
    grid_color   = "#EBEBEB",
    xlab         = NULL,
    ylab         = NULL
) {
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
        x_col <- if (is.character(settings$x) && length(settings$x) == 1 && settings$x %in% nms) {
          settings$x
        } else {
          intersect(c("comparison", "condition", "Comparison", "Condition", "site", "visit"), nms)[1] %||% nms[1]
        }

        y_col <- if (is.character(settings$y) && length(settings$y) == 1 && settings$y %in% nms) {
          settings$y
        } else {
          intersect(c("feature", "species", "gene", "Species", "Gene", "Feature"), nms)[1] %||% nms[2]
        }

        df$.x <- as.character(if (!is.null(settings$x) && !is.character(settings$x)) settings$x else df[[x_col]])
        df$.y <- as.character(if (!is.null(settings$y) && !is.character(settings$y)) settings$y else df[[y_col]])

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

      p <- ggplot2::ggplot(df, ggplot2::aes(x = .data[[".x"]], y = .data[[".y"]])) +
        ggplot2::geom_point(
          ggplot2::aes(size = .data[[".size"]], color = .data[[".fill"]]),
          stroke = 0.3
        ) +
        ggplot2::scale_color_gradient2(
          low = colors[1],
          mid = colors[2],
          high = colors[3],
          midpoint = midpoint,
          limits = limits,
          oob = scales::squish,
          name = fill_name
        ) +
        ggplot2::scale_size_continuous(
          range = c(min_size, max_size),
          name = size_name
        ) +
        ggplot2::labs(
          x = xlab %||% "",
          y = ylab %||% ""
        ) +
        ggplot2::theme(
          panel.grid.major = ggplot2::element_line(color = grid_color, linewidth = 0.3),
          panel.grid.minor = ggplot2::element_blank(),
          axis.text.x = ggplot2::element_text(angle = 45, hjust = 1, vjust = 1),
          axis.text.y = ggplot2::element_text(face = "italic")
        )

      p
    },
    name = "bubble_matrix"
  )
}
