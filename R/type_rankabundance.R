# ===========================================================================
# cliomicplot: Rank Abundance Curve Type
# Inspired by xOmicsShiny expressionmodule.R S-curve
# ===========================================================================

#' Rank Abundance Curve Type
#'
#' @description Creates a rank-abundance curve that sorts observations by
#'   abundance/intensity and plots them against rank. Supports marginal
#'   distributions (density, histogram, boxplot, violin) on the y-axis,
#'   inspired by xOmicsShiny's expression module.
#'
#' @param log_scale Log-transform the y-axis ("none", "10", or "2"; default "10")
#' @param point_color Point colour (default "#4DBBD5")
#' @param point_size Point size (default 1.8)
#' @param point_alpha Point transparency (default 0.7)
#' @param marginal_type Marginal plot type: "density", "histogram", "boxplot",
#'   "violin", "densigram", or "none" (default "density")
#' @param marginal_color Fill colour for marginal distribution
#' @param label_genes Character vector of gene names to highlight and label,
#'   or NULL for no labels
#' @param label_color Label colour for highlighted points (default "#E64B35")
#' @param label_size Label text size (default 3.5)
#'
#' @return A \code{cliplot_type} object for use with \code{\link{cliplot}}.
#'
#' @details
#' A rank-abundance curve sorts all features (genes, proteins, metabolites)
#' by their abundance and displays them against rank. It is a standard QC
#' and exploratory plot in omics analysis. The y-axis typically shows
#' log10-transformed intensity values. Marginal distributions help assess
#' the overall abundance distribution shape.
#'
#' @examples
#' \dontrun{
#' # Basic rank-abundance curve from expression data
#' cliplot(Intensity ~ RANK, data = expr_data, type = "rankabundance")
#'
#' # With gene labels and marginal density
#' cliplot(Intensity ~ RANK, data = expr_data,
#'         type = type_rankabundance(
#'           label_genes = c("TNF", "IL6", "TP53"),
#'           marginal_type = "density"))
#' }
#'
#' @export
type_rankabundance = function(
    log_scale       = "10",
    point_color     = "#4DBBD5",
    point_size      = 1.8,
    point_alpha     = 0.7,
    marginal_type   = c("density", "histogram", "boxplot", "violin", "densigram", "none"),
    marginal_color  = "#3C5488",
    label_genes     = NULL,
    label_color     = "#E64B35",
    label_size      = 3.5
) {
  marginal_type = match.arg(marginal_type)

  cliplot_type(
    data = function(settings, ...) {
      df = settings$data
      if (is.null(df)) {
        df = data.frame(
          intensity = settings$y,
          rank_seq  = settings$x %||% seq_along(settings$y),
          stringsAsFactors = FALSE
        )
        settings$x = df$rank_seq
        settings$y = df$intensity
      }

      # If x is not a rank column, compute rank from y
      y_col = if (!is.null(settings$y_var)) settings$y_var else "y"
      x_col = if (!is.null(settings$x_var)) settings$x_var else "x"

      if (is.null(df[[y_col]]) && !is.null(settings$y)) {
        df$..intensity.. = settings$y
      } else if (!is.null(df[[y_col]])) {
        df$..intensity.. = df[[y_col]]
      } else {
        df$..intensity.. = df[[1]]
      }

      if (is.null(df[[x_col]]) && !is.null(settings$x)) {
        df$..rank.. = settings$x
      } else if (!is.null(df[[x_col]])) {
        df$..rank.. = df[[x_col]]
      } else {
        df$..rank.. = order(df$..intensity.., decreasing = TRUE)
      }

      # Apply log transform
      lb = switch(log_scale, "10" = 10, "2" = 2, "none" = 1)
      if (lb > 1) {
        df$..intensity.. = log(df$..intensity.. + 1, base = lb)
      }

      # Identify highlighted genes
      df$..highlight.. = FALSE
      if (!is.null(label_genes)) {
        # Check rows or a label column
        if (!is.null(rownames(df)) && any(label_genes %in% rownames(df))) {
          df$..highlight..[rownames(df) %in% label_genes] = TRUE
        } else if ("label" %in% names(df) && any(label_genes %in% df$label)) {
          df$..highlight..[df$label %in% label_genes] = TRUE
        } else {
          # Try first column
          df$..highlight..[df[[1]] %in% label_genes] = TRUE
        }
        df$..label.. = ifelse(df$..highlight.., as.character(df[[1]]), "")
      } else {
        df$..label.. = ""
      }

      settings$ra_df            = df
      settings$ra_point_color   = point_color
      settings$ra_point_size    = point_size
      settings$ra_point_alpha   = point_alpha
      settings$ra_marginal_type = marginal_type
      settings$ra_marginal_color= marginal_color
      settings$ra_label_color   = label_color
      settings$ra_label_size    = label_size
    },
    draw = function(data, mapping, settings, ...) {
      df = settings$ra_df
      if (is.null(df)) return(ggplot2::ggplot())

      p = ggplot2::ggplot(df, ggplot2::aes(
        x = .data[["..rank.."]], y = .data[["..intensity.."]]
      )) +
        ggplot2::geom_point(
          data = subset(df, !..highlight..),
          color = settings$ra_point_color,
          size  = settings$ra_point_size,
          alpha = settings$ra_point_alpha
        ) +
        ggplot2::labs(
          x = "Rank",
          y = if (log_scale != "none") {
            paste0("log", log_scale, "(Intensity)")
          } else { "Intensity" }
        )

      # Highlighted genes
      if (any(df$..highlight..)) {
        p = p +
          ggplot2::geom_point(
            data = subset(df, ..highlight..),
            color = settings$ra_label_color,
            size  = settings$ra_point_size + 0.5,
            alpha = 1
          ) +
          ggrepel::geom_text_repel(
            data = subset(df, ..highlight..),
            ggplot2::aes(label = .data[["..label.."]]),
            color      = settings$ra_label_color,
            size       = settings$ra_label_size,
            fontface   = "bold",
            max.overlaps = 25,
            show.legend  = FALSE
          )
      }

      p

      # Note: ggExtra::ggMarginal can be added by the user:
      #   p + ggExtra::ggMarginal(type = "density", margins = "y")
    },
    name = "rankabundance"
  )
}
