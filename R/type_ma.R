# ===========================================================================
# cliomicplot: MA Plot Type
# For differential expression analysis (MD plot)
# ===========================================================================

#' MA Plot Type
#'
#' @description Creates publication-ready MA (Mean-Average) plots for
#'   visualizing differential expression results. Shows log2 fold-change
#'   vs. mean expression with significance highlighting.
#'
#' @param pval_cutoff P-value cutoff for coloring significant genes (default 0.05)
#' @param point_size Point size (default 1.8)
#' @param point_alpha Point transparency (default 0.65)
#' @param sig_color Color for significant points (default "#E64B35")
#' @param down_color Color for significant negative fold changes.
#' @param ns_color Color for non-significant points (default "grey60")
#' @param add_loess Add LOESS smoothing line (default TRUE)
#' @param loess_color Color for LOESS line (default "#3C5488")
#'
#' @return A cliplot_type object for use with \code{\link{cliplot}}.
#'
#' @examples
#' \dontrun{
#' # From DESeq2 results
#' cliplot(log2FoldChange ~ baseMean, data = deseq_res, type = "ma")
#' }
#'
#' @export
type_ma = function(
    pval_cutoff  = 0.05,
    point_size   = 1.8,
    point_alpha  = 0.65,
    sig_color    = "#D73027",
    down_color   = "#2B6CB0",
    ns_color     = "#A8B0BA",
    add_loess    = TRUE,
    loess_color  = "#263238"
) {
  cliplot_type(
    data = function(settings, ...) {
      df = settings$data
      if (is.null(df)) {
        df = data.frame(
          baseMean = settings$x,
          logFC    = settings$y,
          stringsAsFactors = FALSE
        )
      } else {
        df$baseMean = settings$x
        df$logFC    = settings$y
      }

      # Check for p-value column
      if ("padj" %in% names(df) || "pvalue" %in% names(df)) {
        df$pval = df$padj %||% df$pvalue
      } else {
        df$pval = 1  # No significance info
      }

      significant = !is.na(df$pval) & df$pval < pval_cutoff
      df$sig = "NS"
      df$sig[significant & df$logFC >= 0] = "Up"
      df$sig[significant & df$logFC < 0] = "Down"
      df$sig = factor(df$sig, levels = c("Up", "Down", "NS"))

      settings$ma_data = df
    },
    draw = function(data, mapping, settings, ...) {
      df = settings$ma_data
      if (is.null(df)) return(ggplot2::ggplot())

      sig_colors = c("Up" = sig_color, "Down" = down_color, "NS" = ns_color)

      p = ggplot2::ggplot(df, ggplot2::aes(
        x     = .data[["baseMean"]],
        y     = .data[["logFC"]],
        color = .data[["sig"]]
      )) +
        ggplot2::geom_point(size = point_size, alpha = point_alpha) +
        ggplot2::scale_color_manual(
          values = sig_colors,
          name = "",
          guide = ggplot2::guide_legend(override.aes = list(size = 3.4, alpha = 1))
        ) +
        ggplot2::scale_x_log10(labels = scales::label_number()) +
        ggplot2::geom_hline(yintercept = 0, linetype = "dashed",
                            color = "#4A4F57", linewidth = 0.45) +
        ggplot2::labs(
          x = "Mean expression",
          y = expression(log[2]~"Fold Change")
        )

      # Add LOESS line
      if (add_loess && nrow(df) > 10) {
        p = p + ggplot2::geom_smooth(
          method    = "loess",
          se        = FALSE,
          color     = loess_color,
          linewidth = 0.9,
          alpha     = 0.9,
          inherit.aes = FALSE,
          ggplot2::aes(x = .data[["baseMean"]], y = .data[["logFC"]])
        )
      }

      p
    },
    name = "ma"
  )
}
