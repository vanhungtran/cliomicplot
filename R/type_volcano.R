# ===========================================================================
# cliomicplot: Volcano Plot Type
# For differential expression / omics results visualization
# ===========================================================================

#' Volcano Plot Type
#'
#' @description Creates publication-ready volcano plots for visualizing
#'   differential expression or other omics results. Points are colored by
#'   significance and log2 fold-change thresholds.
#'
#' @param pval_cutoff P-value cutoff for significance (default 0.05)
#' @param fc_cutoff Absolute log2 fold-change cutoff (default 1, i.e. 2-fold)
#' @param point_size Point size (default 2.2)
#' @param point_alpha Point transparency (default 0.72)
#' @param label_genes Character vector of gene names to label, or "significant"
#'   to label all significant genes, or NULL for no labels.
#' @param max_overlaps Maximum overlapping labels for ggrepel (default 15)
#' @param up_color Color for upregulated points (default "red")
#' @param down_color Color for downregulated points (default "blue")
#' @param ns_color Color for non-significant points (default "grey70")
#' @param cutoff_region_alpha Alpha for lightly shaded significant regions.
#' @param show_threshold_labels Add text labels for cutoff lines.
#'
#' @return A cliplot_type object for use with \code{\link{cliplot}}.
#'
#' @details
#' Formula orientation matters. cliplot uses the standard \code{y ~ x}
#' convention, and a volcano plot puts log2 fold-change on the x-axis and
#' significance on the y-axis. Supply the formula as
#' \code{-log10(PValue) ~ logFC} (i.e. \code{y ~ x} with the significance
#' term on the left). Row names of \code{data} are used as gene labels.
#' Use an FDR-adjusted p-value column (e.g. \code{padj}) for the y term when
#' available, so the significance threshold reflects multiple-testing control.
#'
#' @examples
#' \dontrun{
#' # Basic volcano plot (y ~ x: significance ~ fold-change)
#' cliplot(-log10(padj) ~ logFC, data = deg_results, type = "volcano")
#'
#' # With gene labels
#' cliplot(-log10(padj) ~ logFC, data = deg_results,
#'         type = type_volcano(label_genes = "significant"))
#'
#' # Custom cutoffs
#' cliplot(-log10(padj) ~ logFC, data = deg_results,
#'         type = type_volcano(pval_cutoff = 0.01, fc_cutoff = 1.5))
#' }
#'
#' @export
type_volcano = function(
    pval_cutoff  = 0.05,
    fc_cutoff    = 1,
    point_size   = 2.2,
    point_alpha  = 0.72,
    label_genes  = NULL,
    max_overlaps = 15,
    up_color     = "#D73027",
    down_color   = "#2B6CB0",
    ns_color     = "#A8B0BA",
    cutoff_region_alpha = 0.045,
    show_threshold_labels = TRUE
) {
  cliplot_type(
    data = function(settings, ...) {
      # Volcano expects x = log2FC, y = -log10(pvalue)
      # The user provides these via formula: -log10(pval) ~ logFC
      # x = logFC, y = -log10(pval)
      df = settings$data
      if (is.null(df)) {
        # Build from x, y vectors
        df = data.frame(
          logFC    = settings$x,
          negLogP  = settings$y,
          row.names = if (!is.null(names(settings$x))) names(settings$x)
                      else paste0("gene", seq_along(settings$x))
        )
      } else {
        df$logFC   = settings$x
        df$negLogP = settings$y
      }

      # Classify genes
      df$sig = "NS"
      df$sig[df$negLogP > -log10(pval_cutoff) & df$logFC > fc_cutoff]  = "Up"
      df$sig[df$negLogP > -log10(pval_cutoff) & df$logFC < -fc_cutoff] = "Down"

      # Label column
      df$label = ""
      if (!is.null(label_genes)) {
        if (identical(label_genes, "significant")) {
          df$label[df$sig != "NS"] = rownames(df)[df$sig != "NS"]
        } else {
          df$label[rownames(df) %in% label_genes] =
            rownames(df)[rownames(df) %in% label_genes]
        }
      }

      settings$volcano_data  = df
      settings$pval_cutoff   = pval_cutoff
      settings$fc_cutoff     = fc_cutoff
      settings$point_size    = point_size
      settings$point_alpha   = point_alpha
      settings$max_overlaps  = max_overlaps
      settings$up_color      = up_color
      settings$down_color    = down_color
      settings$ns_color      = ns_color
      settings$cutoff_region_alpha = cutoff_region_alpha
      settings$show_threshold_labels = show_threshold_labels
    },
    draw = function(data, mapping, settings, ...) {
      df = settings$volcano_data
      if (is.null(df)) return(ggplot2::ggplot())

      sig_colors = c(
        "Up"   = settings$up_color,
        "Down" = settings$down_color,
        "NS"   = settings$ns_color
      )

      # significance line
      pval_line = -log10(settings$pval_cutoff)

      cutoff_regions = data.frame(
        xmin = c(-Inf, settings$fc_cutoff),
        xmax = c(-settings$fc_cutoff, Inf),
        ymin = pval_line,
        ymax = Inf,
        sig = c("Down", "Up"),
        stringsAsFactors = FALSE
      )

      p = ggplot2::ggplot(df, ggplot2::aes(x = .data[["logFC"]],
                                            y = .data[["negLogP"]])) +
        ggplot2::geom_rect(
          data = cutoff_regions,
          ggplot2::aes(xmin = .data[["xmin"]], xmax = .data[["xmax"]],
                       ymin = .data[["ymin"]], ymax = .data[["ymax"]],
                       fill = .data[["sig"]]),
          inherit.aes = FALSE,
          alpha = settings$cutoff_region_alpha,
          color = NA,
          show.legend = FALSE
        ) +
        ggplot2::geom_point(
          ggplot2::aes(color = .data[["sig"]]),
          size  = settings$point_size,
          alpha = settings$point_alpha
        ) +
        ggplot2::scale_color_manual(
          values = sig_colors,
          breaks = c("Up", "Down", "NS"),
          name = "",
          guide = ggplot2::guide_legend(override.aes = list(size = 3.6, alpha = 1))
        ) +
        ggplot2::scale_fill_manual(values = sig_colors) +
        ggplot2::geom_hline(
          yintercept = pval_line,
          linetype   = "dashed",
          color      = "#4A4F57",
          linewidth  = 0.45
        ) +
        ggplot2::geom_vline(
          xintercept = c(-settings$fc_cutoff, settings$fc_cutoff),
          linetype   = "dashed",
          color      = "#4A4F57",
          linewidth  = 0.45
        ) +
        ggplot2::labs(
          x = expression(log[2]~"Fold Change"),
          y = expression(-log[10]~"(p-value)")
        )

      if (settings$show_threshold_labels) {
        p = p +
          ggplot2::annotate(
            "label",
            x = Inf,
            y = pval_line,
            label = sprintf("p = %.3g", settings$pval_cutoff),
            hjust = 1.05,
            vjust = -0.35,
            size = 3,
            linewidth = 0,
            fill = "white",
            color = "#4A4F57",
            alpha = 0.9
          )
      }

      # Add labels if any
      if (any(df$label != "")) {
        p = p + ggrepel::geom_text_repel(
          ggplot2::aes(label = .data[["label"]]),
          size             = 3.2,
          max.overlaps     = settings$max_overlaps,
          show.legend      = FALSE,
          fontface         = "italic",
          box.padding      = 0.35,
          point.padding    = 0.2,
          min.segment.length = 0,
          segment.color    = "grey55"
        )
      }

      p
    },
    name = "volcano"
  )
}
