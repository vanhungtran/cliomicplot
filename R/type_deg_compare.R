# ===========================================================================
# cliomicplot: DEG Comparison Scatter Type
# Inspired by xOmicsShiny degmodule.R "DEGs in Two Comparisons"
# ===========================================================================

#' DEG Comparison Scatter Type
#'
#' @description Compares differential expression results from two independent
#'   comparisons by plotting their log2 fold-changes against each other.
#'   Inspired by xOmicsShiny's DEG comparison feature.
#'
#' @param comp2_data Data frame containing the second comparison results.
#'   Must have the same gene identifiers as the main data.
#' @param merge_by Column name to merge the two comparisons by (default
#'   auto-detected from row names or "Gene" column).
#' @param add_lm Add linear regression line (default TRUE)
#' @param lm_color Colour for the regression line (default "#3C5488")
#' @param lm_se Add confidence band around regression line (default TRUE)
#' @param sig_color Colour for genes significant in both comparisons
#' @param ns_color Colour for non-significant genes
#' @param point_size Point size (default 2)
#' @param point_alpha Point transparency (default 0.6)
#' @param label_genes Character vector of gene names to label, or NULL
#' @param same_scale Force same x and y axis limits (default TRUE)
#'
#' @return A \code{cliplot_type} object for use with \code{\link{cliplot}}.
#'
#' @details
#' This plot compares fold-change direction and magnitude between two
#' experimental contrasts. It helps identify:
#' - Consistently regulated genes (both up/up or down/down)
#' - Discordantly regulated genes (up in one, down in the other)
#' - Comparison-specific effects
#'
#' @examples
#' \dontrun{
#' # Compare two drug treatments vs control
#' cliplot(logFC ~ comp1_logFC, data = merged_de,
#'         type = type_deg_compare(comp2_data = de_results_2))
#' }
#'
#' @export
type_deg_compare = function(
    comp2_data  = NULL,
    merge_by    = NULL,
    add_lm      = TRUE,
    lm_color    = "#3C5488",
    lm_se       = TRUE,
    sig_color   = "#E64B35",
    ns_color    = "grey60",
    point_size  = 2,
    point_alpha = 0.6,
    label_genes = NULL,
    same_scale  = TRUE
) {
  cliplot_type(
    data = function(settings, ...) {
      df1 = settings$data
      df2 = comp2_data
      if (is.null(df1)) stop("type_deg_compare requires data.", call. = FALSE)
      if (is.null(df2)) stop("comp2_data must be provided.", call. = FALSE)

      # Determine merge column
      merge_col = merge_by
      if (is.null(merge_col)) {
        # Try common ID columns
        for (cn in c("Gene", "Gene.Name", "UniqueID", "gene", "SYMBOL")) {
          if (cn %in% names(df1) && cn %in% names(df2)) {
            merge_col = cn; break
          }
        }
        if (is.null(merge_col)) {
          # Use row names
          df1$..id.. = rownames(df1)
          df2$..id.. = rownames(df2)
          merge_col = "..id.."
        }
      }

      # Get logFC columns — try formula-derived names first, then common patterns
      fc1_col = "logFC"
      fc2_col = "logFC"

      # If x_var/y_var are set, use those names
      y_col = settings$y_var
      x_col = settings$x_var

      # Detect logFC columns by pattern matching
      find_fc_col = function(df, exclude = NULL) {
        for (pat in c("logFC", "log2FoldChange", "log2FC", "log2Fold", "coef", "logFC_")) {
          matches = grep(pat, names(df), value = TRUE, ignore.case = TRUE)
          if (length(matches) > 0) {
            # Return first match not in exclude
            for (m in matches) if (!m %in% exclude) return(m)
          }
        }
        # Last resort: any column with "FC" in name
        matches = grep("FC", names(df), value = TRUE, ignore.case = TRUE)
        if (length(matches) > 0) {
          for (m in matches) if (!m %in% exclude) return(m)
        }
        return(NULL)
      }

      fc1_col = find_fc_col(df1)
      if (!is.null(fc1_col)) {
        # Also detect merge column as x var name if present
        if (!is.null(x_col) && x_col %in% names(df1)) merge_col = x_col
      }
      fc2_col = find_fc_col(df2)

      if (is.null(fc1_col) || is.null(fc2_col)) {
        stop("Cannot identify logFC columns. Ensure columns contain 'logFC' or similar.", call. = FALSE)
      }

      # Also look for p-value columns
      p1_col = NULL; p2_col = NULL
      for (cn in c("padj", "P.Value", "adj.P.Val", "PValue")) {
        if (cn %in% names(df1)) { p1_col = cn; break }
      }
      for (cn in c("padj", "P.Value", "adj.P.Val", "PValue")) {
        if (cn %in% names(df2)) { p2_col = cn; break }
      }

      sub1 = df1[, c(merge_col, fc1_col)]
      if (!is.null(p1_col)) sub1[[p1_col]] = df1[[p1_col]]
      names(sub1)[names(sub1) == fc1_col] = "logFC1"

      sub2 = df2[, c(merge_col, fc2_col)]
      if (!is.null(p2_col)) sub2[[p2_col]] = df2[[p2_col]]
      names(sub2)[names(sub2) == fc2_col] = "logFC2"

      merged = merge(sub1, sub2, by = merge_col, all = FALSE)

      # Classify concordance
      merged$Sig = "NS"
      if (!is.null(p1_col) && !is.null(p2_col)) {
        p_thresh = 0.05
        merged$Sig[merged[[p1_col]] < p_thresh &
                   merged[[p2_col]] < p_thresh] = "Both"
      }

      # Labels
      merged$Label = ""
      if (!is.null(label_genes)) {
        merged$Label[merged[[merge_col]] %in% label_genes] =
          merged[[merge_col]][merged[[merge_col]] %in% label_genes]
      }

      settings$dc_merged     = merged
      settings$dc_add_lm     = add_lm
      settings$dc_lm_color   = lm_color
      settings$dc_lm_se      = lm_se
      settings$dc_sig_color  = sig_color
      settings$dc_ns_color   = ns_color
      settings$dc_point_size = point_size
      settings$dc_point_alpha = point_alpha
      settings$dc_same_scale = same_scale
    },
    draw = function(data, mapping, settings, ...) {
      merged = settings$dc_merged
      if (is.null(merged)) return(ggplot2::ggplot())

      colors = c("Both" = settings$dc_sig_color, "NS" = settings$dc_ns_color)

      lims = if (settings$dc_same_scale) {
        r = range(c(merged$logFC1, merged$logFC2), na.rm = TRUE)
        max_abs = max(abs(r))
        c(-max_abs, max_abs) * 1.05
      } else NULL

      p = ggplot2::ggplot(merged,
        ggplot2::aes(x = .data[["logFC1"]], y = .data[["logFC2"]])) +
        ggplot2::geom_hline(yintercept = 0, linetype = "dashed",
                            color = "grey70", linewidth = 0.35) +
        ggplot2::geom_vline(xintercept = 0, linetype = "dashed",
                            color = "grey70", linewidth = 0.35)

      if (settings$dc_add_lm) {
        p = p + ggplot2::geom_smooth(
          method = "lm", se = settings$dc_lm_se,
          color = settings$dc_lm_color,
          alpha = 0.15,
          linewidth = 0.8
        )
      }

      p = p +
        ggplot2::geom_point(
          data = subset(merged, Sig == "NS"),
          color = settings$dc_ns_color,
          size  = settings$dc_point_size,
          alpha = settings$dc_point_alpha
        ) +
        ggplot2::geom_point(
          data = subset(merged, Sig == "Both"),
          color = settings$dc_sig_color,
          size  = settings$dc_point_size,
          alpha = settings$dc_point_alpha + 0.2
        ) +
        ggplot2::labs(
          x = expression(log[2]~"FC — Comparison 1"),
          y = expression(log[2]~"FC — Comparison 2")
        )

      # Add labels
      if (any(merged$Label != "")) {
        p = p + ggrepel::geom_text_repel(
          data = subset(merged, Label != ""),
          ggplot2::aes(label = .data[["Label"]]),
          size = 3.2,
          max.overlaps = 25,
          fontface = "italic",
          show.legend = FALSE
        )
      }

      # Add concordance stats
      total = nrow(merged)
      both = sum(merged$Sig == "Both")
      concordant = sum(sign(merged$logFC1) == sign(merged$logFC2), na.rm = TRUE)
      p = p +
        ggplot2::labs(
          caption = sprintf(
            "n = %d  |  Concordant direction: %d/%d (%.1f%%)  |  Sig in both: %d",
            total, concordant, total,
            100 * concordant / total, both
          )
        )

      if (!is.null(lims)) {
        p = p + ggplot2::coord_fixed(xlim = lims, ylim = lims)
      }

      p
    },
    name = "deg_compare"
  )
}
