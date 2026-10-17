# ===========================================================================
# cliomicplot: GSEA-style Enrichment Dot Plot Type
# Inspired by cnsplots gseaplot() - pathway/term enrichment visualization
# ===========================================================================

# Internal helpers -----------------------------------------------------------

# Find a column by preferred names, then by type
.detect_col = function(df, patterns, col_type = "numeric") {
  for (pat in patterns) {
    hit = grep(pat, names(df), ignore.case = TRUE, value = TRUE)
    if (length(hit) > 0) return(hit[1])
  }
  idx = which(vapply(df, function(col) {
    if (col_type == "character") {
      is.character(col) || is.factor(col)
    } else {
      is.numeric(col)
    }
  }, logical(1)))
  if (length(idx) > 0) return(names(df)[idx[1]])
  NULL
}

#' GSEA-Style Enrichment Dot Plot Type
#'
#' @description Creates publication-ready gene-set enrichment dot plots
#'   (GSEA-style) from prerank-style result tables. Terms are ranked along the
#'   y-axis, the enrichment score (e.g. NES) on the x-axis, dot size encodes
#'   statistical significance (-log10 FDR) and dot color encodes the direction
#'   of enrichment. Inspired by cnsplots' \code{gseaplot()}.
#'
#' @param top_term Maximum number of terms to display (default 20). Terms are
#'   ranked by absolute enrichment score, then by FDR.
#' @param term_col Optional name of the term/pathway column. Auto-detected.
#' @param nes_col Optional name of the enrichment-score column. Auto-detected
#'   from names like \code{NES}, \code{Enrichment Score}, \code{ES}.
#' @param fdr_col Optional name of the FDR/q-value column. Auto-detected from
#'   names like \code{FDR q-val}, \code{FDR}, \code{qvalue}, \code{padj},
#'   \code{Adj.P.Value}.
#' @param dot_size_range Range of dot sizes (default \code{c(2, 7)}).
#' @param nes_colors Diverging colors for negative/zero/positive enrichment
#'   (default blue-white-red).
#' @param show_fdr_legend Show the size legend for -log10(FDR) (default TRUE).
#' @param point_alpha Transparency of dots (default 0.9).
#'
#' @return A \code{cliplot_type} object for use with \code{\link{cliplot}}.
#'
#' @details
#' Expects a data frame with one row per pathway/term and at least an
#' enrichment-score column and an FDR/q-value column. Use the formula
#' interface \code{cliplot(NES ~ Term, data = gsea_res, type = "gsea")} or pass
#' the data frame directly. Column names are auto-detected, so results from
#' common tools (fgsea, clusterProfiler, GSEA desktop) work out of the box.
#'
#' @examples
#' \dontrun{
#' # Typical prerank-style GSEA results
#' cliplot(gsea_res, type = "gsea", theme = "nature")
#'
#' # Formula interface
#' cliplot(NES ~ Clean_Term, data = gsea_res, type = "gsea")
#' }
#'
#' @export
type_gsea = function(
    top_term       = 20,
    term_col       = NULL,
    nes_col        = NULL,
    fdr_col        = NULL,
    dot_size_range = c(2, 7),
    nes_colors     = c("#2166AC", "#F7F7F7", "#B2182B"),
    show_fdr_legend = TRUE,
    point_alpha    = 0.9
) {
  cliplot_type(
    data = function(settings, ...) {
      df = settings$data
      if (is.null(df) && !is.null(settings$x) && is.data.frame(settings$x)) {
        df = settings$x
      }
      if (is.null(df)) {
        stop("type_gsea requires a data frame of enrichment results.",
             call. = FALSE)
      }
      df = as.data.frame(df)

      # Term column
      term = term_col
      if (is.null(term)) {
        term = .detect_col(df,
          c("Clean_Term", "term", "pathway", "description", "ID", "name",
            "test", "Category"),
          "character")
      }
      # Formula overrides: x = term vector
      if (!is.null(settings$x) &&
          (is.character(settings$x) || is.factor(settings$x))) {
        term = NULL
      }

      # NES column
      nes = nes_col
      if (is.null(nes)) {
        nes = .detect_col(df,
          c("NES", "enrichment", "score", "ES", "logFC", "log2FoldChange"),
          "numeric")
      }
      # FDR column
      fdr = fdr_col
      if (is.null(fdr)) {
        fdr = .detect_col(df,
          c("FDR q-val", "FDR", "qvalue", "q_val", "q-value", "padj",
            "Adj.P.Value", "p.adjust", "P.Value", "pvalue", "p.value"),
          "numeric")
      }
      if (is.null(term) && is.null(nes)) {
        stop("type_gsea could not find a term column and an enrichment-score ",
             "column. Provide `term_col` / `nes_col`.", call. = FALSE)
      }

      out = data.frame(
        term = if (!is.null(term)) df[[term]] else settings$x,
        stringsAsFactors = FALSE
      )
      if (!is.null(nes)) {
        out$nes = df[[nes]]
      } else if (!is.null(settings$y)) {
        out$nes = settings$y
      }
      if (is.null(out$nes)) {
        # Fall back to any numeric column if no explicit NES detected
        num_idx = which(vapply(df, is.numeric, logical(1)))
        if (length(num_idx) > 0) out$nes = df[[num_idx[1]]]
      }
      if (is.null(fdr)) {
        warning("No FDR/q-value column detected; dot size will be constant.",
                call. = FALSE)
        out$fdr = rep(0.05, nrow(out))
      } else {
        out$fdr = as.numeric(df[[fdr]])
      }
      out = stats::na.omit(out)
      if (nrow(out) == 0) {
        stop("No complete rows in the enrichment results.", call. = FALSE)
      }

      # Rank terms: by |NES| desc, then by FDR asc
      out$rank = order(-abs(out$nes), out$fdr)
      out = out[order(out$rank), ]
      if (nrow(out) > top_term) out = out[seq_len(top_term), ]

      settings$gsea_data = out
      settings$dot_size_range = dot_size_range
      settings$nes_colors     = nes_colors
      settings$show_fdr_legend = show_fdr_legend
      settings$point_alpha    = point_alpha
    },
    draw = function(data, mapping, settings, ...) {
      g = settings$gsea_data
      if (is.null(g)) return(ggplot2::ggplot())

      g$term = factor(g$term, levels = rev(unique(g$term)))
      g$neglogfdr = -log10(pmax(g$fdr, 1e-300))

      p = ggplot2::ggplot(g, ggplot2::aes(
        x = .data[["nes"]],
        y = .data[["term"]]
      )) +
        ggplot2::geom_vline(
          xintercept = 0, linetype = "dashed",
          color = "grey55", linewidth = 0.4
        ) +
        ggplot2::geom_point(
          ggplot2::aes(size = .data[["neglogfdr"]], fill = .data[["nes"]]),
          shape = 21, color = "grey25", stroke = 0.35,
          alpha = settings$point_alpha
        ) +
        ggplot2::scale_fill_gradient2(
          low   = settings$nes_colors[1],
          mid   = settings$nes_colors[2],
          high  = settings$nes_colors[3],
          midpoint = 0,
          name = "Enrichment\nScore"
        ) +
        ggplot2::scale_size_continuous(
          range = settings$dot_size_range,
          name = "-log10(FDR)"
        ) +
        ggplot2::labs(
          x = "Enrichment Score (NES)",
          y = NULL
        )

      if (!isTRUE(settings$show_fdr_legend)) {
        p = p + ggplot2::guides(size = "none")
      }

      p
    },
    name = "gsea"
  )
}

