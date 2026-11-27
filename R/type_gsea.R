# ===========================================================================
# cliomicplot: GSEA-style Enrichment Dot Plot & Running Score Types
# Inspired by Subramanian et al. PNAS 2005 & cnsplots gseaplot()
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

#' Compute GSEA Running Enrichment Score Walk
#'
#' @description
#' Calculates the running enrichment score walk, peak enrichment score (ES),
#' and leading-edge subset for a target gene set given a pre-ranked list of gene
#' statistics, matching the Subramanian et al. (PNAS 2005) algorithm.
#'
#' @param stats Named numeric vector of gene statistics (e.g. log2FC or t-statistic),
#'   sorted in descending order, or a data frame with gene identifier and score columns.
#' @param gene_set Character vector of gene symbols belonging to the gene set of interest.
#' @param p Numeric exponent weight for ranking metric (default 1.0, standard GSEA weighting).
#'
#' @return A data frame containing:
#'   \itemize{
#'     \item \code{rank}: Integer gene rank from 1 to N.
#'     \item \code{gene}: Gene identifier symbol.
#'     \item \code{stat}: Ranking statistic value.
#'     \item \code{in_set}: Logical indicating membership in the gene set.
#'     \item \code{running_es}: Running enrichment score at each rank.
#'     \item \code{is_peak}: Logical indicating the maximum deviation peak rank.
#'     \item \code{is_leading_edge}: Logical indicating genes within the leading edge subset.
#'   }
#'   With attributes \code{es} (peak enrichment score), \code{peak_rank}, and \code{set_size}.
#'
#' @examples
#' set.seed(42)
#' all_genes <- paste0("Gene_", 1:1000)
#' stats <- sort(rnorm(1000, 0, 2), decreasing = TRUE)
#' names(stats) <- all_genes
#' pathway_genes <- paste0("Gene_", sample(1:200, 30))
#' walk_df <- cli_calc_gsea_running(stats, pathway_genes)
#' head(walk_df)
#'
#' @export
cli_calc_gsea_running = function(stats, gene_set, p = 1.0) {
  if (is.data.frame(stats)) {
    gene_col <- .detect_col(stats, c("gene", "symbol", "id", "feature", "name"), "character")
    stat_col <- .detect_col(stats, c("stat", "log2fc", "logfc", "t", "score", "metric"), "numeric")
    if (is.null(gene_col) || is.null(stat_col)) {
      stop("cli_calc_gsea_running requires gene and statistic columns in data frame.", call. = FALSE)
    }
    vec <- stats[[stat_col]]
    names(vec) <- stats[[gene_col]]
    stats <- vec
  }

  stats <- sort(stats, decreasing = TRUE)
  genes <- names(stats)
  N <- length(stats)
  hits <- genes %in% gene_set
  N_H <- sum(hits)

  if (N_H == 0) {
    warning("None of the gene set members were identified in the ranked statistics.", call. = FALSE)
  }

  weights <- if (p == 0) rep(1, N) else abs(stats)^p
  N_R <- sum(weights[hits])
  if (N_R == 0) N_R <- 1

  p_hit <- cumsum(ifelse(hits, weights / N_R, 0))
  p_miss <- cumsum(ifelse(!hits, 1 / max(1, (N - N_H)), 0))
  running_es <- p_hit - p_miss

  max_dev_idx <- which.max(abs(running_es))
  peak_es <- running_es[max_dev_idx]

  is_leading <- if (peak_es >= 0) {
    (seq_len(N) <= max_dev_idx) & hits
  } else {
    (seq_len(N) >= max_dev_idx) & hits
  }

  out <- data.frame(
    rank            = seq_len(N),
    gene            = genes,
    stat            = as.numeric(stats),
    in_set          = hits,
    running_es      = running_es,
    is_peak         = seq_len(N) == max_dev_idx,
    is_leading_edge = is_leading,
    stringsAsFactors = FALSE
  )
  attr(out, "es") <- peak_es
  attr(out, "peak_rank") <- max_dev_idx
  attr(out, "set_size") <- N_H
  out
}

#' GSEA Running Enrichment Score Composite Plot Type
#'
#' @description
#' Creates the publication-standard 3-tier GSEA running enrichment score plot
#' (Subramanian et al., PNAS 2005) featuring:
#' 1. Running Enrichment Score curve with horizontal zero line and peak ES marker.
#' 2. Barcode rug plot indicating positions of gene set members in the ranked list.
#' 3. Ranked list metric gradient area (e.g. t-statistic or log2FC).
#'
#' @param line_col Line color for the running ES curve (default \code{"#1B7837"}).
#' @param line_width Thickness of the running ES curve line (default 1.1).
#' @param barcode_col Color of barcode vertical tick marks for gene hits (default \code{"#182033"}).
#' @param show_metric Logical; if TRUE, shows the lower panel with ranked metric values (default TRUE).
#' @param peak_color Color for the peak ES marker point (default \code{"#D73027"}).
#' @param peak_size Size for the peak ES marker point (default 2.8).
#' @param pathway_name Optional title or pathway label displayed in the plot header.
#' @param nes Optional pre-calculated Normalized Enrichment Score to display in annotation.
#' @param fdr Optional pre-calculated FDR value to display in annotation.
#' @param heights Numeric vector of relative heights for the composite panels (default \code{c(3.0, 0.7, 1.3)}).
#'
#' @return A \code{cliplot_type} object or composite ggplot object.
#' @export
type_gsea_running = function(
    line_col        = "#1B7837",
    line_width      = 1.1,
    barcode_col     = "#182033",
    show_metric     = TRUE,
    peak_color      = "#D73027",
    peak_size       = 2.8,
    pathway_name    = NULL,
    nes             = NULL,
    fdr             = NULL,
    heights         = c(3.0, 0.7, 1.3)
) {
  cliplot_type(
    data = function(settings, ...) {
      df <- settings$data
      if (is.null(df) && is.data.frame(settings$x)) {
        df <- settings$x
      }
      if (is.null(df)) {
        stop("type_gsea_running requires a data frame from cli_calc_gsea_running().", call. = FALSE)
      }

      # Detect essential columns
      rank_col <- .detect_col(df, c("rank", "index", "order"), "numeric")
      es_col   <- .detect_col(df, c("running_es", "es", "runninges", "score"), "numeric")
      hit_col  <- .detect_col(df, c("in_set", "hit", "in_pathway", "member"), "logical")
      stat_col <- .detect_col(df, c("stat", "log2fc", "metric", "t"), "numeric")

      if (is.null(rank_col) || is.null(es_col)) {
        stop("type_gsea_running requires 'rank' and 'running_es' columns.", call. = FALSE)
      }

      plot_df <- data.frame(
        rank       = df[[rank_col]],
        running_es = df[[es_col]],
        in_set     = if (!is.null(hit_col)) as.logical(df[[hit_col]]) else rep(FALSE, nrow(df)),
        stat       = if (!is.null(stat_col)) as.numeric(df[[stat_col]]) else rep(0, nrow(df)),
        stringsAsFactors = FALSE
      )

      # Store peak info
      peak_idx <- which.max(abs(plot_df$running_es))
      peak_r   <- plot_df$rank[peak_idx]
      peak_val <- plot_df$running_es[peak_idx]

      settings$gs_df           <- plot_df
      settings$gs_peak_rank    <- peak_r
      settings$gs_peak_es      <- peak_val
      settings$gs_line_col     <- line_col
      settings$gs_line_width   <- line_width
      settings$gs_barcode_col  <- barcode_col
      settings$gs_show_metric  <- show_metric
      settings$gs_peak_col     <- peak_color
      settings$gs_peak_sz      <- peak_size
      settings$gs_pathway      <- pathway_name
      settings$gs_nes          <- nes
      settings$gs_fdr          <- fdr
      settings$gs_heights      <- heights
    },
    draw = function(data, mapping, settings, ...) {
      df <- settings$gs_df
      if (is.null(df)) return(ggplot2::ggplot())

      max_r <- max(df$rank, na.rm = TRUE)

      # 1. Top Panel: Running Enrichment Score curve
      p_es <- ggplot2::ggplot(df, ggplot2::aes(x = .data[["rank"]], y = .data[["running_es"]])) +
        ggplot2::geom_hline(yintercept = 0, linetype = "dashed", color = "grey55", linewidth = 0.4) +
        ggplot2::geom_line(color = settings$gs_line_col, linewidth = settings$gs_line_width) +
        ggplot2::geom_point(
          data = data.frame(rank = settings$gs_peak_rank, running_es = settings$gs_peak_es),
          ggplot2::aes(x = .data[["rank"]], y = .data[["running_es"]]),
          color = settings$gs_peak_col, size = settings$gs_peak_sz
        ) +
        ggplot2::scale_x_continuous(expand = c(0.01, 0)) +
        theme_cli_base() +
        ggplot2::theme(
          axis.title.x = ggplot2::element_blank(),
          axis.text.x  = ggplot2::element_blank(),
          axis.ticks.x = ggplot2::element_blank(),
          panel.grid.minor = ggplot2::element_blank()
        ) +
        ggplot2::labs(
          y = "Enrichment\nScore (ES)",
          title = settings$gs_pathway
        )

      # Annotate ES / NES / FDR if available
      anno_text <- paste0(
        "Peak ES: ", round(settings$gs_peak_es, 3),
        if (!is.null(settings$gs_nes)) paste0("\nNES: ", round(settings$gs_nes, 2)) else "",
        if (!is.null(settings$gs_fdr)) paste0("\nFDR: ", format(settings$gs_fdr, digits = 2)) else ""
      )
      y_pos <- if (settings$gs_peak_es >= 0) max(df$running_es) * 0.9 else min(df$running_es) * 0.9
      p_es <- p_es + ggplot2::annotate(
        "text",
        x = max_r * 0.82,
        y = y_pos,
        label = anno_text,
        size = 3.0,
        fontface = "bold",
        hjust = 0,
        color = "#182033"
      )

      # 2. Middle Panel: Barcode rug ticks
      hits_df <- df[df$in_set, , drop = FALSE]
      p_barcode <- ggplot2::ggplot(hits_df, ggplot2::aes(x = .data[["rank"]])) +
        ggplot2::geom_segment(
          ggplot2::aes(x = .data[["rank"]], xend = .data[["rank"]], y = 0, yend = 1),
          color = settings$gs_barcode_col, linewidth = 0.4, alpha = 0.85
        ) +
        ggplot2::scale_x_continuous(limits = c(1, max_r), expand = c(0.01, 0)) +
        ggplot2::scale_y_continuous(expand = c(0, 0)) +
        theme_cli_base() +
        ggplot2::theme(
          axis.title   = ggplot2::element_blank(),
          axis.text    = ggplot2::element_blank(),
          axis.ticks   = ggplot2::element_blank(),
          panel.grid   = ggplot2::element_blank(),
          panel.border = ggplot2::element_rect(color = "grey75", fill = NA, linewidth = 0.4)
        )

      if (!isTRUE(settings$gs_show_metric)) {
        if (requireNamespace("patchwork", quietly = TRUE)) {
          return(patchwork::wrap_plots(p_es, p_barcode, ncol = 1, heights = c(3.0, 0.8)))
        } else {
          return(p_es)
        }
      }

      # 3. Bottom Panel: Ranked Metric Gradient area
      p_metric <- ggplot2::ggplot(df, ggplot2::aes(x = .data[["rank"]], y = .data[["stat"]])) +
        ggplot2::geom_area(ggplot2::aes(fill = .data[["stat"]]), alpha = 0.85) +
        ggplot2::geom_hline(yintercept = 0, color = "grey40", linewidth = 0.3) +
        ggplot2::scale_fill_gradient2(
          low = "#2166AC", mid = "#F7F7F7", high = "#B2182B", midpoint = 0, guide = "none"
        ) +
        ggplot2::scale_x_continuous(expand = c(0.01, 0)) +
        theme_cli_base() +
        ggplot2::theme(
          panel.grid.minor = ggplot2::element_blank()
        ) +
        ggplot2::labs(
          x = "Rank in Ordered Dataset",
          y = "Ranked\nMetric"
        )

      if (requireNamespace("patchwork", quietly = TRUE)) {
        patchwork::wrap_plots(p_es, p_barcode, p_metric, ncol = 1, heights = settings$gs_heights)
      } else {
        p_es
      }
    },
    name = "gsea_running"
  )
}
