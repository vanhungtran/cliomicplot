# ===========================================================================
# cliomicplot: Manhattan Plot (GWAS and Microbial mGWAS)
# Clinical & Genomic Figure Archetype 27
# ===========================================================================

#' Manhattan Plot for Genome-Wide and Microbial Association Studies
#'
#' @description
#' Creates publication-ready Manhattan plots for host GWAS and microbial
#' genome-wide association studies (mGWAS). Supports multi-chromosome genomic
#' layouts with alternating chromosome hues and centered axis labels, as well as
#' single-contig microbial chromosomes. Includes customizable significance
#' threshold lines, hit highlighting, and automatic gene/variant labeling.
#'
#' @param threshold Significance cutoff. For raw p-values or q-values, defaults
#'   to \code{0.05} (or \code{5e-8} for genome-wide significance). For data
#'   already on the -log10 scale, specifies the vertical line position.
#' @param is_already_log10 Logical. If TRUE, the y-axis values are assumed to
#'   already be \eqn{-\log_{10}(p)} or \eqn{-\log_{10}(q)}. Default FALSE.
#' @param point_size Size of variant points (default 1.2).
#' @param point_alpha Opacity of variant points (default 0.85).
#' @param chr_colors Character vector of two alternating colors for chromosomes
#'   (default \code{c("#2B5C8F", "#82B0D2")}).
#' @param highlight_color Color for variants exceeding the significance
#'   threshold (default \code{"#E64B35"}).
#' @param threshold_color Color for the horizontal significance line (default
#'   \code{"#888888"}).
#' @param threshold_linetype Line type for the threshold line (default \code{"dashed"}).
#' @param label_hits Logical. If TRUE, annotates significant variants exceeding
#'   the threshold with text labels. Default TRUE.
#' @param label_col Column name or character vector containing variant or gene
#'   labels. If NULL, auto-detects columns named "label", "gene", "snp", or "hit".
#' @param max_overlaps Maximum overlapping labels allowed by ggrepel (default 25).
#' @param xlab Label for x-axis. Defaults to "Chromosome" for multi-chromosome
#'   or "Chromosome position (bp)" for single-contig genomes.
#' @param ylab Label for y-axis (default \code{expression(-log[10]~italic(p))}).
#'
#' @return A \code{cliplot_type} object for use with \code{\link{cliplot}}.
#'
#' @examples
#' \dontrun{
#' # Simulated microbial GWAS data
#' set.seed(42)
#' mgwas_df <- data.frame(
#'   pos = seq(1000, 500000, by = 5000),
#'   qvalue = c(runif(95, 0.05, 1.0), runif(5, 1e-5, 0.01)),
#'   gene = c(rep("", 95), paste0("mut_", 1:5))
#' )
#'
#' cliplot(qvalue ~ pos, data = mgwas_df,
#'         type = type_manhattan(threshold = 0.05, label_col = "gene"))
#' }
#'
#' @export
type_manhattan = function(
    threshold          = 0.05,
    is_already_log10   = FALSE,
    point_size         = 1.2,
    point_alpha        = 0.85,
    chr_colors         = c("#2B5C8F", "#82B0D2"),
    highlight_color    = "#E64B35",
    threshold_color    = "#888888",
    threshold_linetype = "dashed",
    label_hits         = TRUE,
    label_col          = NULL,
    max_overlaps       = 25,
    xlab               = NULL,
    ylab               = NULL
) {
  cliplot_type(
    data = function(settings, ...) {
      df <- settings$data
      if (is.null(df)) {
        df <- data.frame(
          x = as.numeric(settings$x),
          y = as.numeric(settings$y),
          stringsAsFactors = FALSE
        )
        if (!is.null(settings$by)) df$chr <- as.character(settings$by)
      } else {
        # Determine x, y, chr from settings
        nms <- names(df)
        x_col <- if (is.character(settings$x) && length(settings$x) == 1 && settings$x %in% nms) {
          settings$x
        } else {
          intersect(c("pos", "bp", "position", "BP", "POS", "Position"), nms)[1] %||% nms[1]
        }

        y_col <- if (is.character(settings$y) && length(settings$y) == 1 && settings$y %in% nms) {
          settings$y
        } else {
          intersect(c("pvalue", "p", "qvalue", "q", "P", "pval", "padj", "PVAL"), nms)[1] %||% nms[2]
        }

        df$.pos <- as.numeric(if (!is.null(settings$x) && !is.character(settings$x)) settings$x else df[[x_col]])
        df$.raw_y <- as.numeric(if (!is.null(settings$y) && !is.character(settings$y)) settings$y else df[[y_col]])

        chr_col <- if (!is.null(settings$by)) {
          if (is.character(settings$by) && length(settings$by) == 1 && settings$by %in% nms) settings$by else NULL
        } else {
          cand_chr <- intersect(c("chr", "CHR", "chromosome", "Chromosome", "contig"), nms)
          if (length(cand_chr) > 0) cand_chr[1] else NULL
        }

        if (!is.null(chr_col) && chr_col %in% nms) {
          df$.chr <- as.character(df[[chr_col]])
        } else if (!is.null(settings$by)) {
          df$.chr <- as.character(settings$by)
        } else {
          df$.chr <- rep("1", nrow(df))
        }
      }

      # Compute -log10 y
      if (isTRUE(is_already_log10)) {
        df$.l10y <- df$.raw_y
        log_thresh <- as.numeric(threshold)
      } else {
        df$.l10y <- -log10(pmax(df$.raw_y, .Machine$double.xmin))
        log_thresh <- -log10(pmax(as.numeric(threshold), .Machine$double.xmin))
      }

      df$.sig <- df$.l10y >= log_thresh

      # Label detection
      lbl <- NULL
      if (!is.null(label_col) && label_col %in% names(df)) {
        lbl <- as.character(df[[label_col]])
      } else {
        cand <- intersect(c("label", "gene", "Gene", "snp", "SNP", "hit", "Hit", "Preferred_name"), names(df))
        if (length(cand) > 0) lbl <- as.character(df[[cand[1]]])
      }
      df$.label <- if (!is.null(lbl)) lbl else ""

      # Chromosome layout and cumulative position calculation
      chrs <- unique(df$.chr)
      is_multichr <- length(chrs) > 1

      if (is_multichr) {
        # Natural order chromosomes (1, 2, ..., 22, X, Y, MT)
        chr_num <- suppressWarnings(as.numeric(chrs))
        chr_order <- order(ifelse(is.na(chr_num), 1000 + seq_along(chrs), chr_num))
        ordered_chrs <- chrs[chr_order]

        # Calculate chromosome lengths and cumulative offsets
        chr_lengths <- vapply(ordered_chrs, function(c) {
          sub_pos <- df$.pos[df$.chr == c]
          if (length(sub_pos) == 0) 0 else max(sub_pos, na.rm = TRUE)
        }, numeric(1))

        cum_offsets <- c(0, cumsum(as.numeric(chr_lengths[-length(chr_lengths)])))
        names(cum_offsets) <- ordered_chrs

        df$.cum_pos <- df$.pos + cum_offsets[df$.chr]

        chr_ticks <- cum_offsets + (chr_lengths / 2)
        chr_color_idx <- (match(df$.chr, ordered_chrs) %% 2 == 1)

        settings$manhattan_ticks <- chr_ticks
        settings$manhattan_tick_labels <- ordered_chrs
        settings$manhattan_chr_color_idx <- chr_color_idx
      } else {
        df$.cum_pos <- df$.pos
        settings$manhattan_ticks <- NULL
        settings$manhattan_tick_labels <- NULL
        settings$manhattan_chr_color_idx <- rep(TRUE, nrow(df))
      }

      settings$manhattan_df <- df
      settings$manhattan_log_thresh <- log_thresh
      settings$manhattan_is_multichr <- is_multichr
    },

    draw = function(data, mapping, settings, ...) {
      df <- settings$manhattan_df
      if (is.null(df)) return(ggplot2::ggplot())

      log_thresh <- settings$manhattan_log_thresh
      is_multichr <- settings$manhattan_is_multichr
      c_colors <- rep_len(chr_colors, 2)

      # Color assignment: alternating chromosome colors, or highlight if significant
      df$.point_color <- ifelse(
        df$.sig,
        highlight_color,
        ifelse(settings$manhattan_chr_color_idx, c_colors[1], c_colors[2])
      )

      p <- ggplot2::ggplot(df, ggplot2::aes(x = .data[[".cum_pos"]], y = .data[[".l10y"]])) +
        ggplot2::geom_point(ggplot2::aes(color = I(.data[[".point_color"]])),
                            size = point_size, alpha = point_alpha) +
        ggplot2::geom_hline(yintercept = log_thresh,
                            linetype = threshold_linetype,
                            color = threshold_color,
                            linewidth = 0.5)

      # Axis configuration
      if (is_multichr && !is.null(settings$manhattan_ticks)) {
        p <- p + ggplot2::scale_x_continuous(
          breaks = settings$manhattan_ticks,
          labels = settings$manhattan_tick_labels,
          expand = ggplot2::expansion(mult = c(0.01, 0.01))
        )
      } else {
        p <- p + ggplot2::scale_x_continuous(
          labels = scales::label_number(scale = 1e-6, suffix = " Mb"),
          expand = ggplot2::expansion(mult = c(0.02, 0.02))
        )
      }

      p <- p + ggplot2::labs(
        x = xlab %||% (if (is_multichr) "Chromosome" else "Chromosome position"),
        y = ylab %||% expression(-log[10]~italic(p))
      )

      # Text labeling for significant hits
      if (isTRUE(label_hits)) {
        top_hits <- df[df$.sig & nzchar(df$.label), , drop = FALSE]
        if (nrow(top_hits) > 0) {
          if (requireNamespace("ggrepel", quietly = TRUE)) {
            p <- p + ggrepel::geom_text_repel(
              data = top_hits,
              ggplot2::aes(x = .data[[".cum_pos"]], y = .data[[".l10y"]], label = .data[[".label"]]),
              size = 2.8,
              fontface = "italic",
              min.segment.length = 0,
              segment.size = 0.25,
              segment.color = "#555555",
              box.padding = 0.3,
              max.overlaps = max_overlaps
            )
          } else {
            p <- p + ggplot2::geom_text(
              data = top_hits,
              ggplot2::aes(x = .data[[".cum_pos"]], y = .data[[".l10y"]], label = .data[[".label"]]),
              size = 2.8,
              fontface = "italic",
              vjust = -0.5
            )
          }
        }
      }

      p
    },
    name = "manhattan"
  )
}
