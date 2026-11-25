# ===========================================================================
# cliomicplot: Cis-pQTL Mendelian Randomization & Colocalization Heatmap
# Clinical & Genomic Figure Archetype 36
# Inspired by Terekhova et al. (Immunity 2026) & UKB-PPP to FinnGen R12
# ===========================================================================

#' Cis-pQTL Mendelian Randomization and Colocalization Heatmap
#'
#' @description
#' Creates publication-ready Mendelian Randomization (MR) and Bayesian
#' colocalization heatmaps (Clinical Archetype 36) for evaluating causal
#' genetic links between molecular exposures (e.g. plasma pQTLs, eQTLs, or
#' single-cell marker signatures) and complex disease endpoints.
#'
#' Evaluates Wald ratio effect sizes (Z-score or beta) as a
#' diverging gradient, overlays odds ratios where p < 0.05, outlines
#' tiles passing multiple-testing thresholds (e.g. Bonferroni), places corner
#' glyphs for robust colocalization (PP.H4 >= 0.80),
#' and mutes discordant associations (PP.H3 >= 0.80, such as MHC/HLA
#' linkage disequilibrium artifacts) in italicized grey text.
#'
#' @param beta_col Column name for MR effect estimate (beta or Wald ratio).
#' @param se_col Column name for standard error of the MR estimate.
#' @param z_col Column name for Z-score (beta / SE). Auto-computed if \code{beta_col} and \code{se_col} are present.
#' @param p_col Column name for association p-value.
#' @param or_col Column name for odds ratio per SD (or effect size for text display).
#' @param pp4_col Column name for colocalization posterior probability of hypothesis 4 (shared causal variant, \eqn{\text{PP.H4}}).
#' @param pp3_col Column name for colocalization posterior probability of hypothesis 3 (distinct causal variants / LD artifact, \eqn{\text{PP.H3}}).
#' @param fdr_col Column name for FDR-adjusted q-value.
#' @param set_col Column name for exposure grouping (e.g. marker protein class: effector vs memory).
#' @param family_col Column name for outcome disease family (e.g. Cardiovascular, Allergic, Autoimmune).
#' @param p_threshold Numeric p-value threshold for displaying numerical effect sizes inside tiles (default 0.05).
#' @param bonferroni Logical or numeric. If \code{TRUE} (default), computes 0.05 / N over all pairs and draws a prominent black border around significant tiles. If numeric, uses the specified threshold. Set to \code{FALSE} to disable.
#' @param coloc_threshold Minimum PP.H4 to display the colocalization corner triangle glyph (default 0.80).
#' @param discordant_threshold Minimum PP.H3 to format text as muted italic grey (default 0.80), flagging distinct variants in the same locus.
#' @param text_size Font size for numbers inside tiles (default 2.0).
#' @param text_format String format for effect numbers (default "\%.2f").
#' @param glyph_shape Point shape for colocalization marker (default 17, solid triangle).
#' @param glyph_size Size of colocalization corner marker (default 1.0).
#' @param glyph_nudge Numeric vector of length 2 specifying x and y nudge for the corner glyph (default \code{c(0.36, 0.30)}).
#' @param colors Diverging color gradient vector of length 3 (low, mid, high). Default \code{c("#2166AC", "white", "#B2182B")}.
#' @param limits Symmetrical color scale limits for \eqn{Z}-score (default \code{c(-6, 6)}). Values beyond are squished.
#' @param fill_name Legend title for color gradient (default \code{"z of Wald ratio"}).
#' @param border_color Grid border color between tiles (default \code{"grey85"}).
#' @param border_linewidth Grid line width between tiles (default 0.25).
#' @param frame_color Outline color for Bonferroni-significant tiles (default \code{"black"}).
#' @param frame_linewidth Outline stroke width for Bonferroni-significant tiles (default 0.55).
#' @param show_caption Logical. If \code{TRUE} (default), appends a standardized publication caption explaining numbers, frames, triangles, and italicized text.
#' @param caption Optional override caption string.
#' @param xlab,ylab Axis labels.
#'
#' @return A \code{cliplot_type} object for use with \code{\link{cliplot}}.
#'
#' @examples
#' \dontrun{
#' library(cliomicplot)
#' data(pqtl_mr_coloc)
#'
#' # Full multi-panel MR & Colocalization matrix
#' cliplot(
#'   symbol ~ label_short,
#'   data = pqtl_mr_coloc,
#'   type = type_mr_coloc(
#'     set_col = "set_label",
#'     family_col = "family_label",
#'     p_threshold = 0.05,
#'     bonferroni = TRUE,
#'     coloc_threshold = 0.80
#'   ),
#'   title = "Cis-pQTL Mendelian Randomization & Colocalization Matrix",
#'   subtitle = "UKB-PPP Plasma Proteomics vs FinnGen R12 Endpoints"
#' )
#' }
#'
#' @export
type_mr_coloc = function(
    beta_col             = NULL,
    se_col               = NULL,
    z_col                = NULL,
    p_col                = NULL,
    or_col               = NULL,
    pp4_col              = NULL,
    pp3_col              = NULL,
    fdr_col              = NULL,
    set_col              = NULL,
    family_col           = NULL,
    p_threshold          = 0.05,
    bonferroni           = TRUE,
    coloc_threshold      = 0.80,
    discordant_threshold = 0.80,
    text_size            = 2.0,
    text_format          = "%.2f",
    glyph_shape          = 17,
    glyph_size           = 1.0,
    glyph_nudge          = c(0.36, 0.30),
    colors               = c("#2166AC", "white", "#B2182B"),
    limits               = c(-6, 6),
    fill_name            = "z of Wald ratio",
    border_color         = "grey85",
    border_linewidth     = 0.25,
    frame_color          = "black",
    frame_linewidth      = 0.55,
    show_caption         = TRUE,
    caption              = NULL,
    xlab                 = NULL,
    ylab                 = NULL
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
        df <- as.data.frame(df)
      }

      nms <- names(df)

      # Resolve x (endpoint) and y (symbol/exposure)
      find_col <- function(candidates, default_val = NULL) {
        m <- intersect(candidates, nms)
        if (length(m) > 0) m[1] else default_val
      }

      x_c <- if (is.character(settings$x) && length(settings$x) == 1 && settings$x %in% nms) {
        settings$x
      } else {
        find_col(c("endpoint_lvl", "label_short", "endpoint", "outcome", "trait", "label", "disease", "x"), nms[1])
      }

      y_c <- if (is.character(settings$y) && length(settings$y) == 1 && settings$y %in% nms) {
        settings$y
      } else {
        find_col(c("symbol_lvl", "symbol", "exposure", "protein", "gene", "feature", "y"), nms[min(2, ncol(df))])
      }

      df$.x <- if (!is.null(settings$x) && length(settings$x) == nrow(df)) as.character(settings$x) else as.character(df[[x_c]])
      df$.y <- if (!is.null(settings$y) && length(settings$y) == nrow(df)) as.character(settings$y) else as.character(df[[y_c]])

      # Resolve effect columns
      zc <- z_col %||% find_col(c("z", "zscore", "z_score", "statistic"))
      bc <- beta_col %||% find_col(c("b_mr", "beta", "b", "estimate", "effect"))
      sec <- se_col %||% find_col(c("se_mr", "se", "stderr", "std_err"))

      if (!is.null(zc) && zc %in% nms) {
        df$.z <- as.numeric(df[[zc]])
      } else if (!is.null(bc) && !is.null(sec) && bc %in% nms && sec %in% nms) {
        df$.z <- as.numeric(df[[bc]]) / as.numeric(df[[sec]])
      } else if (!is.null(bc) && bc %in% nms) {
        df$.z <- as.numeric(df[[bc]])
      } else {
        df$.z <- 0
      }

      # Resolve p-value
      pc <- p_col %||% find_col(c("p_mr", "p", "pval", "pvalue", "p_val"))
      df$.p <- if (!is.null(pc) && pc %in% nms) as.numeric(df[[pc]]) else NA_real_

      # Resolve odds ratio / text number
      orc <- or_col %||% find_col(c("or_per_sd", "or", "odds_ratio", "or_val"))
      if (!is.null(orc) && orc %in% nms) {
        df$.or <- as.numeric(df[[orc]])
      } else if (!is.null(bc) && bc %in% nms) {
        df$.or <- exp(as.numeric(df[[bc]]))
      } else {
        df$.or <- df$.z
      }

      # Resolve colocalization PP.H4 (shared) and PP.H3 (discordant)
      p4c <- pp4_col %||% find_col(c("PP_H4", "pp_h4", "PP.H4", "pp4", "coloc_pp", "h4"))
      df$.pp4 <- if (!is.null(p4c) && p4c %in% nms) as.numeric(df[[p4c]]) else NA_real_

      p3c <- pp3_col %||% find_col(c("PP_H3", "pp_h3", "PP.H3", "pp3", "h3"))
      df$.pp3 <- if (!is.null(p3c) && p3c %in% nms) as.numeric(df[[p3c]]) else NA_real_

      # Resolve groupings for faceting
      sc <- set_col %||% find_col(c("set_label", "set", "exposure_group", "category", "class"))
      df$.set <- if (!is.null(sc) && sc %in% nms) factor(df[[sc]]) else NULL

      fc <- family_col %||% find_col(c("family_label", "family", "outcome_family", "disease_family", "group"))
      df$.family <- if (!is.null(fc) && fc %in% nms) factor(df[[fc]]) else NULL

      # Bonferroni threshold calculation
      bonf_thr <- if (isTRUE(bonferroni)) {
        0.05 / max(1, nrow(df))
      } else if (is.numeric(bonferroni)) {
        bonferroni
      } else {
        -1
      }

      # Status flags for rendering layers
      df$.show_text <- !is.na(df$.p) & df$.p < p_threshold
      df$.show_frame <- !is.na(df$.p) & df$.p < bonf_thr
      df$.show_coloc <- !is.na(df$.pp4) & df$.pp4 >= coloc_threshold
      df$.show_discordant <- df$.show_text & !is.na(df$.pp3) & df$.pp3 >= discordant_threshold
      df$.text_label <- sprintf(text_format, df$.or)

      # Level factors
      if (!is.factor(df$.x)) df$.x <- factor(df$.x, levels = unique(df$.x))
      if (!is.factor(df$.y)) df$.y <- factor(df$.y, levels = rev(unique(df$.y)))

      settings$mr_coloc_df <- df
      settings$mr_bonf_thr <- bonf_thr
      settings$skip_postprocess <- TRUE
    },

    draw = function(data, mapping, settings, ...) {
      df <- settings$mr_coloc_df
      if (is.null(df)) return(ggplot2::ggplot())

      p <- ggplot2::ggplot(df, ggplot2::aes(x = .data[[".x"]], y = .data[[".y"]])) +
        ggplot2::geom_tile(
          ggplot2::aes(fill = .data[[".z"]]),
          colour = border_color,
          linewidth = border_linewidth
        )

      # Overlay Bonferroni significant tile outlines
      if (any(df$.show_frame, na.rm = TRUE)) {
        p <- p + ggplot2::geom_tile(
          data = df[df$.show_frame == TRUE, , drop = FALSE],
          fill = NA,
          colour = frame_color,
          linewidth = frame_linewidth
        )
      }

      # Overlay black numbers for significant pairs
      black_text <- df$.show_text == TRUE & df$.show_discordant == FALSE
      if (any(black_text, na.rm = TRUE)) {
        p <- p + ggplot2::geom_text(
          data = df[black_text, , drop = FALSE],
          ggplot2::aes(label = .data[[".text_label"]]),
          size = text_size,
          colour = "black"
        )
      }

      # Overlay grey italic numbers for discordant colocalization (PP.H3 >= 0.80)
      if (any(df$.show_discordant, na.rm = TRUE)) {
        p <- p + ggplot2::geom_text(
          data = df[df$.show_discordant == TRUE, , drop = FALSE],
          ggplot2::aes(label = .data[[".text_label"]]),
          size = text_size,
          colour = "grey45",
          fontface = "italic"
        )
      }

      # Overlay colocalization corner marker glyph (PP.H4 >= 0.80)
      if (any(df$.show_coloc, na.rm = TRUE)) {
        p <- p + ggplot2::geom_point(
          data = df[df$.show_coloc == TRUE, , drop = FALSE],
          shape = glyph_shape,
          size = glyph_size,
          colour = "black",
          position = ggplot2::position_nudge(x = glyph_nudge[1], y = glyph_nudge[2])
        )
      }

      # Color scale
      p <- p + ggplot2::scale_fill_gradient2(
        low = colors[1],
        mid = colors[2],
        high = colors[3],
        midpoint = 0,
        limits = limits,
        oob = scales::squish,
        name = fill_name
      )

      # Faceting
      has_set <- !is.null(df$.set)
      has_fam <- !is.null(df$.family)
      if (has_set && has_fam) {
        p <- p + ggplot2::facet_grid(.set ~ .family, scales = "free", space = "free")
      } else if (has_set) {
        p <- p + ggplot2::facet_grid(.set ~ ., scales = "free_y", space = "free_y")
      } else if (has_fam) {
        p <- p + ggplot2::facet_grid(. ~ .family, scales = "free_x", space = "free_x")
      }

      # Standard publication caption
      cap_str <- if (!is.null(caption)) {
        caption
      } else if (isTRUE(show_caption)) {
        paste0(
          "Number = odds ratio per SD of genetically higher plasma protein (p < ", p_threshold, ").\n",
          if (settings$mr_bonf_thr > 0) paste0("Black frame: p < ", sprintf("%.2e", settings$mr_bonf_thr), " (Bonferroni threshold). ") else "",
          "▲ = Colocalization with shared causal variant (PP.H4 >= ", coloc_threshold, ").\n",
          "Grey italics = Distinct causal variants / LD artifact (PP.H3 >= ", discordant_threshold, ")."
        )
      } else {
        settings$caption
      }

      p <- p + ggplot2::labs(
        title    = settings$title,
        subtitle = settings$subtitle,
        caption  = cap_str,
        x        = xlab %||% settings$xlab %||% "",
        y        = ylab %||% settings$ylab %||% ""
      )

      p <- p + resolve_theme(settings$theme %||% "minimal") +
        ggplot2::theme(
          axis.text.x = ggplot2::element_text(angle = 40, hjust = 1, colour = "black"),
          axis.text.y = ggplot2::element_text(colour = "black", face = "italic"),
          strip.text = ggplot2::element_text(face = "bold"),
          strip.text.y = ggplot2::element_text(angle = 0, hjust = 0),
          panel.grid = ggplot2::element_blank(),
          plot.caption = ggplot2::element_text(hjust = 0, size = 6.2, lineheight = 1.2),
          legend.position = "bottom",
          legend.key.height = grid::unit(2.5, "mm"),
          legend.key.width = grid::unit(14, "mm")
        )

      p
    },
    name = "mr_coloc"
  )
}
