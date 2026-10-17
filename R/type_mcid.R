# ===========================================================================
# cliomicplot: Minimal Clinically Important Difference (MCID) Dumbbell Plot
# Clinical Figure Archetype 1 (clinical and cohort figure standards)
# ===========================================================================

#' Minimal Clinically Important Difference (MCID) Dumbbell Plot Type
#'
#' @description Creates publication-ready MCID dumbbell plots (Clinical
#'   Archetype 1) to distinguish genuine clinical relevance from mere statistical
#'   significance. Connects observed causal or clinical effect sizes to validated
#'   instrument MCID thresholds (e.g., DLQI = 4.0, EASI = 6.6, SCORAD = 8.7),
#'   scaling point glyphs by sample size (N treated) and ordering
#'   endpoints by effect-to-MCID ratio.
#'
#' @param mcid Numeric threshold or column name containing MCID reference values.
#' @param estimate Column name or vector of observed effect estimates.
#' @param n_treated Optional column name or vector of sample sizes to scale point sizes.
#' @param category Optional column name for clinical status classification
#'   (e.g., "SoC exceeding MCID", "SoC below MCID", "Concomitant exceeding MCID").
#' @param sort_by_ratio Sort endpoints by effect-to-MCID ratio (default TRUE).
#' @param segment_width Dumbbell connecting line width (default 1.2).
#' @param mcid_tick_size Size of MCID reference vertical tick (default 3.0).
#' @param point_size_range Range for sample-size-scaled points (default \code{c(3, 7.5)}).
#' @param palette Custom color palette for categories.
#' @param xlab Label for x-axis (default "Absolute Effect Estimate (|estimate|)").
#' @param ylab Label for y-axis (default NULL).
#'
#' @return A cliplot_type object for use with \code{\link{cliplot}}.
#'
#' @examples
#' \dontrun{
#' mcid_df = data.frame(
#'   Endpoint = c("EASI (Dupilumab)", "DLQI (Dupilumab)", "SCORAD (TCS)", "DLQI (Antihistamines)"),
#'   Estimate = c(9.2, 5.8, 4.1, 1.2),
#'   MCID     = c(6.6, 4.0, 8.7, 4.0),
#'   N        = c(420, 395, 210, 150),
#'   Category = c("SoC Exceeding", "SoC Exceeding", "SoC Below", "Concomitant Below")
#' )
#'
#' cliplot(Estimate ~ Endpoint, data = mcid_df, type = type_mcid(mcid = "MCID", n_treated = "N"))
#' }
#'
#' @export
type_mcid = function(
    mcid             = NULL,
    estimate         = NULL,
    n_treated        = NULL,
    category         = NULL,
    sort_by_ratio    = TRUE,
    segment_width    = 1.2,
    mcid_tick_size   = 3.0,
    point_size_range = c(3, 7.5),
    palette          = NULL,
    xlab             = "Absolute Clinical Estimate (|estimate|)",
    ylab             = NULL
) {
  cliplot_type(
    data = function(settings, ...) {
      df = settings$data
      if (is.null(df)) {
        df = data.frame(
          endpoint = settings$x %||% paste0("Endpoint_", seq_along(settings$y)),
          estimate = abs(as.numeric(settings$y)),
          stringsAsFactors = FALSE
        )
      } else {
        nms = names(df)
        df$endpoint = as.character(settings$x %||% df[[1]])
        raw_est     = as.numeric(settings$y %||% (if (!is.null(estimate) && estimate %in% nms) df[[estimate]] else df[[2]]))
        df$estimate = abs(raw_est)
      }

      # Extract MCID
      if (!is.null(mcid) && is.character(mcid) && length(mcid) == 1 && mcid %in% names(df)) {
        df$mcid = as.numeric(df[[mcid]])
      } else if (is.numeric(mcid)) {
        df$mcid = if (length(mcid) == 1) rep(mcid, nrow(df)) else mcid
      } else if ("mcid" %in% tolower(names(df))) {
        col_m = names(df)[tolower(names(df)) == "mcid"][1]
        df$mcid = as.numeric(df[[col_m]])
      } else {
        # Default fallback MCID if not provided
        df$mcid = 5.0
      }

      # Extract sample size
      df$n_treated = 50
      if (!is.null(n_treated) && is.character(n_treated) && length(n_treated) == 1 && n_treated %in% names(df)) {
        df$n_treated = as.numeric(df[[n_treated]])
      } else if (is.numeric(n_treated) && length(n_treated) == nrow(df)) {
        df$n_treated = n_treated
      } else if ("n" %in% tolower(names(df))) {
        col_n = names(df)[tolower(names(df)) == "n"][1]
        df$n_treated = as.numeric(df[[col_n]])
      }

      # Effect to MCID ratio
      df$ratio = df$estimate / pmax(df$mcid, 1e-4)

      # Automatic clinical categorization if not provided
      if (!is.null(category) && is.character(category) && length(category) == 1 && category %in% names(df)) {
        df$category = as.character(df[[category]])
      } else if ("category" %in% tolower(names(df))) {
        col_c = names(df)[tolower(names(df)) == "category"][1]
        df$category = as.character(df[[col_c]])
      } else {
        df$category = ifelse(
          df$ratio >= 1.0,
          "Exceeding MCID (Clinically Meaningful)",
          "Below MCID (Sub-clinical / Non-meaningful)"
        )
      }

      # Sort by ratio
      if (sort_by_ratio) {
        df = df[order(df$ratio), ]
      }
      df$endpoint = factor(df$endpoint, levels = df$endpoint)

      # Standard Archetype 1 Palette
      cat_levels = unique(df$category)
      default_cols = c(
        "#14A76C", # emerald green (exceeding)
        "#2BAEE5", # blue (standard below)
        "#E20079", # magenta/red (concomitant exceeding)
        "#8A949E", # grey (below)
        "#E18727", # amber
        "#3C5488"  # slate
      )
      names(default_cols) = cat_levels[seq_len(min(length(cat_levels), length(default_cols)))]

      settings$mcid_data         = df
      settings$mcid_colors       = palette %||% default_cols
      settings$segment_width     = segment_width
      settings$mcid_tick_size    = mcid_tick_size
      settings$point_size_range  = point_size_range
      settings$xlab              = xlab
      settings$ylab              = ylab
    },
    draw = function(data, mapping, settings, ...) {
      df = settings$mcid_data
      if (is.null(df)) return(ggplot2::ggplot())

      p = ggplot2::ggplot(df) +
        # Subtle horizontal guide lines
        ggplot2::geom_segment(
          ggplot2::aes(
            y = .data[["endpoint"]],
            yend = .data[["endpoint"]],
            x = 0,
            xend = max(.data[["estimate"]], .data[["mcid"]]) * 1.15
          ),
          color = "#EEEEEE", linewidth = 0.5, linetype = "solid"
        ) +
        # Connecting dumbbell segment
        ggplot2::geom_segment(
          ggplot2::aes(
            y    = .data[["endpoint"]],
            yend = .data[["endpoint"]],
            x    = .data[["mcid"]],
            xend = .data[["estimate"]],
            color = .data[["category"]]
          ),
          linewidth = settings$segment_width
        ) +
        # MCID reference vertical tick (shape 124 = '|')
        ggplot2::geom_point(
          ggplot2::aes(x = .data[["mcid"]], y = .data[["endpoint"]]),
          shape = 124, size = settings$mcid_tick_size, stroke = 2.4, color = "#444444"
        ) +
        # Observed estimate point sized by sample size
        ggplot2::geom_point(
          ggplot2::aes(
            x    = .data[["estimate"]],
            y    = .data[["endpoint"]],
            fill = .data[["category"]],
            size = .data[["n_treated"]]
          ),
          shape = 21, color = "white", stroke = 0.6
        ) +
        ggplot2::scale_size_continuous(
          range = settings$point_size_range,
          name  = "Sample size (N)"
        ) +
        ggplot2::scale_color_manual(
          values = settings$mcid_colors,
          name   = "Clinical Category"
        ) +
        ggplot2::scale_fill_manual(
          values = settings$mcid_colors,
          name   = "Clinical Category"
        ) +
        ggplot2::labs(
          x = settings$xlab,
          y = settings$ylab %||% "",
          subtitle = "Observed Effect Estimate vs MCID Reference Threshold (|) by Effect-to-MCID Ratio"
        ) +
        ggplot2::theme(
          panel.grid.major.y = ggplot2::element_blank(),
          panel.grid.minor   = ggplot2::element_blank(),
          axis.text.y        = ggplot2::element_text(face = "bold", size = 8.5)
        )

      p
    },
    name = "mcid"
  )
}
