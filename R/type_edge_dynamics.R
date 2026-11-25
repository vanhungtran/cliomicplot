# ===========================================================================
# cliomicplot: Ecological Network Edge-Sign Dynamics (Competition vs Mutualism)
# Positive vs Negative Co-Occurrence Ratios Across Treatments & Gradients
# Faust & Raes (2012), Deng et al. (Science 2012), Wen et al. (iMeta 2025)
# ===========================================================================

#' Ecological Network Edge-Sign Dynamics Plot Type
#'
#' @description
#' Visualizes the balance between ecological cooperation (positive co-occurrence links)
#' and competitive exclusion (negative correlation links) across experimental groups,
#' clinical disease states, or spatiotemporal gradients (Wen et al. iMeta 2025).
#'
#' @param mode Character string specifying representation: \code{"stack"} (stacked proportion
#'   100\% bar chart), \code{"ratio"} (negative-to-positive ratio trendline/points),
#'   or \code{"counts"} (grouped absolute edge count bars). Default is \code{"stack"}.
#' @param pos_color Color for positive co-occurrence edges (default \code{"#4DBBD5"}, cyan blue).
#' @param neg_color Color for negative co-occurrence edges (default \code{"#E64B35"}, coral red).
#' @param bar_width Numeric width of bars (default 0.65).
#' @param show_labels Logical. If TRUE (default), displays percentage text inside stacked bars.
#' @param label_size Numeric font size for percentage labels (default 3.0).
#' @param threshold_ratio Numeric reference ratio line (default 1.0 or NULL).
#' @param edge_data Optional data frame with columns \code{group}, \code{sign} (or \code{cor}/\code{weight}),
#'   and optional \code{count}.
#'
#' @return A \code{cliplot_type} object for use with \code{\link{cliplot}}.
#'
#' @export
#' @examples
#' \dontrun{
#' cliplot(type = type_edge_dynamics(mode = "stack"))
#' }
type_edge_dynamics = function(
    mode            = c("stack", "ratio", "counts"),
    pos_color       = "#4DBBD5",
    neg_color       = "#E64B35",
    bar_width       = 0.65,
    show_labels     = TRUE,
    label_size      = 3.0,
    threshold_ratio = NULL,
    edge_data       = NULL
) {
  mode <- match.arg(mode)

  cliplot_type(
    name = "edge_dynamics",
    data = function(settings, ...) {
      settings$mode            <- mode
      settings$pos_color       <- pos_color
      settings$neg_color       <- neg_color
      settings$bar_width       <- bar_width
      settings$show_labels     <- show_labels
      settings$label_size      <- label_size
      settings$threshold_ratio <- threshold_ratio
      settings$edge_data       <- edge_data %||% settings$data
      if (is.null(settings$legend)) settings$legend <- "top"
    },
    draw = function(data, mapping, settings, ...) {
      df <- settings$edge_data

      # Generate demo dataset if null
      if (is.null(df)) {
        groups <- c("Healthy", "Mild", "Moderate", "Severe")
        set.seed(42)
        pos_counts <- c(340, 290, 220, 150)
        neg_counts <- c(45, 90, 140, 185)

        df_list <- list()
        for (i in seq_along(groups)) {
          df_list[[length(df_list) + 1]] <- data.frame(
            group = groups[i], sign = "Positive (+)", count = pos_counts[i], stringsAsFactors = FALSE
          )
          df_list[[length(df_list) + 1]] <- data.frame(
            group = groups[i], sign = "Negative (-)", count = neg_counts[i], stringsAsFactors = FALSE
          )
        }
        df <- do.call(rbind, df_list)
        df$group <- factor(df$group, levels = groups)
      }

      names(df) <- tolower(names(df))

      grp_col  <- names(df)[which(names(df) %in% c("group", "condition", "treatment", "time", "timepoint"))[1]]
      sign_col <- names(df)[which(names(df) %in% c("sign", "type", "direction", "interaction"))[1]]
      cnt_col  <- names(df)[which(names(df) %in% c("count", "n", "edges", "frequency"))[1]]

      if (is.na(cnt_col)) {
        # Tabulate if raw edge list
        df$count <- 1
        cnt_col <- "count"
      }

      df$grp_clean  <- factor(df[[grp_col]])
      df$sign_clean <- factor(
        ifelse(grepl("pos|\\+", tolower(as.character(df[[sign_col]]))), "Positive (+)", "Negative (-)"),
        levels = c("Positive (+)", "Negative (-)")
      )
      df$count_num  <- as.numeric(df[[cnt_col]])

      # Aggregate counts per group and sign
      agg <- stats::aggregate(count_num ~ grp_clean + sign_clean, data = df, FUN = sum)
      grp_totals <- stats::aggregate(count_num ~ grp_clean, data = agg, FUN = sum)
      names(grp_totals)[2] <- "total"

      agg <- merge(agg, grp_totals, by = "grp_clean")
      agg$pct <- round(100 * agg$count_num / agg$total, 1)

      sign_colors <- c("Positive (+)" = settings$pos_color, "Negative (-)" = settings$neg_color)

      p <- ggplot2::ggplot()

      if (settings$mode == "stack") {
        # 100% Stacked bar chart
        p <- p +
          ggplot2::geom_col(
            data = agg,
            ggplot2::aes(x = .data$grp_clean, y = .data$pct, fill = .data$sign_clean),
            position = "stack",
            width = settings$bar_width,
            color = "white",
            linewidth = 0.4
          ) +
          ggplot2::scale_fill_manual(values = sign_colors, name = "Interaction Sign: ") +
          ggplot2::scale_y_continuous(
            labels = function(x) paste0(x, "%"),
            expand = ggplot2::expansion(mult = c(0, 0.05))
          ) +
          ggplot2::labs(
            x = "",
            y = "Proportion of Network Co-Occurrences (%)",
            title = "Ecological Interaction Dynamics (Cooperation vs Competition)",
            subtitle = "Positive co-occurrences (mutualism) vs Negative correlations (competition)"
          )

        if (settings$show_labels) {
          # Calculate midpoint for labels
          agg_pos <- agg[agg$sign_clean == "Positive (+)", ]
          agg_neg <- agg[agg$sign_clean == "Negative (-)", ]

          # In stack, Positive is bottom (pct / 2), Negative is top (100 - pct / 2)
          label_df <- rbind(
            data.frame(
              grp_clean = agg_pos$grp_clean,
              y = agg_pos$pct / 2,
              label = paste0(agg_pos$pct, "%"),
              color = "white"
            ),
            data.frame(
              grp_clean = agg_neg$grp_clean,
              y = 100 - agg_neg$pct / 2,
              label = paste0(agg_neg$pct, "%"),
              color = "white"
            )
          )
          label_df <- label_df[label_df$y > 5 & label_df$y < 95, ]

          p <- p + ggplot2::geom_text(
            data = label_df,
            ggplot2::aes(x = .data$grp_clean, y = .data$y, label = .data$label),
            size = settings$label_size,
            fontface = "bold",
            color = "white"
          )
        }
      } else if (settings$mode == "counts") {
        # Absolute counts grouped
        p <- p +
          ggplot2::geom_col(
            data = agg,
            ggplot2::aes(x = .data$grp_clean, y = .data$count_num, fill = .data$sign_clean),
            position = ggplot2::position_dodge(width = settings$bar_width),
            width = settings$bar_width * 0.9,
            color = "white",
            linewidth = 0.3
          ) +
          ggplot2::scale_fill_manual(values = sign_colors, name = "Interaction Sign: ") +
          ggplot2::scale_y_continuous(expand = ggplot2::expansion(mult = c(0, 0.08))) +
          ggplot2::labs(
            x = "",
            y = "Number of Co-Occurrence Edges",
            title = "Absolute Edge-Sign Frequency Across Conditions"
          )
      } else if (settings$mode == "ratio") {
        # Negative-to-Positive Ratio Trendline
        wide_counts <- stats::reshape(agg[, c("grp_clean", "sign_clean", "count_num")],
                                      idvar = "grp_clean", timevar = "sign_clean", direction = "wide")
        names(wide_counts) <- c("group", "pos", "neg")
        wide_counts$pos[is.na(wide_counts$pos)] <- 0
        wide_counts$neg[is.na(wide_counts$neg)] <- 0
        wide_counts$ratio <- round(wide_counts$neg / pmax(1, wide_counts$pos), 3)

        p <- p +
          ggplot2::geom_line(
            data = wide_counts,
            ggplot2::aes(x = as.numeric(.data$group), y = .data$ratio),
            color = "#E64B35",
            linewidth = 1.0,
            group = 1
          ) +
          ggplot2::geom_point(
            data = wide_counts,
            ggplot2::aes(x = as.numeric(.data$group), y = .data$ratio),
            size = 4.2,
            shape = 21,
            fill = "#E64B35",
            color = "white",
            stroke = 0.8
          ) +
          ggplot2::geom_text(
            data = wide_counts,
            ggplot2::aes(x = as.numeric(.data$group), y = .data$ratio, label = sprintf("%.2f", .data$ratio)),
            vjust = -0.8,
            size = settings$label_size,
            fontface = "bold",
            color = "#2A2D34"
          ) +
          ggplot2::scale_x_continuous(
            breaks = seq_along(levels(agg$grp_clean)),
            labels = levels(agg$grp_clean)
          ) +
          ggplot2::scale_y_continuous(expand = ggplot2::expansion(mult = c(0.1, 0.18))) +
          ggplot2::labs(
            x = "",
            y = "Competition Ratio (Negative / Positive Edges)",
            title = "Ecological Competition Ratio Trajectory"
          )

        if (!is.null(settings$threshold_ratio)) {
          p <- p + ggplot2::geom_hline(
            yintercept = settings$threshold_ratio,
            linetype = "dashed",
            color = "#8A92A0",
            linewidth = 0.5
          )
        }
      }

      p <- p +
        ggplot2::theme_minimal(base_size = 11) +
        ggplot2::theme(
          legend.position = settings$legend,
          panel.grid.major.x = ggplot2::element_blank(),
          panel.grid.minor = ggplot2::element_blank(),
          axis.text.x = ggplot2::element_text(size = 9.5, face = "bold", color = "#2A2D34"),
          axis.text.y = ggplot2::element_text(size = 8.5),
          plot.title = ggplot2::element_text(face = "bold", size = 11),
          plot.subtitle = ggplot2::element_text(size = 9, color = "#6B7280")
        )

      p
    }
  )
}
