# ===========================================================================
# cliomicplot: Cross-Network Module Comparison Heatmap
# Multi-Omics & Systems Biology Figure Archetype
# ===========================================================================

#' Cross-Network Module Comparison Heatmap
#'
#' @description
#' Creates publication-ready cross-network module comparison plots to evaluate
#' module preservation, overlap, and reorganization across distinct biological
#' conditions, cohorts, timepoints, or tissues (e.g., Discovery vs Replication,
#' Health vs Disease, Infant vs Mother, or Microbiome vs Host).
#' Evaluates module correspondence using hypergeometric test statistics,
#' Jaccard similarity indices, representation factors (fold enrichment), and raw overlap counts.
#'
#' @param fill_by Metric used for tile fill color: \code{"log10p"} (-\eqn{\log_{10}(p)}
#'   from hypergeometric test, default), \code{"jaccard"} (Jaccard similarity index),
#'   \code{"overlap"} (count of shared nodes), or \code{"rf"} (representation factor / fold enrichment).
#' @param show_counts Logical. If TRUE, displays the number of shared features in each cell. Default TRUE.
#' @param show_p Logical. If TRUE, displays the hypergeometric p-value (or -log10 p) in each cell. Default FALSE.
#' @param show_stars Logical. If TRUE, appends significance asterisks (* p < 0.05, ** p < 0.01, *** p < 0.001). Default TRUE.
#' @param text_size Numeric text size for cell annotations (default 3.2).
#' @param text_color Character color for cell annotations. If "auto" (default), chooses white for dark tiles and dark grey for light tiles.
#' @param colors Sequential or gradient color vector for fill scale. Default \code{c("#F7FBFF", "#9ECAE1", "#2171B5", "#08306B")}.
#' @param na_color Background color for cells with zero overlap (default \code{"white"}).
#' @param border_color Outline color for tiles (default \code{"#DCE2EC"}).
#' @param border_size Outline stroke width (default \code{0.3}).
#' @param legend_title Legend title string (default depends on \code{fill_by}).
#' @param show_totals Logical. If TRUE, appends module member counts to axis labels (e.g. "M1 (n=45)"). Default TRUE.
#' @param total_universe Optional integer specifying the total shared feature universe size N for the hypergeometric test.
#'   If NULL, calculated automatically from unique features in the input data.
#' @param min_overlap Minimum overlap count required to display text label (default 0).
#' @param xlab,ylab Axis labels.
#'
#' @return A \code{cliplot_type} object for use with \code{\link{cliplot}}.
#'
#' @examples
#' \dontrun{
#' # Simulated node-level network module assignments
#' set.seed(42)
#' genes <- paste0("Gene_", 1:120)
#' net1 <- sample(c("M1_Inflam", "M2_Barrier", "M3_Lipid"), 120, replace = TRUE)
#' net2 <- net1
#' idx <- sample(1:120, 35)
#' net2[idx] <- sample(c("ModA_Th2", "ModB_Stratum", "ModC_Sebum"), 35, replace = TRUE)
#'
#' df_nodes <- data.frame(Discovery = net1, Replication = net2)
#' cliplot(Replication ~ Discovery, data = df_nodes, type = type_module_compare())
#' }
#'
#' @export
type_module_compare = function(
    fill_by        = c("log10p", "jaccard", "overlap", "rf"),
    show_counts    = TRUE,
    show_p         = FALSE,
    show_stars     = TRUE,
    text_size      = 3.2,
    text_color     = "auto",
    colors         = c("#F7FBFF", "#9ECAE1", "#2171B5", "#08306B"),
    na_color       = "white",
    border_color   = "#DCE2EC",
    border_size    = 0.3,
    legend_title   = NULL,
    show_totals    = TRUE,
    total_universe = NULL,
    min_overlap    = 0,
    xlab           = NULL,
    ylab           = NULL
) {
  fill_by <- match.arg(fill_by)

  cliplot_type(
    data = function(settings, ...) {
      df <- settings$data
      nms <- if (!is.null(df)) names(df) else character(0)

      # Determine x (Network 1) and y (Network 2)
      candidates_1 <- intersect(c("net1", "network1", "discovery", "module1", "mod1", "net1_module", "Net1", "Network1"), nms)
      candidates_2 <- intersect(c("net2", "network2", "replication", "module2", "mod2", "net2_module", "Net2", "Network2"), nms)

      x_vec <- if (!is.null(settings$x)) {
        if (length(settings$x) == nrow(df)) as.character(settings$x)
        else if (is.character(settings$x) && length(settings$x) == 1 && settings$x %in% nms) as.character(df[[settings$x]])
        else if (length(candidates_1) > 0) as.character(df[[candidates_1[1]]])
        else as.character(df[[1]])
      } else if (length(candidates_1) > 0) {
        as.character(df[[candidates_1[1]]])
      } else {
        as.character(df[[1]])
      }

      y_vec <- if (!is.null(settings$y)) {
        if (length(settings$y) == nrow(df)) as.character(settings$y)
        else if (is.character(settings$y) && length(settings$y) == 1 && settings$y %in% nms) as.character(df[[settings$y]])
        else if (length(candidates_2) > 0) as.character(df[[candidates_2[1]]])
        else as.character(df[[min(2, ncol(df))]])
      } else if (length(candidates_2) > 0) {
        as.character(df[[candidates_2[1]]])
      } else {
        as.character(df[[min(2, ncol(df))]])
      }

      # Check if df is already summary or node-level
      is_summary <- ("overlap" %in% tolower(nms) || "count" %in% tolower(nms) || "n_shared" %in% tolower(nms)) &&
                    !any(duplicated(paste(x_vec, y_vec, sep = "___")))

      if (is_summary) {
        cnt_col <- nms[tolower(nms) %in% c("overlap", "count", "n_shared", "n")][1]
        cnt <- as.numeric(df[[cnt_col]])
        p_col <- nms[tolower(nms) %in% c("pvalue", "p", "pval", "padj", "qvalue")][1]
        pval <- if (!is.na(p_col)) as.numeric(df[[p_col]]) else rep(NA_real_, nrow(df))
        j_col <- nms[tolower(nms) %in% c("jaccard", "jaccard_index", "j")][1]
        jacc <- if (!is.na(j_col)) as.numeric(df[[j_col]]) else rep(NA_real_, nrow(df))
        rf_col <- nms[tolower(nms) %in% c("rf", "representation_factor", "enrichment")][1]
        rf <- if (!is.na(rf_col)) as.numeric(df[[rf_col]]) else rep(NA_real_, nrow(df))

        u1 <- unique(x_vec)
        u2 <- unique(y_vec)
        k1_map <- tapply(cnt, x_vec, sum, na.rm = TRUE)
        k2_map <- tapply(cnt, y_vec, sum, na.rm = TRUE)

        comp_df <- data.frame(
          .net1 = x_vec,
          .net2 = y_vec,
          .overlap = cnt,
          .pval = pval,
          .jaccard = jacc,
          .rf = rf,
          .k1 = as.numeric(k1_map[x_vec]),
          .k2 = as.numeric(k2_map[y_vec]),
          stringsAsFactors = FALSE
        )
      } else {
        # Node-level dataset: calculate contingency table and hypergeometric statistics
        u1 <- unique(x_vec)
        u2 <- unique(y_vec)
        N <- total_universe %||% length(x_vec)

        grid <- expand.grid(.net1 = u1, .net2 = u2, stringsAsFactors = FALSE)
        n1_sizes <- table(x_vec)
        n2_sizes <- table(y_vec)

        overlaps <- integer(nrow(grid))
        pvals    <- numeric(nrow(grid))
        jaccards <- numeric(nrow(grid))
        rfs      <- numeric(nrow(grid))

        for (i in seq_len(nrow(grid))) {
          m1 <- grid$.net1[i]
          m2 <- grid$.net2[i]
          m  <- sum(x_vec == m1 & y_vec == m2)
          k1 <- as.integer(n1_sizes[m1])
          k2 <- as.integer(n2_sizes[m2])

          overlaps[i] <- m
          if (m > 0) {
            # phyper(q, m, n, k): q=m-1, m=k2, n=N-k2, k=k1
            pvals[i]    <- stats::phyper(m - 1, k2, max(0, N - k2), k1, lower.tail = FALSE)
            jaccards[i] <- m / (k1 + k2 - m)
            rfs[i]      <- (m / N) / ((k1 / N) * (k2 / N))
          } else {
            pvals[i]    <- 1.0
            jaccards[i] <- 0.0
            rfs[i]      <- 0.0
          }
        }

        comp_df <- data.frame(
          .net1    = grid$.net1,
          .net2    = grid$.net2,
          .overlap = overlaps,
          .pval    = pvals,
          .jaccard = jaccards,
          .rf      = rfs,
          .k1      = as.integer(n1_sizes[grid$.net1]),
          .k2      = as.integer(n2_sizes[grid$.net2]),
          stringsAsFactors = FALSE
        )
      }

      # Compute -log10 p
      comp_df$.log10p <- -log10(pmax(comp_df$.pval, 1e-50))

      # Choose fill variable
      comp_df$.fill <- switch(
        fill_by,
        log10p  = comp_df$.log10p,
        jaccard = comp_df$.jaccard,
        overlap = comp_df$.overlap,
        rf      = comp_df$.rf
      )

      # Optional total counts on axis labels
      if (isTRUE(show_totals)) {
        x_labs <- paste0(unique(comp_df$.net1), "\n(n=", comp_df$.k1[match(unique(comp_df$.net1), comp_df$.net1)], ")")
        names(x_labs) <- unique(comp_df$.net1)
        y_labs <- paste0(unique(comp_df$.net2), " (n=", comp_df$.k2[match(unique(comp_df$.net2), comp_df$.net2)], ")")
        names(y_labs) <- unique(comp_df$.net2)

        comp_df$.x <- factor(x_labs[comp_df$.net1], levels = x_labs)
        comp_df$.y <- factor(y_labs[comp_df$.net2], levels = rev(y_labs))
      } else {
        comp_df$.x <- factor(comp_df$.net1, levels = unique(comp_df$.net1))
        comp_df$.y <- factor(comp_df$.net2, levels = rev(unique(comp_df$.net2)))
      }

      # Construct cell text annotations
      lbls <- character(nrow(comp_df))
      for (i in seq_len(nrow(comp_df))) {
        cnt_val <- comp_df$.overlap[i]
        p_val   <- comp_df$.pval[i]

        txt <- ""
        if (isTRUE(show_counts)) {
          txt <- if (cnt_val >= min_overlap) as.character(cnt_val) else ""
        }
        if (isTRUE(show_stars) && !is.na(p_val) && cnt_val > 0) {
          stars <- if (p_val < 0.001) "***" else if (p_val < 0.01) "**" else if (p_val < 0.05) "*" else ""
          txt <- paste0(txt, stars)
        }
        if (isTRUE(show_p) && !is.na(p_val) && cnt_val > 0) {
          p_str <- if (p_val < 0.001) sprintf("%.1e", p_val) else sprintf("%.3f", p_val)
          txt <- if (nzchar(txt)) paste0(txt, "\n(p=", p_str, ")") else paste0("p=", p_str)
        }
        lbls[i] <- txt
      }
      comp_df$.label <- lbls

      # Automatic contrast text color
      v <- comp_df$.fill
      min_v <- min(v, na.rm = TRUE)
      max_v <- max(v, na.rm = TRUE)
      span  <- if (max_v > min_v) (max_v - min_v) else 1
      norm_v <- (v - min_v) / span
      comp_df$.txt_col <- ifelse(norm_v > 0.58, "white", "grey20")

      settings$comp_df <- comp_df
      settings$comp_fill_by <- fill_by
      settings$comp_legend_title <- legend_title
      settings$comp_text_color <- text_color
      settings$comp_text_size <- text_size
      settings$comp_border_color <- border_color
      settings$comp_border_size <- border_size
      settings$comp_colors <- colors
      settings$comp_na_color <- na_color
      settings$comp_xlab <- xlab
      settings$comp_ylab <- ylab
    },

    draw = function(data, mapping, settings, ...) {
      df <- settings$comp_df
      if (is.null(df) || nrow(df) == 0) return(ggplot2::ggplot())

      leg_title <- settings$comp_legend_title %||% switch(
        settings$comp_fill_by,
        log10p  = expression(-log[10]~italic(p)),
        jaccard = "Jaccard Index",
        overlap = "Shared Count",
        rf      = "Enrichment (RF)"
      )

      p <- ggplot2::ggplot(df, ggplot2::aes(x = .data[[".x"]], y = .data[[".y"]])) +
        ggplot2::geom_tile(
          ggplot2::aes(fill = .data[[".fill"]]),
          color = settings$comp_border_color,
          linewidth = settings$comp_border_size
        ) +
        ggplot2::scale_fill_gradientn(
          colors = settings$comp_colors,
          na.value = settings$comp_na_color,
          name = leg_title
        ) +
        ggplot2::labs(
          x = settings$comp_xlab %||% settings$xlab %||% "",
          y = settings$comp_ylab %||% settings$ylab %||% ""
        ) +
        ggplot2::theme(
          axis.text.x = ggplot2::element_text(angle = 45, hjust = 1, vjust = 1),
          panel.grid = ggplot2::element_blank()
        )

      if (identical(settings$comp_text_color, "auto")) {
        p <- p + ggplot2::geom_text(
          ggplot2::aes(label = .data[[".label"]], color = .data[[".txt_col"]]),
          size = settings$comp_text_size,
          lineheight = 0.85,
          na.rm = TRUE
        ) +
        ggplot2::scale_color_identity()
      } else {
        p <- p + ggplot2::geom_text(
          ggplot2::aes(label = .data[[".label"]]),
          color = settings$comp_text_color,
          size = settings$comp_text_size,
          lineheight = 0.85,
          na.rm = TRUE
        )
      }

      p
    },
    name = "module_compare"
  )
}
