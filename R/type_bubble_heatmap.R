#' Diamond Bubble Heatmap
#'
#' @description
#' Creates a matrix of diamond-shaped bubbles where marker-sample combinations
#' are encoded by size (e.g., fold-change) and colour (e.g., significance).
#'
#' @param label_threshold Size threshold above which cell labels are drawn.
#' @param max_size Maximum point size (default 15).
#' @param band_alpha Opacity for alternating row bands.
#' @param band_fill Fill colour for alternating row bands.
#' @param cell_border Border colour for guide tiles.
#' @param size_name Legend title for the size scale.
#' @param fill_name Legend title for the fill colour scale.
#' @param fill_low,fill_mid,fill_high Colour stops for the fill gradient.
#'
#' @return A \code{cliplot_type} object.
#' @export
type_bubble_heatmap = function(
    label_threshold  = 2.0,
    max_size         = 15,
    band_alpha       = 0.6,
    band_fill        = "#F2F5FA",
    cell_border      = "#E2E6EF",
    size_name        = "Fold change",
    fill_name        = expression(-log[10] ~ "adjusted P"),
    fill_low         = "#D6E4F0",
    fill_mid         = "#59B8A8",
    fill_high        = "#A83D62"
) {
  cliplot_type(
    data = function(settings, ...) {
      row_var  <- settings$x; col_var  <- settings$y; size_var <- settings$by
      if (is.null(row_var) || is.null(col_var))
        stop("type_bubble_heatmap requires row and column variables.", call. = FALSE)
      df <- data.frame(row = as.character(row_var), col = as.character(col_var),
        size = if (!is.null(size_var)) as.numeric(size_var) else rep(1, length(row_var)),
        stringsAsFactors = FALSE)
      if (!is.null(settings$extra_data) && !is.null(settings$extra_data$fill))
        df$fill <- as.numeric(settings$extra_data$fill) else df$fill <- df$size
      seen_r <- character(0); cats_r <- character(0)
      for (r in df$row) { if (!r %in% seen_r) { seen_r <- c(seen_r, r); cats_r <- c(cats_r, r) } }
      seen_c <- character(0); cats_c <- character(0)
      for (c in df$col) { if (!c %in% seen_c) { seen_c <- c(seen_c, c); cats_c <- c(cats_c, c) } }
      df$row <- factor(df$row, levels = rev(cats_r)); df$col <- factor(df$col, levels = cats_c)
      df$label <- ifelse(df$size >= label_threshold, format(round(df$size, 2), nsmall = 2), "")
      settings$bh_df <- df; settings$bh_max_size <- max_size
      settings$bh_band_alpha <- band_alpha; settings$bh_band_fill <- band_fill
      settings$bh_cell_border <- cell_border; settings$bh_size_name <- size_name
      settings$bh_fill_name <- fill_name; settings$bh_fill_low <- fill_low
      settings$bh_fill_mid <- fill_mid; settings$bh_fill_high <- fill_high
      settings$by <- NULL
    },
    draw = function(data, mapping, settings, ...) {
      df <- settings$bh_df
      if (is.null(df)) return(ggplot2::ggplot())
      n_rows <- length(levels(df$row))
      band_ymin <- seq(0.5, n_rows + 0.5, by = 2); band_ymax <- seq(1.5, n_rows + 1.5, by = 2)
      p <- ggplot2::ggplot(df, ggplot2::aes(x = .data[["col"]], y = .data[["row"]])) +
        ggplot2::annotate("rect", xmin = -Inf, xmax = Inf, ymin = band_ymin, ymax = band_ymax,
          fill = settings$bh_band_fill, alpha = settings$bh_band_alpha) +
        ggplot2::geom_tile(fill = NA, colour = settings$bh_cell_border, linewidth = 0.35) +
        ggplot2::geom_point(ggplot2::aes(size = .data[["size"]], fill = .data[["fill"]]),
          shape = 23, colour = "white", stroke = 0.65, alpha = 0.95) +
        ggplot2::geom_text(ggplot2::aes(label = .data[["label"]]),
          size = 2.7, fontface = "bold", colour = "#182033") +
        ggplot2::scale_size_area(max_size = settings$bh_max_size,
          limits = c(0, max(df$size, na.rm = TRUE)), name = settings$bh_size_name) +
        ggplot2::scale_fill_gradientn(
          colours = c(settings$bh_fill_low, settings$bh_fill_mid, settings$bh_fill_high),
          name = settings$bh_fill_name) +
        ggplot2::coord_fixed() + ggplot2::labs(x = NULL, y = "Marker") +
        theme_cli_base() + ggplot2::theme(panel.grid = ggplot2::element_blank(),
          axis.text.x = ggplot2::element_text(angle = 45, hjust = 1, face = "bold", colour = "#3E465A"),
          axis.text.y = ggplot2::element_text(face = "bold", colour = "#3E465A"),
          legend.title = ggplot2::element_text(face = "bold", size = 9), axis.title = ggplot2::element_blank())
      p
    },
    name = "bubble_heatmap"
  )
}
