#' Diamond Bubble Heatmap
#'
#' @description
#' Creates a matrix of diamond-shaped bubbles where marker-sample combinations
#' are encoded by size (e.g., fold-change) and colour (e.g., significance).
#'
#' @param label_threshold Size threshold above which cell labels are drawn (default 1.5).
#' @param max_size Maximum point size (default 9).
#' @param label_size Font size for cell text labels (default 2.4).
#' @param band_alpha Opacity for alternating row bands.
#' @param band_fill Fill colour for alternating row bands.
#' @param cell_border Border colour for guide tiles.
#' @param size_name Legend title for the size scale.
#' @param fill_name Legend title for the fill colour scale.
#' @param fill_low,fill_mid,fill_high Colour stops for the fill gradient.
#' @param axis_text_size Size of axis text. If NULL (default), adaptively scaled.
#' @param axis_text_angle Angle for x-axis labels (default 45).
#' @param transpose If TRUE, swaps rows and columns (default FALSE).
#'
#' @return A \code{cliplot_type} object.
#' @export
type_bubble_heatmap = function(
    label_threshold  = 1.5,
    max_size         = 9,
    label_size       = 2.4,
    band_alpha       = 0.6,
    band_fill        = "#F2F5FA",
    cell_border      = "#E2E6EF",
    size_name        = "|log₂FC|",
    fill_name        = expression(-log[10] ~ "adjusted P"),
    fill_low         = "#D6E4F0",
    fill_mid         = "#59B8A8",
    fill_high        = "#A83D62",
    axis_text_size   = NULL,
    axis_text_angle  = 45,
    transpose        = FALSE
) {
  cliplot_type(
    data = function(settings, ...) {
      # In standard formula y ~ x:
      # y represents the vertical axis (rows)
      # x represents the horizontal axis (columns)
      if (!isTRUE(transpose)) {
        row_var  <- settings$y
        col_var  <- settings$x
      } else {
        row_var  <- settings$x
        col_var  <- settings$y
      }
      size_var <- settings$by

      if (is.null(row_var) || is.null(col_var))
        stop("type_bubble_heatmap requires row and column variables.", call. = FALSE)

      df <- data.frame(
        row  = as.character(row_var),
        col  = as.character(col_var),
        size = if (!is.null(size_var)) as.numeric(size_var) else rep(1, length(row_var)),
        stringsAsFactors = FALSE
      )

      if (!is.null(settings$extra_data) && !is.null(settings$extra_data$fill)) {
        df$fill <- as.numeric(settings$extra_data$fill)
      } else {
        df$fill <- df$size
      }

      seen_r <- character(0); cats_r <- character(0)
      for (r in df$row) { if (!r %in% seen_r) { seen_r <- c(seen_r, r); cats_r <- c(cats_r, r) } }
      seen_c <- character(0); cats_c <- character(0)
      for (c in df$col) { if (!c %in% seen_c) { seen_c <- c(seen_c, c); cats_c <- c(cats_c, c) } }

      # Y-axis (rows) plotted bottom-to-top, so rev(cats_r) keeps first row at top
      df$row <- factor(df$row, levels = rev(cats_r))
      df$col <- factor(df$col, levels = cats_c)

      df$label <- ifelse(
        !is.na(df$size) & df$size >= label_threshold,
        format(round(df$size, 2), nsmall = 2),
        ""
      )

      # Adaptive axis sizing
      n_rows <- length(cats_r)
      n_cols <- length(cats_c)

      if (is.null(axis_text_size)) {
        adaptive_y_size <- if (n_rows <= 10) 9.0
                           else if (n_rows <= 20) 8.0
                           else if (n_rows <= 35) 7.0
                           else 6.0

        adaptive_x_size <- if (n_cols <= 10) 8.5
                           else if (n_cols <= 20) 7.5
                           else 6.5
      } else {
        adaptive_y_size <- axis_text_size
        adaptive_x_size <- axis_text_size
      }

      settings$bh_df          <- df
      settings$bh_max_size    <- max_size
      settings$bh_label_size  <- label_size
      settings$bh_band_alpha  <- band_alpha
      settings$bh_band_fill   <- band_fill
      settings$bh_cell_border <- cell_border
      settings$bh_size_name   <- size_name
      settings$bh_fill_name   <- fill_name
      settings$bh_fill_low    <- fill_low
      settings$bh_fill_mid    <- fill_mid
      settings$bh_fill_high   <- fill_high
      settings$bh_axis_x_size <- adaptive_x_size
      settings$bh_axis_y_size <- adaptive_y_size
      settings$bh_axis_angle  <- axis_text_angle
      settings$by             <- NULL
    },
    draw = function(data, mapping, settings, ...) {
      df <- settings$bh_df
      if (is.null(df)) return(ggplot2::ggplot())

      n_rows <- length(levels(df$row))
      band_ymin <- seq(0.5, n_rows + 0.5, by = 2)
      band_ymax <- seq(1.5, n_rows + 1.5, by = 2)

      p <- ggplot2::ggplot(df, ggplot2::aes(x = .data[["col"]], y = .data[["row"]])) +
        ggplot2::annotate("rect", xmin = -Inf, xmax = Inf, ymin = band_ymin, ymax = band_ymax,
          fill = settings$bh_band_fill, alpha = settings$bh_band_alpha) +
        ggplot2::geom_tile(fill = NA, colour = settings$bh_cell_border, linewidth = 0.35) +
        ggplot2::geom_point(ggplot2::aes(size = .data[["size"]], fill = .data[["fill"]]),
          shape = 23, colour = "white", stroke = 0.65, alpha = 0.95) +
        ggplot2::geom_text(ggplot2::aes(label = .data[["label"]]),
          size = settings$bh_label_size, fontface = "bold", colour = "#182033") +
        ggplot2::scale_size_area(
          max_size = settings$bh_max_size,
          limits   = c(0, max(df$size, na.rm = TRUE)),
          name     = settings$bh_size_name,
          guide    = ggplot2::guide_legend(
            order = 2,
            keywidth = ggplot2::unit(12, "pt"),
            keyheight = ggplot2::unit(12, "pt")
          )
        ) +
        ggplot2::scale_fill_gradientn(
          colours = c(settings$bh_fill_low, settings$bh_fill_mid, settings$bh_fill_high),
          name    = settings$bh_fill_name,
          guide   = ggplot2::guide_colorbar(
            order = 1,
            barwidth = ggplot2::unit(8, "pt"),
            barheight = ggplot2::unit(60, "pt"),
            title.position = "top",
            title.hjust = 0.5
          )
        ) +
        ggplot2::coord_fixed() +
        theme_cli_base() +
        ggplot2::theme(
          panel.grid   = ggplot2::element_blank(),
          axis.text.x  = ggplot2::element_text(
            angle = settings$bh_axis_angle, hjust = 1, vjust = 1,
            face = "bold", colour = "#3E465A", size = settings$bh_axis_x_size
          ),
          axis.text.y  = ggplot2::element_text(
            face = "bold", colour = "#3E465A", size = settings$bh_axis_y_size
          ),
          axis.title.x = ggplot2::element_text(face = "bold", size = 9, margin = ggplot2::margin(t = 6)),
          axis.title.y = ggplot2::element_text(face = "bold", size = 9, margin = ggplot2::margin(r = 6)),
          legend.title = ggplot2::element_text(face = "bold", size = 8.5),
          legend.text  = ggplot2::element_text(size = 7.5),
          legend.spacing.y = ggplot2::unit(4, "pt")
        )

      p
    },
    name = "bubble_heatmap"
  )
}
