#' Bubble Heatmap Archetypes (Diamond, Circle, Square, and Diverging)
#'
#' @description
#' Creates a matrix of bubbles where marker-sample combinations are encoded
#' simultaneously by shape size (e.g., effect magnitude, |log2FC|, -log10 P, or abundance)
#' and colour (e.g., continuous significance, directional NES, or log2FC).
#' Supports multiple glyph shapes (diamond, circle, square, triangle), sequential
#' and diverging publication palettes, significance asterisks, and customizable
#' matrix backgrounds (banded, grid, minimal).
#'
#' @param shape Shape of the bubble glyphs: \code{"diamond"} (23, default),
#'   \code{"circle"} (21), \code{"square"} (22), \code{"triangle_up"} or \code{"triangle"} (24),
#'   \code{"triangle_down"} (25), or an integer shape code (21--25).
#' @param palette_type Color gradient type: \code{"sequential"} (default) or \code{"diverging"}.
#' @param palette Named publication palette or color vector:
#'   \itemize{
#'     \item Diverging: \code{"nature"}, \code{"rdbu"}, \code{"bipolar"}, \code{"lancet"},
#'       \code{"spectral"}, \code{"piyg"}, \code{"prgn"}, \code{"dark"}.
#'     \item Sequential: \code{"mako"}, \code{"rocket"}, \code{"viridis"}, \code{"plasma"},
#'       \code{"blues"}, \code{"reds"}, \code{"ylorrd"}, \code{"purples"}.
#'   }
#' @param fill_low,fill_mid,fill_high Explicit colour stops for the fill gradient when
#'   \code{palette = NULL}.
#' @param midpoint Value representing the midpoint for diverging palettes (default 0).
#' @param limits Numeric vector of length 2 specifying fill scale limits.
#' @param min_size,max_size Point size range (defaults \code{min_size = 1.0}, \code{max_size = 9.0}).
#' @param stroke_col Outline stroke colour for bubbles (default \code{"white"}).
#' @param stroke_width Outline stroke width (default 0.65).
#' @param point_alpha Bubble fill transparency (default 0.95).
#' @param grid_style Background matrix style: \code{"banded"} (alternating row stripes with borders, default),
#'   \code{"grid"} (clean tile matrix borders without stripes), \code{"minimal"} (subtle grid guides),
#'   or \code{"none"}.
#' @param band_alpha Opacity for alternating row bands in banded style (default 0.6).
#' @param band_fill Fill colour for alternating row bands (default \code{"#F2F5FA"}).
#' @param cell_border Border colour for guide tiles (default \code{"#E2E6EF"}).
#' @param label_metric Content of glyph text labels: \code{"value"} (numeric size value if above threshold),
#'   \code{"signif"} (significance asterisks \code{*}, \code{**}, \code{***}), or \code{"none"}.
#' @param label_threshold Size threshold above which cell labels are drawn when \code{label_metric = "value"} (default 1.5).
#' @param signif_cutoffs Significance thresholds for asterisks when \code{label_metric = "signif"} (default \code{c(0.05, 0.01, 0.001)}).
#' @param signif_labels Asterisk labels corresponding to cutoffs (default \code{c("*", "**", "***")}).
#' @param label_size Font size for cell text labels (default 2.4).
#' @param label_col Font colour for cell text labels (default \code{"#182033"}).
#' @param size_name Legend title for the size scale.
#' @param fill_name Legend title for the fill colour scale.
#' @param axis_text_size Size of axis text. If NULL (default), adaptively scaled.
#' @param axis_text_angle Angle for x-axis labels (default 45).
#' @param transpose If TRUE, swaps rows and columns (default FALSE).
#'
#' @return A \code{cliplot_type} object.
#' @export
type_bubble_heatmap = function(
    shape            = c("diamond", "circle", "square", "triangle_up", "triangle_down"),
    palette_type     = c("sequential", "diverging"),
    palette          = NULL,
    fill_low         = "#D6E4F0",
    fill_mid         = "#59B8A8",
    fill_high        = "#A83D62",
    midpoint         = 0,
    limits           = NULL,
    min_size         = 1.0,
    max_size         = 9.0,
    stroke_col       = "white",
    stroke_width     = 0.65,
    point_alpha      = 0.95,
    grid_style       = c("banded", "grid", "minimal", "none"),
    band_alpha       = 0.6,
    band_fill        = "#F2F5FA",
    cell_border      = "#E2E6EF",
    label_metric     = c("value", "signif", "none"),
    label_threshold  = 1.5,
    signif_cutoffs   = c(0.05, 0.01, 0.001),
    signif_labels    = c("*", "**", "***"),
    label_size       = 2.4,
    label_col        = "#182033",
    size_name        = "|log₂FC|",
    fill_name        = expression(-log[10] ~ "adjusted P"),
    axis_text_size   = NULL,
    axis_text_angle  = 45,
    transpose        = FALSE
) {
  # Resolve shape
  shape_val <- if (is.numeric(shape)) {
    shape
  } else {
    shape_choice <- match.arg(as.character(shape)[1], c("diamond", "circle", "square", "triangle_up", "triangle", "triangle_down"))
    switch(shape_choice,
      diamond       = 23,
      circle        = 21,
      square        = 22,
      triangle_up   = 24,
      triangle      = 24,
      triangle_down = 25,
      23
    )
  }

  pal_type <- match.arg(palette_type)
  g_style  <- match.arg(grid_style)
  lbl_met  <- match.arg(label_metric)

  # Palette definition dictionary
  pal_div_map <- list(
    nature   = c("#3B4992", "#F7F7F7", "#EE0000"),
    rdbu     = c("#2166AC", "#F7F7F7", "#B2182B"),
    bipolar  = c("#1F77B4", "#F0F0F0", "#D62728"),
    lancet   = c("#00468B", "#F5F5F5", "#ED0000"),
    dark     = c("#00F2FE", "#1E2638", "#FF007F"),
    cell     = c("#2166AC", "#F7F7F7", "#B2182B"),
    spectral = c("#2B83BA", "#FFFFBF", "#D7191C"),
    piyg     = c("#C51B7D", "#F7F7F7", "#4D9221"),
    prgn     = c("#762A83", "#F7F7F7", "#1B7837")
  )

  pal_seq_map <- list(
    mako     = c("#DEF5E5", "#49969D", "#35406C", "#0B0405"),
    rocket   = c("#FAEDD4", "#F28E2B", "#B63679", "#03051A"),
    viridis  = c("#FDE725", "#35B779", "#31688E", "#440154"),
    plasma   = c("#F0F921", "#E56B5D", "#900DA4", "#0D0887"),
    blues    = c("#EFF3FF", "#BDD7E7", "#6BAED6", "#2171B5", "#08519C"),
    reds     = c("#FEE5D9", "#FCAE91", "#FB6A4A", "#DE2D26", "#A50F15"),
    ylorrd   = c("#FFFFB2", "#FECC5C", "#FD8D3C", "#F03B20", "#BD0026"),
    purples  = c("#F2F0F7", "#CBC9E2", "#9E9AC8", "#756BB1", "#54278F"),
    classic  = c(fill_low, fill_mid, fill_high)
  )

  cliplot_type(
    data = function(settings, ...) {
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

      # Optional explicit significance metric
      if (!is.null(settings$extra_data) && !is.null(settings$extra_data$pvalue)) {
        df$pval <- as.numeric(settings$extra_data$pvalue)
      } else if (!is.null(settings$extra_data) && !is.null(settings$extra_data$padj)) {
        df$pval <- as.numeric(settings$extra_data$padj)
      } else if (!is.null(settings$extra_data) && !is.null(settings$extra_data$signif)) {
        df$pval <- as.numeric(settings$extra_data$signif)
      } else {
        df$pval <- NA_real_
      }

      seen_r <- character(0); cats_r <- character(0)
      for (r in df$row) { if (!r %in% seen_r) { seen_r <- c(seen_r, r); cats_r <- c(cats_r, r) } }
      seen_c <- character(0); cats_c <- character(0)
      for (c in df$col) { if (!c %in% seen_c) { seen_c <- c(seen_c, c); cats_c <- c(cats_c, c) } }

      df$row <- factor(df$row, levels = rev(cats_r))
      df$col <- factor(df$col, levels = cats_c)

      # Build labels based on label_metric
      if (lbl_met == "value") {
        df$label <- ifelse(
          !is.na(df$size) & df$size >= label_threshold,
          format(round(df$size, 2), nsmall = 2),
          ""
        )
      } else if (lbl_met == "signif") {
        # Check either df$pval or infer from df$fill if df$pval is missing
        p_eval <- if (!all(is.na(df$pval))) df$pval else df$fill
        # If values look like -log10(p), invert them to nominal p
        if (all(is.na(df$pval)) && max(p_eval, na.rm = TRUE) > 1) {
          # Assume -log10 P: higher is more significant
          cut_log <- -log10(signif_cutoffs)
          lbls <- rep("", nrow(df))
          for (k in seq_along(cut_log)) {
            lbls[!is.na(p_eval) & p_eval >= cut_log[k]] <- signif_labels[k]
          }
          df$label <- lbls
        } else {
          # Raw P-values: lower is more significant
          lbls <- rep("", nrow(df))
          for (k in seq_along(signif_cutoffs)) {
            lbls[!is.na(p_eval) & p_eval <= signif_cutoffs[k]] <- signif_labels[k]
          }
          df$label <- lbls
        }
      } else {
        df$label <- ""
      }

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

      settings$bh_df           <- df
      settings$bh_shape        <- shape_val
      settings$bh_pal_type     <- pal_type
      settings$bh_palette      <- palette
      settings$bh_fill_low     <- fill_low
      settings$bh_fill_mid     <- fill_mid
      settings$bh_fill_high    <- fill_high
      settings$bh_midpoint     <- midpoint
      settings$bh_limits       <- limits
      settings$bh_min_size     <- min_size
      settings$bh_max_size     <- max_size
      settings$bh_stroke_col   <- stroke_col
      settings$bh_stroke_width <- stroke_width
      settings$bh_point_alpha  <- point_alpha
      settings$bh_grid_style   <- g_style
      settings$bh_band_alpha   <- band_alpha
      settings$bh_band_fill    <- band_fill
      settings$bh_cell_border  <- cell_border
      settings$bh_label_size   <- label_size
      settings$bh_label_col    <- label_col
      settings$bh_size_name    <- size_name
      settings$bh_fill_name    <- fill_name
      settings$bh_axis_x_size  <- adaptive_x_size
      settings$bh_axis_y_size  <- adaptive_y_size
      settings$bh_axis_angle   <- axis_text_angle
      settings$by              <- NULL
    },
    draw = function(data, mapping, settings, ...) {
      df <- settings$bh_df
      if (is.null(df)) return(ggplot2::ggplot())

      p <- ggplot2::ggplot(df, ggplot2::aes(x = .data[["col"]], y = .data[["row"]]))

      # Background matrix styling
      g_style <- settings$bh_grid_style
      if (g_style == "banded") {
        n_rows <- length(levels(df$row))
        band_ymin <- seq(0.5, n_rows + 0.5, by = 2)
        band_ymax <- seq(1.5, n_rows + 1.5, by = 2)
        p <- p + ggplot2::annotate("rect", xmin = -Inf, xmax = Inf, ymin = band_ymin, ymax = band_ymax,
          fill = settings$bh_band_fill, alpha = settings$bh_band_alpha) +
          ggplot2::geom_tile(fill = NA, colour = settings$bh_cell_border, linewidth = 0.35)
      } else if (g_style == "grid") {
        p <- p + ggplot2::geom_tile(fill = NA, colour = settings$bh_cell_border, linewidth = 0.35)
      } else if (g_style == "minimal") {
        p <- p + ggplot2::geom_tile(fill = NA, colour = "grey93", linewidth = 0.25)
      }

      # Bubbles layer
      p <- p + ggplot2::geom_point(
        ggplot2::aes(size = .data[["size"]], fill = .data[["fill"]]),
        shape  = settings$bh_shape,
        colour = settings$bh_stroke_col,
        stroke = settings$bh_stroke_width,
        alpha  = settings$bh_point_alpha
      )

      # Optional label overlay
      if (any(nzchar(df$label))) {
        p <- p + ggplot2::geom_text(
          ggplot2::aes(label = .data[["label"]]),
          size     = settings$bh_label_size,
          fontface = "bold",
          colour   = settings$bh_label_col
        )
      }

      # Size scale
      p <- p + ggplot2::scale_size_area(
        max_size = settings$bh_max_size,
        limits   = c(0, max(df$size, na.rm = TRUE)),
        name     = settings$bh_size_name,
        guide    = ggplot2::guide_legend(
          order = 2,
          keywidth = ggplot2::unit(12, "pt"),
          keyheight = ggplot2::unit(12, "pt"),
          override.aes = list(fill = "grey40", colour = "grey40", shape = settings$bh_shape)
        )
      )

      # Color scale
      if (settings$bh_pal_type == "diverging") {
        div_cols <- if (is.character(settings$bh_palette) && length(settings$bh_palette) == 1) {
          pal_div_map[[tolower(settings$bh_palette)]] %||% pal_div_map$rdbu
        } else if (is.character(settings$bh_palette) && length(settings$bh_palette) >= 3) {
          settings$bh_palette
        } else {
          c(settings$bh_fill_low, settings$bh_fill_mid, settings$bh_fill_high)
        }

        # Symmetric limits if not provided
        lims <- settings$bh_limits
        if (is.null(lims)) {
          max_abs <- max(abs(df$fill), na.rm = TRUE)
          lims <- c(-max_abs, max_abs)
        }

        p <- p + ggplot2::scale_fill_gradient2(
          low      = div_cols[1],
          mid      = div_cols[2],
          high     = div_cols[3],
          midpoint = settings$bh_midpoint,
          limits   = lims,
          oob      = scales::squish,
          name     = settings$bh_fill_name,
          guide    = ggplot2::guide_colorbar(
            order = 1,
            barwidth = ggplot2::unit(8, "pt"),
            barheight = ggplot2::unit(60, "pt"),
            title.position = "top",
            title.hjust = 0.5
          )
        )
      } else {
        # Sequential scale
        seq_cols <- if (is.character(settings$bh_palette) && length(settings$bh_palette) == 1) {
          pal_seq_map[[tolower(settings$bh_palette)]] %||% pal_seq_map$classic
        } else if (is.character(settings$bh_palette) && length(settings$bh_palette) >= 2) {
          settings$bh_palette
        } else {
          c(settings$bh_fill_low, settings$bh_fill_mid, settings$bh_fill_high)
        }

        p <- p + ggplot2::scale_fill_gradientn(
          colours = seq_cols,
          limits  = settings$bh_limits,
          name    = settings$bh_fill_name,
          guide   = ggplot2::guide_colorbar(
            order = 1,
            barwidth = ggplot2::unit(8, "pt"),
            barheight = ggplot2::unit(60, "pt"),
            title.position = "top",
            title.hjust = 0.5
          )
        )
      }

      p <- p +
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

#' Diamond Bubble Heatmap Archetype
#'
#' @inheritParams type_bubble_heatmap
#' @return A \code{cliplot_type} object.
#' @export
type_bubble_diamond = function(...) {
  type_bubble_heatmap(shape = "diamond", ...)
}

#' Circular Dot Bubble Heatmap Archetype
#'
#' @inheritParams type_bubble_heatmap
#' @return A \code{cliplot_type} object.
#' @export
type_bubble_circle = function(
    stroke_col   = "grey30",
    stroke_width = 0.4,
    grid_style   = "minimal",
    ...
) {
  type_bubble_heatmap(
    shape        = "circle",
    stroke_col   = stroke_col,
    stroke_width = stroke_width,
    grid_style   = grid_style,
    ...
  )
}

#' Square Tile Proportional Bubble Heatmap Archetype
#'
#' @inheritParams type_bubble_heatmap
#' @return A \code{cliplot_type} object.
#' @export
type_bubble_square = function(
    stroke_col   = "white",
    stroke_width = 0.5,
    grid_style   = "grid",
    ...
) {
  type_bubble_heatmap(
    shape        = "square",
    stroke_col   = stroke_col,
    stroke_width = stroke_width,
    grid_style   = grid_style,
    ...
  )
}

#' Divergent Effect-Size Bubble Heatmap Archetype
#'
#' @inheritParams type_bubble_heatmap
#' @return A \code{cliplot_type} object.
#' @export
type_bubble_diverging = function(
    palette      = "nature",
    shape        = "circle",
    midpoint     = 0,
    stroke_col   = "grey35",
    stroke_width = 0.35,
    fill_name    = expression(log[2] ~ "FC"),
    ...
) {
  type_bubble_heatmap(
    shape        = shape,
    palette_type = "diverging",
    palette      = palette,
    midpoint     = midpoint,
    stroke_col   = stroke_col,
    stroke_width = stroke_width,
    fill_name    = fill_name,
    ...
  )
}
