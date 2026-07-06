# ===========================================================================
# cliomicplot Gallery-Inspired: Chord, Treemap, Streamgraph
# Stunning visualization types from the R Graph Gallery
# Uses simulated data for demonstrations
# ===========================================================================

#' Chord Diagram Type
#'
#' @description Creates stunning chord diagrams for visualizing relationships
#'   and flows between categories. Uses circlize under the hood. Perfect for
#'   showing patient transitions, gene interactions, or any pairwise flow data.
#'
#' @param alpha Grid color transparency (default 0.5)
#' @param gap.degree Gap between sectors in degrees (default 4)
#' @param start.degree Starting angle (default 90)
#' @param directional Whether arcs have direction arrows (default TRUE)
#' @param link.alpha Transparency of links (default 0.5)
#'
#' @return A cliplot_type object
#' @export
type_chord = function(alpha = 0.5, gap.degree = 4, start.degree = 90,
                       directional = TRUE, link.alpha = 0.5) {
  cliplot_type(
    data = function(settings, ...) {
      df = settings$data
      if (is.null(df)) stop("Chord diagrams need a matrix or data frame of flow data")
      if (is.data.frame(df)) {
        # Expect: from, to, value columns
        if (ncol(df) >= 3) {
          mat = stats::xtabs(df[[3]] ~ df[[1]] + df[[2]])
          settings$chord_mat = as.matrix(mat)
        } else {
          settings$chord_mat = as.matrix(df)
        }
      } else {
        settings$chord_mat = as.matrix(df)
      }
    },
    draw = function(data, mapping, settings, ...) {
      mat = settings$chord_mat
      if (is.null(mat)) return(ggplot2::ggplot())

      if (requireNamespace("circlize", quietly = TRUE)) {
        # Generate colors
        n = nrow(mat)
        cols = get_cli_palette(settings$palette %||% "jco", n)

        circlize::chordDiagram(mat,
          grid.col = stats::setNames(cols[1:n], rownames(mat)),
          transparency = link.alpha,
          directional = if (directional) 1 else 0,
          annotationTrack = c("name", "grid"),
          preAllocateTracks = list(track.height = 0.05)
        )

        if (directional) {
          circlize::circos.track(track.index = 1, panel.fun = function(x, y) {
            circlize::circos.text(circlize::CELL_META$xcenter,
              circlize::CELL_META$ylim[1],
              circlize::CELL_META$sector.index,
              facing = "clockwise", niceFacing = TRUE, adj = c(0, 0.5), cex = 0.8)
          }, bg.border = NA)
        }

        # Return empty ggplot for API consistency
        return(ggplot2::ggplot())
      } else {
        # Fallback: heatmap of the matrix
        melted = reshape2::melt(mat)
        names(melted) = c("from", "to", "value")
        p = ggplot2::ggplot(melted, ggplot2::aes(.data[["to"]], .data[["from"]], fill = .data[["value"]])) +
          ggplot2::geom_tile(color = "white", linewidth = 0.5) +
          ggplot2::scale_fill_viridis_c() +
          ggplot2::labs(x = "To", y = "From", fill = "Flow") +
          ggplot2::theme_minimal()
        return(p)
      }
    },
    name = "chord"
  )
}

#' Treemap Type
#'
#' @description Creates treemaps for visualizing hierarchical proportional data.
#'   Rectangles are sized by value and colored by category. Perfect for
#'   showing composition of budgets, genomic features, or any nested data.
#'
#' @param alpha Fill transparency (default 0.85)
#' @param border_color Border color for rectangles (default "white")
#' @param border_width Border width (default 1.5)
#' @param show_labels Show category labels (default TRUE)
#' @param label_size Label text size (default 3)
#'
#' @return A cliplot_type object
#' @export
type_treemap = function(alpha = 0.85, border_color = "white", border_width = 1.5,
                         show_labels = TRUE, label_size = 3) {
  cliplot_type(
    data = function(settings, ...) {
      df = settings$data
      if (is.null(df)) stop("Treemaps need a data frame with category and value columns")
      settings$tm_data = df
    },
    draw = function(data, mapping, settings, ...) {
      df = settings$tm_data
      if (is.null(df)) df = data

      cat_col = names(df)[1]
      val_col = names(df)[2]
      group_col = if (ncol(df) >= 3) names(df)[3] else NULL

      if (requireNamespace("treemapify", quietly = TRUE)) {
        p = ggplot2::ggplot(df, ggplot2::aes(
          area = .data[[val_col]],
          fill = .data[[cat_col]],
          subgroup = if (!is.null(group_col)) .data[[group_col]] else .data[[cat_col]]
        )) +
          treemapify::geom_treemap(alpha = alpha, color = border_color, size = border_width)

        if (show_labels) {
          p = p + treemapify::geom_treemap_text(
            ggplot2::aes(label = .data[[cat_col]]),
            color = "white", size = label_size, place = "centre", grow = TRUE
          )
        }

        p = p + ggplot2::theme_void() +
          ggplot2::theme(legend.position = "none")

      } else {
        # Fallback: waffle-like grid
        n = 10
        total = sum(df[[val_col]])
        df$pct = round(df[[val_col]] / total * n * n)
        waffle_vec = unlist(lapply(seq_len(nrow(df)), function(i) {
          rep(df[[cat_col]][i], df$pct[i])
        }))
        grid_df = expand.grid(x = 1:ceiling(sqrt(length(waffle_vec))),
                              y = 1:ceiling(sqrt(length(waffle_vec))))
        grid_df = grid_df[1:length(waffle_vec), ]
        grid_df$cat = waffle_vec

        p = ggplot2::ggplot(grid_df, ggplot2::aes(.data[["x"]], .data[["y"]], fill = .data[["cat"]])) +
          ggplot2::geom_tile(color = "white", linewidth = 0.3) +
          ggplot2::coord_equal() +
          ggplot2::theme_void() +
          ggplot2::theme(legend.position = "bottom")
      }

      p
    },
    name = "treemap"
  )
}

#' Streamgraph / Streamchart Type
#'
#' @description Creates beautiful streamgraphs (flowing stacked area charts)
#'   for visualizing changes in composition over time. Uses ggstream for
#'   smooth, organic curves.
#'
#' @param alpha Fill transparency (default 0.8)
#' @param bw Bandwidth for smoothing (default 0.75)
#' @param sorting How to sort layers: "none", "onset", "insideout"
#' @param interpolate Interpolation type: "linear", "basis", "cardinal"
#'
#' @return A cliplot_type object
#' @export
type_streamgraph = function(alpha = 0.8, bw = 0.75,
                             sorting = "none", interpolate = "basis") {
  cliplot_type(
    data = function(settings, ...) {
      df = settings$data
      if (is.null(df)) stop("Streamgraphs need a data frame with time, category, and value columns")
      # If by is provided, use ..by.. as category
      if (!is.null(settings$by)) {
        df = data.frame(
          time = settings$x %||% df[[1]],
          category = settings$by,
          value = settings$y %||% df[[2]]
        )
        names(df) = c("x", "..by..", "y")
      }
      settings$stream_data = df
    },
    draw = function(data, mapping, settings, ...) {
      df = settings$stream_data
      if (is.null(df)) df = data

      # Determine columns
      if ("..by.." %in% names(df) && "x" %in% names(df) && "y" %in% names(df)) {
        time_col = "x"
        cat_col  = "..by.."
        val_col  = "y"
      } else {
        time_col = names(df)[1]
        cat_col  = names(df)[2]
        val_col  = names(df)[3]
      }

      df[[cat_col]] = factor(df[[cat_col]])

      if (requireNamespace("ggstream", quietly = TRUE)) {
        p = ggplot2::ggplot(df, ggplot2::aes(
          x = .data[[time_col]], y = .data[[val_col]],
          fill = .data[[cat_col]]
        )) +
          ggstream::geom_stream(
            type = interpolate, bw = bw,
            sorting = sorting, alpha = alpha
          ) +
          ggplot2::labs(x = time_col, y = val_col) +
          ggplot2::theme_minimal() +
          ggplot2::theme(
            legend.position = "bottom",
            panel.grid.minor = ggplot2::element_blank()
          )

      } else {
        # Fallback: stacked area
        p = ggplot2::ggplot(df, ggplot2::aes(
          x = .data[[time_col]], y = .data[[val_col]],
          fill = .data[[cat_col]]
        )) +
          ggplot2::geom_area(alpha = alpha, position = "stack") +
          ggplot2::labs(x = time_col, y = val_col) +
          ggplot2::theme_minimal() +
          ggplot2::theme(legend.position = "bottom")
      }

      p
    },
    name = "streamgraph"
  )
}
