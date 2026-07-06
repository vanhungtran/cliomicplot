# ===========================================================================
# cliomicplot Gallery-Inspired 2: Connected Scatter, Circular Barplot,
# 2D Density, Parallel Coordinates, Dendrogram
# ===========================================================================

#' Connected Scatterplot Type
#'
#' @description Creates connected scatterplots showing the path of paired
#'   observations over time or sequence. Each point is connected to the next
#'   in order, revealing trajectories and phase changes.
#'
#' @param point_size Point size (default 3)
#' @param linewidth Connecting line width (default 0.6)
#' @param point_alpha Point transparency
#' @param line_alpha Line transparency
#' @param show_arrows Show direction arrows (default FALSE)
#'
#' @return A cliplot_type object
#' @export
type_connected = function(point_size = 3, linewidth = 0.6,
                           point_alpha = 0.9, line_alpha = 0.6,
                           show_arrows = FALSE) {
  cliplot_type(
    data = function(settings, ...) {
      df = settings$data
      if (is.null(df)) df = data.frame(x = settings$x, y = settings$y)
      else df = data.frame(x = settings$x %||% df[[1]], y = settings$y %||% df[[2]])
      if (!is.null(settings$by)) df$..by.. = as.factor(settings$by)
      settings$conn_data = df
    },
    draw = function(data, mapping, settings, ...) {
      df = settings$conn_data
      if (is.null(df)) df = data

      p = ggplot2::ggplot(df, ggplot2::aes(x = .data[["x"]], y = .data[["y"]])) +
        ggplot2::geom_path(
          linewidth = linewidth, alpha = line_alpha,
          color = settings$col.default %||% "#2B6CB0"
        ) +
        ggplot2::geom_point(
          size = point_size, alpha = point_alpha,
          color = settings$col.default %||% "#2B6CB0"
        )

      if (!is.null(settings$by)) {
        p = ggplot2::ggplot(df, ggplot2::aes(
          x = .data[["x"]], y = .data[["y"]],
          color = .data[["..by.."]], group = .data[["..by.."]]
        )) +
          ggplot2::geom_path(linewidth = linewidth, alpha = line_alpha) +
          ggplot2::geom_point(size = point_size, alpha = point_alpha)
      }

      p
    },
    name = "connected"
  )
}

#' Circular Barplot Type
#'
#' @description Creates eye-catching circular barplots where bars radiate
#'   from the center. Perfect for ranking visualizations with many categories
#'   and a dramatic visual impact.
#'
#' @param bar_width Width of bars in degrees (default 0.8)
#' @param inner_radius Inner radius of the circle (default 0.3)
#' @param show_labels Show bar labels (default TRUE)
#' @param label_size Label text size (default 3)
#'
#' @return A cliplot_type object
#' @export
type_circular_bar = function(bar_width = 0.8, inner_radius = 0.3,
                              show_labels = TRUE, label_size = 3) {
  cliplot_type(
    data = function(settings, ...) {
      df = settings$data
      if (is.null(df)) df = data.frame(
        label = settings$x %||% as.character(seq_along(settings$y)),
        value = settings$y
      )
      else df = data.frame(label = settings$x %||% df[[1]], value = settings$y %||% df[[2]])
      df$label = factor(df$label, levels = df$label[order(df$value)])
      df$id = seq_len(nrow(df))
      settings$cb_data = df
    },
    draw = function(data, mapping, settings, ...) {
      df = settings$cb_data
      if (is.null(df)) df = data

      n = nrow(df)
      angle = 90 - 360 * (df$id - 0.5) / n
      df$hjust = ifelse(angle < -90, 1, 0)
      df$angle = ifelse(angle < -90, angle + 180, angle)

      p = ggplot2::ggplot(df, ggplot2::aes(
        x = .data[["label"]], y = .data[["value"]], fill = .data[["label"]]
      )) +
        ggplot2::geom_col(width = bar_width, alpha = 0.9, color = "white", linewidth = 0.2) +
        ggplot2::coord_polar(start = 0) +
        ggplot2::ylim(-max(df$value) * inner_radius, max(df$value) * 1.1) +
        ggplot2::theme_minimal() +
        ggplot2::theme(
          axis.text.x = ggplot2::element_blank(),
          axis.title = ggplot2::element_blank(),
          panel.grid = ggplot2::element_blank(),
          legend.position = "none"
        )

      if (show_labels) {
        p = p + ggplot2::geom_text(
          ggplot2::aes(
            y = .data[["value"]] + max(df$value) * 0.05,
            label = .data[["label"]],
            angle = .data[["angle"]],
            hjust = .data[["hjust"]]
          ),
          size = label_size, color = "grey30"
        )
      }

      p
    },
    name = "circular_bar"
  )
}

#' 2D Density / Contour Plot Type
#'
#' @description Creates a 2D density plot showing the joint distribution of
#'   two continuous variables. Includes filled contours and optional scatter
#'   overlay. An elegant alternative to overplotted scatterplots.
#'
#' @param bins Number of contour bins (default 10)
#' @param alpha Fill transparency
#' @param show_points Overlay scatter points (default FALSE)
#' @param point_size Point size for overlay
#' @param point_alpha Point transparency for overlay
#'
#' @return A cliplot_type object
#' @export
type_density2d = function(bins = 10, alpha = 0.6, show_points = FALSE,
                           point_size = 1, point_alpha = 0.3) {
  cliplot_type(
    data = function(settings, ...) {
      df = settings$data
      if (is.null(df)) df = data.frame(x = settings$x, y = settings$y)
      else df = data.frame(x = settings$x %||% df[[1]], y = settings$y %||% df[[2]])
      settings$d2d_data = df
    },
    draw = function(data, mapping, settings, ...) {
      df = settings$d2d_data
      if (is.null(df)) df = data

      p = ggplot2::ggplot(df, ggplot2::aes(x = .data[["x"]], y = .data[["y"]])) +
        ggplot2::stat_density_2d(
          ggplot2::aes(fill = ggplot2::after_stat(level)),
          geom = "polygon", bins = bins, alpha = alpha
        ) +
        ggplot2::scale_fill_viridis_c(option = "magma") +
        ggplot2::labs(fill = "Density") +
        ggplot2::theme_minimal()

      if (show_points) {
        p = p + ggplot2::geom_point(size = point_size, alpha = point_alpha, color = "white")
      }

      p
    },
    name = "density2d"
  )
}

#' Parallel Coordinates Plot Type
#'
#' @description Creates parallel coordinate plots for multivariate data
#'   exploration. Each variable is shown as a vertical axis, and each
#'   observation is a line connecting its values across all axes. Colored
#'   by a grouping variable.
#'
#' @param alpha Line transparency (default 0.5)
#' @param linewidth Line width (default 0.4)
#' @param box_alpha Box plot alpha on axes (default 0.3)
#'
#' @return A cliplot_type object
#' @export
type_parallel = function(alpha = 0.5, linewidth = 0.4, box_alpha = 0.3) {
  cliplot_type(
    data = function(settings, ...) {
      df = settings$data
      if (is.null(df)) stop("Parallel coordinates need a data frame with numeric columns and optional group column")
      settings$pc_data = df
    },
    draw = function(data, mapping, settings, ...) {
      df = settings$pc_data
      if (is.null(df)) df = data

      # Assume first column may be group, rest numeric
      all_num = all(vapply(df, is.numeric, logical(1)))
      if (all_num || ncol(df) < 2) stop("Need at least 2 columns, one may be grouping")

      group_col = names(df)[1]
      num_cols  = names(df)[-1]
      df$id = seq_len(nrow(df))

      # Scale numeric columns
      for (cn in num_cols) {
        rng = range(df[[cn]], na.rm = TRUE)
        if (rng[2] > rng[1]) df[[cn]] = (df[[cn]] - rng[1]) / (rng[2] - rng[1])
        else df[[cn]] = 0.5
      }

      # Reshape to long
      pc_long = stats::reshape(df, direction = "long",
        varying = num_cols, v.names = "value",
        timevar = "variable", times = num_cols,
        idvar = "id")
      pc_long$variable = factor(pc_long$variable, levels = num_cols)
      pc_long[[group_col]] = factor(pc_long[[group_col]])

      p = ggplot2::ggplot(pc_long, ggplot2::aes(
        x = .data[["variable"]], y = .data[["value"]],
        group = .data[["id"]], color = .data[[group_col]]
      )) +
        ggplot2::geom_line(alpha = alpha, linewidth = linewidth) +
        ggplot2::labs(x = "", y = "Scaled Value", color = group_col) +
        ggplot2::theme_minimal() +
        ggplot2::theme(
          panel.grid.minor = ggplot2::element_blank(),
          legend.position = "bottom"
        )

      p
    },
    name = "parallel"
  )
}

#' Dendrogram Type
#'
#' @description Creates a dendrogram (hierarchical clustering tree) from
#'   data. Automatically clusters rows and displays the tree structure.
#'
#' @param dist_method Distance method: "euclidean", "manhattan", etc.
#' @param hclust_method Clustering method: "complete", "ward.D2", etc.
#' @param k Number of clusters to color (default NULL = no coloring)
#' @param linewidth Line width (default 0.6)
#' @param leaf_size Leaf label size (default 3)
#'
#' @return A cliplot_type object
#' @export
type_dendrogram = function(dist_method = "euclidean", hclust_method = "complete",
                            k = NULL, linewidth = 0.6, leaf_size = 3) {
  cliplot_type(
    data = function(settings, ...) {
      df = settings$data
      if (is.null(df)) stop("Dendrogram needs a matrix or data frame")
      if (is.data.frame(df)) df = as.matrix(df[, vapply(df, is.numeric, logical(1))])
      settings$dend_mat = df
    },
    draw = function(data, mapping, settings, ...) {
      mat = settings$dend_mat
      if (is.null(mat)) mat = as.matrix(data)

      d = stats::dist(mat, method = dist_method)
      hc = stats::hclust(d, method = hclust_method)

      if (requireNamespace("ggdendro", quietly = TRUE)) {
        dend_data = ggdendro::dendro_data(hc, type = "rectangle")

        p = ggplot2::ggplot(ggdendro::segment(dend_data)) +
          ggplot2::geom_segment(ggplot2::aes(
            x = .data[["x"]], y = .data[["y"]],
            xend = .data[["xend"]], yend = .data[["yend"]]
          ), linewidth = linewidth, color = "grey40") +
          ggplot2::geom_text(
            data = ggdendro::label(dend_data),
            ggplot2::aes(x = .data[["x"]], y = .data[["y"]],
                         label = .data[["label"]], hjust = 0),
            size = leaf_size, nudge_y = 0.2
          ) +
          ggplot2::coord_flip() +
          ggplot2::scale_y_reverse(expand = c(0.2, 0)) +
          ggplot2::theme_void()

      } else {
        # Fallback: base R dendrogram
        graphics::plot(stats::as.dendrogram(hc))
        p = ggplot2::ggplot() +
          ggplot2::annotate("text", x = 1, y = 1, label = "Dendrogram rendered in base R") +
          ggplot2::theme_void()
      }

      # Add cluster colors if k specified
      if (!is.null(k) && requireNamespace("ggdendro", quietly = TRUE)) {
        clust = stats::cutree(hc, k = k)
        dend_data$labels$cluster = factor(clust[dend_data$labels$label])
        p = p + ggplot2::geom_text(
          data = ggdendro::label(dend_data),
          ggplot2::aes(x = .data[["x"]], y = .data[["y"]],
                       label = .data[["label"]], hjust = 0,
                       color = .data[["cluster"]]),
          size = leaf_size, nudge_y = 0.2
        ) + ggplot2::theme(legend.position = "none")
      }

      p
    },
    name = "dendrogram"
  )
}
