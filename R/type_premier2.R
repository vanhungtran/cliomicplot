# ===========================================================================
# cliomicplot Premier: Radar, Alluvial, Waffle, Interactive
# ===========================================================================

#' Radar / Spider Chart Type
#'
#' @description Creates stunning radar (spider) charts for multivariate
#'   comparisons across groups. Perfect for profile visualization.
#'
#' @param alpha Fill transparency (default 0.25)
#' @param linewidth Line width (default 1)
#' @param gridline_width Grid line width (default 0.3)
#' @param show_legend Show legend (default TRUE)
#'
#' @return A cliplot_type object
#' @export
type_radar = function(alpha = 0.25, linewidth = 1, gridline_width = 0.3,
                       show_legend = TRUE) {
  cliplot_type(
    data = function(settings, ...) {
      df = settings$data
      if (is.null(df)) stop("Radar charts need a data frame with variables as columns and groups as rows")
      # Expect: first column = group labels, remaining = variables
      settings$radar_data = df
    },
    draw = function(data, mapping, settings, ...) {
      df = settings$radar_data
      if (is.null(df)) df = data

      group_col = names(df)[1]
      var_cols  = names(df)[-1]
      n_vars    = length(var_cols)

      # Reshape to long format
      radar_long = stats::reshape(
        df, direction = "long",
        varying = var_cols,
        v.names = "value",
        timevar = "variable",
        times = var_cols,
        idvar = group_col
      )
      radar_long$variable = factor(radar_long$variable, levels = var_cols)

      p = ggplot2::ggplot(radar_long, ggplot2::aes(
        x = .data[["variable"]], y = .data[["value"]],
        group = .data[[group_col]], color = .data[[group_col]],
        fill = .data[[group_col]]
      )) +
        # Grid lines
        ggplot2::geom_hline(
          yintercept = seq(0, max(radar_long$value, na.rm = TRUE), length.out = 5),
          color = "grey85", linewidth = gridline_width
        ) +
        # Filled polygon
        ggplot2::geom_polygon(alpha = alpha, linewidth = linewidth) +
        # Points at vertices
        ggplot2::geom_point(size = 3) +
        # Coordinate system
        ggplot2::coord_polar(start = 0) +
        ggplot2::labs(x = "", y = "", color = group_col, fill = group_col) +
        ggplot2::theme_minimal() +
        ggplot2::theme(
          axis.text.x = ggplot2::element_text(size = 10, face = "bold"),
          panel.grid.major = ggplot2::element_blank(),
          legend.position = if (show_legend) "right" else "none"
        )

      p
    },
    name = "radar"
  )
}

#' Alluvial / Sankey Diagram Type
#'
#' @description Creates flow diagrams showing transitions between categorical
#'   states. Perfect for patient journeys, customer flows, or any sequential
#'   categorical data.
#'
#' @param alpha Flow transparency (default 0.6)
#' @param curve_type Curve type: "sigmoid", "linear", "cubic"
#' @param node_width Node width (default 0.15)
#' @param show_labels Show axis labels (default TRUE)
#'
#' @return A cliplot_type object
#' @export
type_alluvial = function(alpha = 0.6, curve_type = "sigmoid",
                          node_width = 0.15, show_labels = TRUE) {
  cliplot_type(
    data = function(settings, ...) {
      df = settings$data
      if (is.null(df)) stop("Alluvial diagrams need a data frame with categorical columns")
      settings$alluvial_data = df
    },
    draw = function(data, mapping, settings, ...) {
      df = settings$alluvial_data
      if (is.null(df)) df = data

      if (requireNamespace("ggalluvial", quietly = TRUE)) {
        # Count frequencies
        axis_cols = names(df)
        freq_df = df
        freq_df$freq = 1
        freq_agg = stats::aggregate(
          freq ~ ., data = freq_df, FUN = sum
        )

        p = ggplot2::ggplot(freq_agg, ggplot2::aes(
          y = .data[["freq"]], axis1 = .data[[axis_cols[1]]]
        ))

        # Add subsequent axes
        for (i in 2:length(axis_cols)) {
          p = p + ggplot2::aes(!!paste0("axis", i) := .data[[axis_cols[i]]])
        }

        if (requireNamespace("ggalluvial", quietly = TRUE)) {
          flow_args = list(
            ggplot2::aes(fill = .data[[axis_cols[1]]]),
            alpha = alpha, width = node_width
          )
          if (curve_type == "sigmoid") {
            p = p + do.call(ggalluvial::geom_flow, flow_args) +
              do.call(ggalluvial::geom_stratum, list(alpha = 0.8, width = node_width))
          } else {
            p = p + do.call(ggalluvial::geom_alluvium, flow_args) +
              do.call(ggalluvial::geom_stratum, list(alpha = 0.8, width = node_width))
          }
        }

        if (show_labels) {
          p = p + ggplot2::geom_text(
            ggplot2::aes(label = ggplot2::after_stat(stratum)),
            stat = "stratum", size = 3
          )
        }

        p = p + ggplot2::labs(x = "", y = "Count") +
          ggplot2::theme_minimal() +
          ggplot2::theme(
            panel.grid = ggplot2::element_blank(),
            axis.text.x = ggplot2::element_text(size = 10, face = "bold"),
            legend.position = "none"
          )

      } else {
        # Fallback: grouped bar chart
        bar_long = stats::reshape(
          df, direction = "long",
          varying = names(df),
          v.names = "category",
          timevar = "step",
          times = names(df)
        )
        bar_agg = as.data.frame(table(bar_long$step, bar_long$category))
        names(bar_agg) = c("step", "category", "count")

        p = ggplot2::ggplot(bar_agg, ggplot2::aes(
          x = .data[["step"]], y = .data[["count"]], fill = .data[["category"]]
        )) +
          ggplot2::geom_col(position = "fill", alpha = 0.85) +
          ggplot2::scale_y_continuous(labels = scales::percent) +
          ggplot2::labs(x = "", y = "Proportion", fill = "") +
          ggplot2::theme_minimal()
      }

      p
    },
    name = "alluvial"
  )
}

#' Waffle Chart Type
#'
#' @description Creates stunning waffle (square pie) charts for proportional
#'   data. Each square represents a unit, making proportions intuitive.
#'
#' @param rows Number of rows in the waffle grid (default 10)
#' @param colors Vector of fill colors
#' @param size Square size (default 1.5)
#' @param pad Padding between squares (default 0.1)
#'
#' @return A cliplot_type object
#' @export
type_waffle = function(rows = 10, colors = NULL, size = 1.5, pad = 0.1) {
  cliplot_type(
    data = function(settings, ...) {
      df = settings$data
      if (is.null(df)) {
        df = data.frame(
          category = settings$x %||% names(settings$y),
          value = settings$y
        )
      }
      settings$waffle_data = df
    },
    draw = function(data, mapping, settings, ...) {
      df = settings$waffle_data
      if (is.null(df)) df = data

      cat_col = names(df)[1]
      val_col = names(df)[2]

      # Scale to integer counts
      total = sum(df[[val_col]])
      df$pct = round(df[[val_col]] / total * 100)
      df = df[df$pct > 0, ]

      # Build waffle grid
      waffle_vec = unlist(lapply(seq_len(nrow(df)), function(i) {
        rep(df[[cat_col]][i], df$pct[i])
      }))

      # Create grid positions
      n = length(waffle_vec)
      cols = ceiling(n / rows)
      waffle_df = expand.grid(x = 1:cols, y = 1:rows)
      waffle_df = waffle_df[1:n, ]
      waffle_df$category = as.character(waffle_vec)
      waffle_df$category = factor(waffle_df$category, levels = unique(waffle_vec))

      cols_use = colors %||% get_cli_palette(
        settings$palette %||% "jco", length(unique(waffle_df$category))
      )

      p = ggplot2::ggplot(waffle_df, ggplot2::aes(
        x = .data[["x"]], y = .data[["y"]], fill = .data[["category"]]
      )) +
        ggplot2::geom_tile(color = "white", linewidth = pad, size = size) +
        ggplot2::scale_fill_manual(values = cols_use[1:length(unique(waffle_df$category))]) +
        ggplot2::coord_equal() +
        ggplot2::labs(x = "", y = "", fill = cat_col) +
        ggplot2::theme_void() +
        ggplot2::theme(
          legend.position = "bottom",
          legend.title = ggplot2::element_text(size = 10)
        )

      p
    },
    name = "waffle"
  )
}

#' Interactive Plot Mode
#'
#' @description Converts a cliomicplot ggplot to an interactive plotly chart
#'   with hover tooltips, zoom, and pan support.
#'
#' @param p A ggplot object produced by \code{\link{cliplot}}.
#' @param tooltip Columns to show in hover tooltips (default: all mapped)
#' @param width Plot width (NULL = auto)
#' @param height Plot height (NULL = auto)
#'
#' @return A plotly interactive plot object
#' @export
cliplot_interactive = function(p, tooltip = NULL, width = NULL, height = NULL) {
  if (!requireNamespace("plotly", quietly = TRUE)) {
    warning("Package 'plotly' is required for interactive plots. Install with install.packages('plotly')")
    return(p)
  }

  plotly::ggplotly(p, tooltip = tooltip, width = width, height = height)
}
