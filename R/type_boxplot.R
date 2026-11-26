# ===========================================================================
# cliomicplot: Boxplot Type & Scatter-Box-Violin Composites
# With jittered points, statistical annotations, and publication styling
# Conforms to Nature Aging / SciFig standards (Yadav et al. 2026)
# and David Baranger (2018) Scatter-Box-Violin distribution principles
# ===========================================================================

#' Boxplot Type
#'
#' @description Creates publication-ready boxplots with optional jittered
#'   points, violin overlays, statistical annotations, and specialized styling.
#'   Supports classic cliplot (`"default"`), Nature Aging pathology style
#'   (`"pathology"`), and David Baranger (2018) scatter-box-violin composites
#'   (`"baranger"`, `"scatter_box_violin"`, `"scatter_box"`, `"box_violin"`).
#'
#' @param style Boxplot aesthetic style: `"default"` (classic cliplot),
#'   `"pathology"` (Nature Aging / SciFig style with colored borders, bold median,
#'   white diamond mean, dotted mean trajectory lines, and dashed zero baseline),
#'   `"baranger"` / `"scatter_box_violin"` (David Baranger style with wide violin,
#'   narrow notched boxplot, and overlaid jittered points with black border),
#'   `"scatter_box"` (notched boxplot with bordered jittered points), or
#'   `"box_violin"` (wide violin with narrow notched boxplot, no points).
#' @param add_jitter Add jittered points (default TRUE).
#' @param jitter_width Width of jitter (default 0.15 for default, 0.10 for pathology, 0.12 for baranger).
#' @param jitter_alpha Transparency of jittered points (default 0.38 for default, 0.35 for pathology, 0.65 for baranger).
#' @param jitter_size Size of jittered points (default 1.6 for default, 0.85 for pathology, 1.8 for baranger).
#' @param jitter_subsample Optional integer. When specified, subsamples up to
#'   `jitter_subsample` observations per category/group for jittered point display
#'   while computing boxplots and statistics on all data (ideal for large cohorts
#'   such as GTEx with N ~ 20k).
#' @param point_shape Point shape for raw data scatter points. `21` renders
#'   filled circles with black borders (default in Baranger style); `16` renders
#'   solid circular points (default in classic/pathology styles).
#' @param point_border Border outline color for points when `point_shape` is 21–25
#'   (default `"black"`).
#' @param point_stroke Border stroke thickness for points (default `0.35`).
#' @param add_violin Overlay violin plot (default FALSE for classic/pathology;
#'   default TRUE for Baranger style).
#' @param violin_alpha Violin transparency (default 0.20 for classic, 0.30 for baranger).
#' @param violin_width Width of violin layer (default `0.80` for baranger).
#' @param violin_color Outline color of violin layer (`"black"` for baranger, `NA` for classic).
#' @param violin_linewidth Line width of violin outline (default `0.55`).
#' @param trim Logical. Trim violin tails to observed data range (default TRUE).
#' @param scale Density scaling method for violin: `"width"` (default), `"area"`, or `"count"`.
#' @param notch Logical. Draw notched boxplots showing the 95% confidence interval
#'   around the median (default TRUE in Baranger styles, FALSE otherwise).
#' @param notchwidth Notch depth relative to box width (default `0.5`).
#' @param outlier_shape Shape for outlier points. Set to `NA` to hide
#'   outliers (publication style). Default `NA`.
#' @param outlier_size Size of outlier points (default 1.5).
#' @param width Box width (default 0.55 for default, 0.32 for pathology, 0.22 for baranger).
#' @param alpha Box fill transparency (default 0.28 for default, 0.22 for pathology, 0.75 for baranger).
#' @param linewidth Box outline width (default 0.65 for default, 0.45 for pathology).
#' @param box_color Custom outline color for boxplots (default `"black"` for baranger,
#'   `"#30343B"` for default, or group-matched for pathology).
#' @param position_dodge Dodge width for grouped boxes. Default 0.8 for default/baranger,
#'   0.42 for pathology style.
#' @param show_mean Logical. Show mean marker diamond (default `TRUE` when
#'   `style = "pathology"`, `FALSE` otherwise).
#' @param show_mean_line Logical. Show dotted connecting line between paired
#'   means within each x category (default `TRUE` when `style = "pathology"`, `FALSE` otherwise).
#' @param mean_shape Shape for mean marker (default `23`, diamond).
#' @param mean_fill Fill color for mean marker (default `"#FFFFFF"`, white).
#' @param mean_color Border color for mean marker (default `"black"`).
#' @param mean_size Size of mean marker (default `2.0`).
#' @param mean_stroke Border thickness for mean marker (default `0.45`).
#' @param mean_line_lty Linetype for paired mean connecting line (default `"dotted"`).
#' @param mean_line_col Color for paired mean connecting line (default `"grey45"`).
#' @param mean_line_lwd Linewidth for paired mean connecting line (default `0.38`).
#' @param bold_median Logical. Draw a bold black horizontal line across each box
#'   for the median (default `TRUE` when `style = "pathology"`, `FALSE` otherwise).
#' @param bold_median_col Color for bold median line (default `"#111111"`).
#' @param bold_median_lwd Linewidth for bold median line (default `0.85`).
#' @param zero_line Logical. Draw a horizontal reference line at y = 0 (default
#'   `TRUE` when `style = "pathology"`, `FALSE` otherwise).
#' @param zero_line_y Intercept for zero reference line (default `0`).
#' @param zero_line_col Color for zero reference line (default `"grey75"`).
#' @param zero_line_lty Linetype for zero reference line (default `"dashed"`).
#'
#' @return A cliplot_type object for use with \code{\link{cliplot}}.
#'
#' @references
#' Baranger, D. (2018). Showing your data: Scatter, box, & violin plots.
#' \url{https://davidbaranger.com/2018/03/05/showing-your-data-scatter-box-violin-plots/}
#'
#' @examples
#' \dontrun{
#' # Basic publication-style boxplot (transparent fill, no outliers)
#' cliplot(len ~ dose, data = ToothGrowth, type = "boxplot")
#'
#' # David Baranger style scatter-box-violin composite
#' cliplot(Sepal.Length ~ Species, data = iris,
#'         type = type_boxplot(style = "baranger"))
#'
#' # Nature Aging / SciFig pathology style boxplot
#' cliplot(delta ~ pathology | status, data = df_gtex,
#'         type = type_pathology_boxplot(),
#'         palette = "pathology", theme = "pathology")
#' }
#'
#' @export
type_boxplot = function(
    style            = c("default", "pathology", "baranger", "scatter_box_violin", "scatter_box", "box_violin"),
    add_jitter       = NULL,
    jitter_width     = NULL,
    jitter_alpha     = NULL,
    jitter_size      = NULL,
    jitter_subsample = NULL,
    point_shape      = NULL,
    point_border     = "black",
    point_stroke     = 0.35,
    add_violin       = NULL,
    violin_alpha     = NULL,
    violin_width     = NULL,
    violin_color     = NULL,
    violin_linewidth = 0.55,
    trim             = TRUE,
    scale            = "width",
    notch            = NULL,
    notchwidth       = 0.5,
    outlier_shape    = NA,
    outlier_size     = 1.5,
    width            = NULL,
    alpha            = NULL,
    linewidth        = NULL,
    box_color        = NULL,
    position_dodge   = NULL,
    show_mean        = NULL,
    show_mean_line   = NULL,
    mean_shape       = 23,
    mean_fill        = "#FFFFFF",
    mean_color       = "black",
    mean_size        = 2.0,
    mean_stroke      = 0.45,
    mean_line_lty    = "dotted",
    mean_line_col    = "grey45",
    mean_line_lwd    = 0.38,
    bold_median      = NULL,
    bold_median_col  = "#111111",
    bold_median_lwd  = 0.85,
    zero_line        = NULL,
    zero_line_y      = 0,
    zero_line_col    = "grey75",
    zero_line_lty    = "dashed"
) {
  if (is.character(style)) {
    style <- match.arg(style)
  } else {
    style <- "default"
  }
  is_pathology   <- identical(style, "pathology")
  is_baranger    <- style %in% c("baranger", "scatter_box_violin", "scatter_box", "box_violin")
  is_scatter_box <- identical(style, "scatter_box")
  is_box_violin  <- identical(style, "box_violin")

  # Style-specific defaults if not explicitly set
  if (is_baranger) {
    if (is.null(add_violin))       add_violin       <- if (is_scatter_box) FALSE else TRUE
    if (is.null(violin_alpha))     violin_alpha     <- 0.30
    if (is.null(violin_width))     violin_width     <- 0.80
    if (is.null(violin_color))     violin_color     <- "black"
    if (is.null(width))            width            <- if (is_scatter_box) 0.45 else 0.22
    if (is.null(alpha))            alpha            <- 0.75
    if (is.null(linewidth))        linewidth        <- 0.65
    if (is.null(box_color))        box_color        <- "black"
    if (is.null(notch))            notch            <- TRUE
    if (is.null(add_jitter))       add_jitter       <- if (is_box_violin) FALSE else TRUE
    if (is.null(jitter_width))     jitter_width     <- 0.12
    if (is.null(jitter_alpha))     jitter_alpha     <- 0.65
    if (is.null(jitter_size))      jitter_size      <- 1.8
    if (is.null(point_shape))      point_shape      <- 21
    if (is.null(position_dodge))   position_dodge   <- 0.80
  } else if (is_pathology) {
    if (is.null(add_violin))       add_violin       <- FALSE
    if (is.null(violin_alpha))     violin_alpha     <- 0.20
    if (is.null(violin_width))     violin_width     <- 0.38
    if (is.null(width))            width            <- 0.32
    if (is.null(alpha))            alpha            <- 0.22
    if (is.null(linewidth))        linewidth        <- 0.45
    if (is.null(notch))            notch            <- FALSE
    if (is.null(add_jitter))       add_jitter       <- TRUE
    if (is.null(jitter_width))     jitter_width     <- 0.10
    if (is.null(jitter_alpha))     jitter_alpha     <- 0.35
    if (is.null(jitter_size))      jitter_size      <- 0.85
    if (is.null(point_shape))      point_shape      <- 16
    if (is.null(position_dodge))   position_dodge   <- 0.42
  } else {
    # Default classic cliplot
    if (is.null(add_violin))       add_violin       <- FALSE
    if (is.null(violin_alpha))     violin_alpha     <- 0.20
    if (is.null(violin_width))     violin_width     <- 0.65
    if (is.null(width))            width            <- 0.55
    if (is.null(alpha))            alpha            <- 0.28
    if (is.null(linewidth))        linewidth        <- 0.65
    if (is.null(notch))            notch            <- FALSE
    if (is.null(add_jitter))       add_jitter       <- TRUE
    if (is.null(jitter_width))     jitter_width     <- 0.15
    if (is.null(jitter_alpha))     jitter_alpha     <- 0.38
    if (is.null(jitter_size))      jitter_size      <- 1.6
    if (is.null(point_shape))      point_shape      <- 16
    if (is.null(position_dodge))   position_dodge   <- 0.80
  }

  show_mean      <- show_mean %||% is_pathology
  show_mean_line <- show_mean_line %||% is_pathology
  bold_median    <- bold_median %||% is_pathology
  zero_line      <- zero_line %||% is_pathology

  cliplot_type(
    draw = function(data, mapping, settings, ...) {
      # Ensure categorical x for boxplots so numeric levels dodge cleanly
      if ("x" %in% names(data) && is.numeric(data[["x"]])) {
        data[["x"]] <- as.factor(data[["x"]])
      }

      p = ggplot2::ggplot(data, mapping)

      # Build position dodge if grouping is present and distinct from x
      is_grouped_dodge <- !is.null(settings$by) && !identical(as.character(data[["x"]]), as.character(data[["..by.."]]))
      dodge_width <- position_dodge %||% (if (is_pathology) 0.42 else 0.8)
      pos <- if (is_grouped_dodge) ggplot2::position_dodge(width = dodge_width) else ggplot2::position_identity()

      # Add zero reference line if requested (rendered behind data)
      if (isTRUE(zero_line)) {
        p = p + ggplot2::geom_hline(
          yintercept = zero_line_y,
          linetype   = zero_line_lty,
          color      = zero_line_col,
          linewidth  = 0.35
        )
      }

      # 1. Add violin if requested (rendered behind boxplot)
      if (isTRUE(add_violin)) {
        if (!is.null(settings$by)) {
          p = p + ggplot2::geom_violin(
            ggplot2::aes(fill = .data[["..by.."]]),
            alpha     = violin_alpha,
            width     = violin_width,
            color     = violin_color %||% NA,
            linewidth = violin_linewidth,
            trim      = trim,
            scale     = scale,
            position  = pos
          )
        } else {
          v_fill_aes <- if (is_baranger) ggplot2::aes(fill = .data[["x"]]) else NULL
          p = p + ggplot2::geom_violin(
            mapping   = v_fill_aes,
            alpha     = violin_alpha,
            width     = violin_width,
            fill      = if (!is_baranger) "grey80" else NULL,
            color     = violin_color %||% NA,
            linewidth = violin_linewidth,
            trim      = trim,
            scale     = scale
          )
        }
      }

      # 2. Boxplot layer
      if (!is.null(settings$by)) {
        if (is_pathology) {
          # Pathology style: outlines and whiskers match group/status color
          p = p + ggplot2::geom_boxplot(
            ggplot2::aes(fill = .data[["..by.."]], color = .data[["..by.."]]),
            width         = width,
            alpha         = alpha,
            linewidth     = linewidth,
            notch         = isTRUE(notch),
            notchwidth    = notchwidth,
            outlier.shape = outlier_shape,
            outlier.size  = outlier_size,
            position      = pos
          )
        } else {
          b_color <- box_color %||% (if (is_baranger) "black" else "#30343B")
          p = p + ggplot2::geom_boxplot(
            ggplot2::aes(fill = .data[["..by.."]]),
            width         = width,
            alpha         = alpha,
            linewidth     = linewidth,
            color         = b_color,
            notch         = isTRUE(notch),
            notchwidth    = notchwidth,
            outlier.shape = outlier_shape,
            outlier.size  = outlier_size,
            position      = pos
          )
        }
      } else {
        col_fill <- if (is_pathology) (settings$col.default %||% "#0F4D92") else (settings$col.default %||% "#d62728")
        col_line <- box_color %||% (if (is_pathology) col_fill else (if (is_baranger) "black" else "#30343B"))
        b_fill_aes <- if (is_baranger) ggplot2::aes(fill = .data[["x"]]) else NULL
        p = p + ggplot2::geom_boxplot(
          mapping       = b_fill_aes,
          width         = width,
          alpha         = alpha,
          linewidth     = linewidth,
          fill          = if (!is_baranger) col_fill else NULL,
          color         = col_line,
          notch         = isTRUE(notch),
          notchwidth    = notchwidth,
          outlier.shape = outlier_shape,
          outlier.size  = outlier_size
        )
      }

      # 3. Add jittered biological replicates / points
      if (isTRUE(add_jitter)) {
        jitter_data <- data
        if (!is.null(jitter_subsample) && is.numeric(jitter_subsample) && jitter_subsample > 0) {
          by_col <- if (!is.null(settings$by)) jitter_data[["..by.."]] else rep(1, nrow(jitter_data))
          grp_keys <- interaction(jitter_data[["x"]], by_col, drop = TRUE)
          idx_keep <- unlist(lapply(split(seq_len(nrow(jitter_data)), grp_keys), function(idx) {
            if (length(idx) > jitter_subsample) {
              set.seed(42)
              sample(idx, jitter_subsample)
            } else {
              idx
            }
          }), use.names = FALSE)
          jitter_data <- jitter_data[sort(idx_keep), , drop = FALSE]
        }

        if (!is.null(settings$by)) {
          jit_pos <- if (is_grouped_dodge) {
            ggplot2::position_jitterdodge(
              jitter.width  = jitter_width,
              dodge.width   = dodge_width,
              jitter.height = 0
            )
          } else {
            ggplot2::position_jitter(
              width  = jitter_width,
              height = 0
            )
          }

          if (point_shape %in% c(21:25)) {
            p = p + ggplot2::geom_point(
              data     = jitter_data,
              mapping  = ggplot2::aes(fill = .data[["..by.."]]),
              shape    = point_shape,
              color    = point_border %||% "black",
              stroke   = point_stroke,
              alpha    = jitter_alpha,
              size     = jitter_size,
              position = jit_pos
            )
          } else {
            p = p + ggplot2::geom_point(
              data     = jitter_data,
              mapping  = ggplot2::aes(color = .data[["..by.."]]),
              shape    = point_shape,
              alpha    = jitter_alpha,
              size     = jitter_size,
              position = jit_pos
            )
          }
        } else {
          # Single-factor
          if (point_shape %in% c(21:25)) {
            p = p + ggplot2::geom_jitter(
              data     = jitter_data,
              mapping  = ggplot2::aes(fill = .data[["x"]]),
              shape    = point_shape,
              color    = point_border %||% "black",
              stroke   = point_stroke,
              width    = jitter_width,
              height   = 0,
              alpha    = jitter_alpha,
              size     = jitter_size
            )
          } else {
            p = p + ggplot2::geom_jitter(
              data     = jitter_data,
              width    = jitter_width,
              alpha    = jitter_alpha,
              size     = jitter_size,
              height   = 0,
              shape    = point_shape,
              color    = if (is_pathology) (settings$col.default %||% "#0F4D92") else "grey30"
            )
          }
        }
      }

      # Paired mean connecting lines (connecting means across consecutive by-levels)
      if (isTRUE(show_mean_line) && is_grouped_dodge) {
        x_fac   <- factor(data[["x"]])
        x_lvls  <- levels(x_fac)
        by_fac  <- factor(data[["..by.."]])
        by_lvls <- levels(by_fac)
        K       <- length(by_lvls)

        if (K >= 2) {
          seg_rows <- list()
          for (i in seq_along(x_lvls)) {
            x_val <- x_lvls[i]
            for (k in 1:(K - 1)) {
              y_sub1 <- data$y[x_fac == x_val & by_fac == by_lvls[k]]
              y_sub2 <- data$y[x_fac == x_val & by_fac == by_lvls[k + 1]]
              m1 <- mean(y_sub1, na.rm = TRUE)
              m2 <- mean(y_sub2, na.rm = TRUE)
              if (!is.na(m1) && !is.na(m2)) {
                x_offset1 <- dodge_width * ((k - 0.5) / K - 0.5)
                x_offset2 <- dodge_width * ((k + 1 - 0.5) / K - 0.5)
                seg_rows[[length(seg_rows) + 1]] <- data.frame(
                  x    = i + x_offset1,
                  xend = i + x_offset2,
                  y    = m1,
                  yend = m2
                )
              }
            }
          }
          if (length(seg_rows) > 0) {
            seg_df <- do.call(rbind, seg_rows)
            p = p + ggplot2::geom_segment(
              data        = seg_df,
              mapping     = ggplot2::aes(x = x, xend = xend, y = y, yend = yend),
              linetype    = mean_line_lty,
              color       = mean_line_col,
              linewidth   = mean_line_lwd,
              inherit.aes = FALSE
            )
          }
        }
      }

      # Bold black median bar across each box
      if (isTRUE(bold_median)) {
        if (!is.null(settings$by)) {
          p = p + ggplot2::stat_summary(
            geom      = "errorbar",
            mapping   = ggplot2::aes(ymax = ggplot2::after_stat(y), ymin = ggplot2::after_stat(y), group = .data[["..by.."]]),
            fun       = stats::median,
            width     = width * 0.95,
            color     = bold_median_col %||% "#111111",
            linewidth = bold_median_lwd %||% 0.85,
            position  = pos
          )
        } else {
          p = p + ggplot2::stat_summary(
            geom      = "errorbar",
            mapping   = ggplot2::aes(ymax = ggplot2::after_stat(y), ymin = ggplot2::after_stat(y)),
            fun       = stats::median,
            width     = width * 0.95,
            color     = bold_median_col %||% "#111111",
            linewidth = bold_median_lwd %||% 0.85
          )
        }
      }

      # Diamond mean markers
      if (isTRUE(show_mean)) {
        if (!is.null(settings$by)) {
          p = p + ggplot2::stat_summary(
            fun       = mean,
            geom      = "point",
            mapping   = ggplot2::aes(group = .data[["..by.."]]),
            shape     = mean_shape,
            fill      = mean_fill,
            color     = mean_color,
            size      = mean_size,
            stroke    = mean_stroke %||% 0.45,
            position  = pos
          )
        } else {
          p = p + ggplot2::stat_summary(
            fun       = mean,
            geom      = "point",
            shape     = mean_shape,
            fill      = mean_fill,
            color     = mean_color,
            size      = mean_size,
            stroke    = mean_stroke %||% 0.45
          )
        }
      }

      # Remove redundant colour legend when jitter mirrors fill in default/baranger style
      if (add_jitter && !is.null(settings$by) && !is_pathology) {
        p = p + ggplot2::guides(color = "none")
      }

      p
    },
    name = if (is_pathology) "pathology_boxplot" else if (is_baranger) "scatter_box_violin" else "boxplot"
  )
}

#' Pathology Boxplot Type (Nature Aging / SciFig Style)
#'
#' @description Creates publication-grade pathology boxplots conforming to
#'   Nature Aging and SciFig standards (as seen in Yadav et al. 2026).
#'   Features semi-transparent boxes with status-matched borders, bold black
#'   horizontal medians, diamond mean markers, dotted trajectory lines
#'   connecting paired means across conditions, dashed zero reference baseline,
#'   and optional subsampling for large-cohort biological replicates.
#'
#' @inheritParams type_boxplot
#'
#' @return A cliplot_type object for use with \code{\link{cliplot}}.
#'
#' @examples
#' \dontrun{
#' cliplot(delta ~ pathology | status, data = df_gtex,
#'         type = type_pathology_boxplot(),
#'         palette = "pathology", theme = "pathology")
#' }
#'
#' @export
type_pathology_boxplot = function(
    add_jitter       = TRUE,
    jitter_width     = 0.10,
    jitter_alpha     = 0.35,
    jitter_size      = 0.85,
    jitter_subsample = NULL,
    add_violin       = FALSE,
    violin_alpha     = 0.2,
    outlier_shape    = NA,
    outlier_size     = 1.5,
    width            = 0.32,
    alpha            = 0.22,
    linewidth        = 0.45,
    position_dodge   = 0.42,
    show_mean        = TRUE,
    show_mean_line   = TRUE,
    mean_shape       = 23,
    mean_fill        = "#FFFFFF",
    mean_color       = "black",
    mean_size        = 2.0,
    mean_stroke      = 0.45,
    mean_line_lty    = "dotted",
    mean_line_col    = "grey45",
    mean_line_lwd    = 0.38,
    bold_median      = TRUE,
    bold_median_col  = "#111111",
    bold_median_lwd  = 0.85,
    zero_line        = TRUE,
    zero_line_y      = 0,
    zero_line_col    = "grey75",
    zero_line_lty    = "dashed"
) {
  type_boxplot(
    style            = "pathology",
    add_jitter       = add_jitter,
    jitter_width     = jitter_width,
    jitter_alpha     = jitter_alpha,
    jitter_size      = jitter_size,
    jitter_subsample = jitter_subsample,
    add_violin       = add_violin,
    violin_alpha     = violin_alpha,
    outlier_shape    = outlier_shape,
    outlier_size     = outlier_size,
    width            = width,
    alpha            = alpha,
    linewidth        = linewidth,
    position_dodge   = position_dodge,
    show_mean        = show_mean,
    show_mean_line   = show_mean_line,
    mean_shape       = mean_shape,
    mean_fill        = mean_fill,
    mean_color       = mean_color,
    mean_size        = mean_size,
    mean_stroke      = mean_stroke,
    mean_line_lty    = mean_line_lty,
    mean_line_col    = mean_line_col,
    mean_line_lwd    = mean_line_lwd,
    bold_median      = bold_median,
    bold_median_col  = bold_median_col,
    bold_median_lwd  = bold_median_lwd,
    zero_line        = zero_line,
    zero_line_y      = zero_line_y,
    zero_line_col    = zero_line_col,
    zero_line_lty    = zero_line_lty
  )
}

#' Scatter-Box-Violin Plot Type (David Baranger Style)
#'
#' @description Creates publication-ready distribution plots layering three
#'   complementary representations of data as advocated by David Baranger (2018):
#'   1. A background semi-transparent violin plot showing the probability density
#'      and shape of the distribution.
#'   2. A narrow foreground notched boxplot displaying summary statistics
#'      (median, 95% CI notch, interquartile range).
#'   3. An overlay of individual jittered raw data points (with black outline and
#'      group fill) ensuring complete data transparency.
#'   Supports both single-factor and dodged multi-factor groups.
#'
#' @inheritParams type_boxplot
#'
#' @return A cliplot_type object for use with \code{\link{cliplot}}.
#'
#' @references
#' Baranger, D. (2018). Showing your data: Scatter, box, & violin plots.
#' \url{https://davidbaranger.com/2018/03/05/showing-your-data-scatter-box-violin-plots/}
#'
#' @examples
#' \dontrun{
#' # Classic Baranger scatter-box-violin plot on single factor
#' cliplot(Sepal.Length ~ Species, data = iris,
#'         type = type_scatter_box_violin())
#'
#' # Grouped / dodged Baranger plot with 2-way layout
#' cliplot(len ~ dose | supp, data = ToothGrowth,
#'         type = type_scatter_box_violin(notch = FALSE),
#'         palette = "nature")
#' }
#'
#' @export
type_scatter_box_violin = function(
    notch            = TRUE,
    notchwidth       = 0.5,
    point_shape      = 21,
    point_border     = "black",
    point_stroke     = 0.35,
    add_violin       = TRUE,
    violin_alpha     = 0.30,
    violin_width     = 0.80,
    violin_color     = "black",
    violin_linewidth = 0.55,
    trim             = TRUE,
    scale            = "width",
    add_jitter       = TRUE,
    jitter_width     = 0.12,
    jitter_size      = 1.8,
    jitter_alpha     = 0.65,
    jitter_subsample = NULL,
    width            = 0.22,
    alpha            = 0.75,
    linewidth        = 0.65,
    box_color        = "black",
    position_dodge   = 0.80,
    outlier_shape    = NA,
    outlier_size     = 1.5,
    ...
) {
  type_boxplot(
    style            = "baranger",
    add_jitter       = add_jitter,
    jitter_width     = jitter_width,
    jitter_alpha     = jitter_alpha,
    jitter_size      = jitter_size,
    jitter_subsample = jitter_subsample,
    point_shape      = point_shape,
    point_border     = point_border,
    point_stroke     = point_stroke,
    add_violin       = add_violin,
    violin_alpha     = violin_alpha,
    violin_width     = violin_width,
    violin_color     = violin_color,
    violin_linewidth = violin_linewidth,
    trim             = trim,
    scale            = scale,
    notch            = notch,
    notchwidth       = notchwidth,
    outlier_shape    = outlier_shape,
    outlier_size     = outlier_size,
    width            = width,
    alpha            = alpha,
    linewidth        = linewidth,
    box_color        = box_color,
    position_dodge   = position_dodge,
    ...
  )
}

#' @rdname type_scatter_box_violin
#' @export
type_baranger = type_scatter_box_violin

#' @rdname type_scatter_box_violin
#' @export
type_sbv = type_scatter_box_violin

#' @rdname type_scatter_box_violin
#' @export
type_scatter_box = function(
    notch            = TRUE,
    point_shape      = 21,
    point_border     = "black",
    point_stroke     = 0.35,
    add_jitter       = TRUE,
    jitter_width     = 0.15,
    jitter_size      = 1.8,
    jitter_alpha     = 0.65,
    width            = 0.45,
    alpha            = 0.75,
    linewidth        = 0.65,
    box_color        = "black",
    position_dodge   = 0.80,
    ...
) {
  type_boxplot(
    style            = "scatter_box",
    add_violin       = FALSE,
    add_jitter       = add_jitter,
    jitter_width     = jitter_width,
    jitter_alpha     = jitter_alpha,
    jitter_size      = jitter_size,
    point_shape      = point_shape,
    point_border     = point_border,
    point_stroke     = point_stroke,
    notch            = notch,
    width            = width,
    alpha            = alpha,
    linewidth        = linewidth,
    box_color        = box_color,
    position_dodge   = position_dodge,
    ...
  )
}

#' @rdname type_scatter_box_violin
#' @export
type_box_violin = function(
    notch            = TRUE,
    add_violin       = TRUE,
    violin_alpha     = 0.30,
    violin_width     = 0.80,
    violin_color     = "black",
    violin_linewidth = 0.55,
    trim             = TRUE,
    scale            = "width",
    width            = 0.22,
    alpha            = 0.75,
    linewidth        = 0.65,
    box_color        = "black",
    position_dodge   = 0.80,
    ...
) {
  type_boxplot(
    style            = "box_violin",
    add_violin       = add_violin,
    violin_alpha     = violin_alpha,
    violin_width     = violin_width,
    violin_color     = violin_color,
    violin_linewidth = violin_linewidth,
    trim             = trim,
    scale            = scale,
    add_jitter       = FALSE,
    notch            = notch,
    width            = width,
    alpha            = alpha,
    linewidth        = linewidth,
    box_color        = box_color,
    position_dodge   = position_dodge,
    ...
  )
}
