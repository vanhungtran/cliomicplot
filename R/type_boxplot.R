# ===========================================================================
# cliomicplot: Boxplot Type
# With jittered points, statistical annotations, and publication styling
# ===========================================================================

#' Boxplot Type
#'
#' @description Creates publication-ready boxplots with optional jittered
#'   points, violin overlays, and statistical annotations. Supports grouped
#'   dodging and facet_grid/facet_wrap layouts for matrix-style figures.
#'
#' @param add_jitter Add jittered points (default TRUE)
#' @param jitter_width Width of jitter (default 0.15)
#' @param jitter_alpha Transparency of jittered points (default 0.25)
#' @param jitter_size Size of jittered points (default 1.2)
#' @param add_violin Overlay violin plot (default FALSE)
#' @param violin_alpha Violin transparency (default 0.2)
#' @param outlier_shape Shape for outlier points. Set to `NA` to hide
#'   outliers (publication style). Default `NA`.
#' @param outlier_size Size of outlier points (default 1.5)
#' @param width Box width (default 0.55)
#' @param alpha Box fill transparency. Lower = more transparent (default 0.15)
#' @param linewidth Box outline width (default 0.5)
#' @param position_dodge Dodge width for grouped boxes. `NULL` uses ggplot2
#'   default (0.9). Set to e.g. 0.7 for tighter grouping.
#'
#' @return A cliplot_type object for use with \code{\link{cliplot}}.
#'
#' @examples
#' \dontrun{
#' # Basic publication-style boxplot (transparent fill, no outliers)
#' cliplot(len ~ dose, data = ToothGrowth, type = "boxplot")
#'
#' # With violin overlay + stats + bold outline
#' cliplot(len ~ dose, data = ToothGrowth,
#'         type = type_boxplot(add_violin = TRUE, linewidth = 0.8),
#'         stat.test = "t.test")
#'
#' # Grouped boxplot with faceting (matrix layout)
#' cliplot(value ~ method, data = df,
#'         type = type_boxplot(position_dodge = 0.7),
#'         facet = panel ~ logfc + fdr)
#' }
#'
#' @export
type_boxplot = function(
    add_jitter      = TRUE,
    jitter_width    = 0.15,
    jitter_alpha    = 0.38,
    jitter_size     = 1.6,
    add_violin      = FALSE,
    violin_alpha    = 0.2,
    outlier_shape   = NA,
    outlier_size    = 1.5,
    width           = 0.55,
    alpha           = 0.28,
    linewidth       = 0.65,
    position_dodge  = NULL
) {
  cliplot_type(
    draw = function(data, mapping, settings, ...) {
      p = ggplot2::ggplot(data, mapping)

      # Build position dodge if grouping is present
      dodge_width <- position_dodge %||% 0.9
      pos <- ggplot2::position_dodge(width = dodge_width)

      # Add violin if requested
      if (add_violin) {
        if (!is.null(settings$by)) {
          p = p + ggplot2::geom_violin(
            ggplot2::aes(fill = .data[["..by.."]]),
            alpha     = violin_alpha,
            width     = width * 1.2,
            color     = NA,
            position  = pos
          )
        } else {
          p = p + ggplot2::geom_violin(
            alpha    = violin_alpha,
            width    = width * 1.2,
            fill     = "grey80",
            color    = NA
          )
        }
      }

      # Boxplot
      if (!is.null(settings$by)) {
        p = p + ggplot2::geom_boxplot(
          ggplot2::aes(fill = .data[["..by.."]]),
          width         = width,
          alpha         = alpha,
          linewidth     = linewidth,
          color         = "#30343B",
          outlier.shape = outlier_shape,
          outlier.size  = outlier_size,
          position      = pos
        )
      } else {
        p = p + ggplot2::geom_boxplot(
          width         = width,
          alpha         = alpha,
          linewidth     = linewidth,
          fill          = settings$col.default %||% "#d62728",
          color         = "#30343B",
          outlier.shape = outlier_shape,
          outlier.size  = outlier_size
        )
      }

      # Add jittered points (publication style)
      if (add_jitter) {
        if (!is.null(settings$by)) {
          p = p + ggplot2::geom_jitter(
            ggplot2::aes(color = .data[["..by.."]]),
            alpha    = jitter_alpha,
            size     = jitter_size,
            position = ggplot2::position_jitterdodge(
              jitter.width = jitter_width,
              dodge.width  = dodge_width,
              jitter.height = 0
            )
          )
        } else {
          p = p + ggplot2::geom_jitter(
            width  = jitter_width,
            alpha  = jitter_alpha,
            size   = jitter_size,
            height = 0,
            color  = "grey30"
          )
        }
      }

      # Remove redundant colour legend when jitter mirrors fill
      if (add_jitter && !is.null(settings$by)) {
        p = p + ggplot2::guides(color = "none")
      }

      p
    },
    name = "boxplot"
  )
}
