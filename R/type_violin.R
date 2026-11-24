# ===========================================================================
# cliomicplot: Violin Plot Type
# ===========================================================================

#' Violin Plot Type
#'
#' @description Creates publication-ready violin plots with optional boxplot
#'   overlay and jittered points.
#'
#' @param add_boxplot Overlay boxplot (default TRUE)
#' @param add_jitter Add jittered points (default TRUE)
#' @param jitter_width Width of jitter (default 0.15)
#' @param jitter_alpha Transparency of jittered points (default 0.5)
#' @param jitter_size Size of jittered points (default 1.5)
#' @param violin_alpha Violin transparency (default 0.7)
#' @param box_width Width of boxplot overlay (default 0.2)
#' @param trim Trim violin tails to data range (default TRUE)
#'
#' @return A cliplot_type object for use with \code{\link{cliplot}}.
#'
#' @examples
#' \dontrun{
#' cliplot(len ~ dose, data = ToothGrowth, type = "violin")
#'
#' # Custom violin
#' cliplot(len ~ dose, data = ToothGrowth,
#'         type = type_violin(add_boxplot = FALSE, jitter_width = 0.1))
#' }
#'
#' @export
type_violin = function(
    add_boxplot   = TRUE,
    add_jitter    = TRUE,
    jitter_width  = 0.15,
    jitter_alpha  = 0.45,
    jitter_size   = 1.6,
    violin_alpha  = 0.78,
    box_width     = 0.2,
    trim          = TRUE
) {
  cliplot_type(
    draw = function(data, mapping, settings, ...) {
      # Ensure categorical x for violin plots so numeric levels dodge cleanly
      if ("x" %in% names(data) && is.numeric(data[["x"]])) {
        data[["x"]] <- as.factor(data[["x"]])
      }

      p = ggplot2::ggplot(data, mapping)

      # Build position dodge if grouping is present and distinct from x
      is_grouped_dodge <- !is.null(settings$by) && !identical(as.character(data[["x"]]), as.character(data[["..by.."]]))
      dodge_width <- 0.8
      pos <- if (is_grouped_dodge) ggplot2::position_dodge(width = dodge_width) else ggplot2::position_identity()

      # Violin
      if (!is.null(settings$by)) {
        p = p + ggplot2::geom_violin(
          ggplot2::aes(fill = .data[["..by.."]]),
          alpha     = violin_alpha,
          trim      = trim,
          color     = "#30343B",
          linewidth = 0.25,
          position  = pos
        )
      } else {
        p = p + ggplot2::geom_violin(
          alpha     = violin_alpha,
          trim      = trim,
          fill      = settings$col.default %||% "#0073C2",
          color     = "#30343B",
          linewidth = 0.25
        )
      }

      # Boxplot overlay
      if (add_boxplot) {
        if (!is.null(settings$by)) {
          p = p + ggplot2::geom_boxplot(
            ggplot2::aes(fill = .data[["..by.."]]),
            width         = box_width,
            alpha         = 0.42,
            color         = "#20242A",
            outlier.shape = NA,
            position      = pos,
            show.legend   = FALSE
          )
        } else {
          p = p + ggplot2::geom_boxplot(
            width         = box_width,
            alpha         = 0.42,
            color         = "#20242A",
            fill          = "white",
            outlier.shape = NA
          )
        }
      }

      # Jittered points
      if (add_jitter) {
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
          p = p + ggplot2::geom_jitter(
            ggplot2::aes(color = .data[["..by.."]]),
            alpha    = jitter_alpha,
            size     = jitter_size,
            position = jit_pos
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

      if (add_jitter && !is.null(settings$by)) {
        p = p + ggplot2::guides(color = "none")
      }

      p
    },
    name = "violin"
  )
}
