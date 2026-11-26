# ===========================================================================
# cliomicplot: Split Violin Plot Type
# ===========================================================================

#' Split Violin Plot Type
#'
#' @description Creates publication-ready split violin plots comparing two
#'   conditions (such as Healthy vs Diseased, Control vs Treatment, or
#'   Responder vs Non-Responder) side-by-side as left and right halves sharing
#'   a common categorical axis.
#'
#' @param inner Interior summary structure: \code{"quartiles"} (default, draws
#'   25\%, median, and 75\% quantile lines), \code{"boxplot"} (miniature boxplot
#'   on each half), \code{"mean_se"} (mean diamond glyph with standard error bar),
#'   or \code{"none"}.
#' @param quantiles Numeric vector of quantile probabilities (default \code{c(0.25, 0.5, 0.75)}).
#' @param quantile_color Color for quantile lines (default \code{"grey25"}).
#' @param quantile_linewidth Line width for quantile lines (default 0.45).
#' @param quantile_linetype Line type for quantile lines (default \code{"solid"}).
#' @param add_jitter Add jittered points on each split half (default \code{FALSE}).
#' @param jitter_width Horizontal jitter width (default 0.08).
#' @param jitter_alpha Point transparency (default 0.45).
#' @param jitter_size Point size (default 1.3).
#' @param violin_alpha Transparency of split violins (default 0.8).
#' @param violin_color Border color of violins (default \code{"#20242A"}).
#' @param linewidth Outline stroke width (default 0.35).
#' @param box_width Width of inner boxplot when \code{inner = "boxplot"} (default 0.15).
#' @param box_fill Fill style for inner boxplot: \code{"group"} (default, filled
#'   by group color) or \code{"white"}.
#' @param box_alpha Transparency of inner boxplot (default 0.9).
#' @param trim Trim violin tails to observed data range (default \code{TRUE}).
#' @param scale Violin scaling method: \code{"width"} (default, all categories
#'   have equal maximum width), \code{"area"}, or \code{"count"}.
#' @param orientation Plot orientation: \code{"vertical"} (default) or \code{"horizontal"}.
#'
#' @return A cliplot_type object for use with \code{\link{cliplot}}.
#'
#' @examples
#' \dontrun{
#' # Compare supplement effects across doses
#' cliplot(len ~ dose | supp, data = ToothGrowth,
#'         type = type_split_violin(),
#'         title = "Odontoblast Length by Dose & Supplement")
#'
#' # With inner boxplots highlighted by group
#' cliplot(len ~ dose | supp, data = ToothGrowth,
#'         type = type_split_violin(inner = "boxplot"))
#' }
#'
#' @export
type_split_violin = function(
    inner              = c("quartiles", "boxplot", "mean_se", "none"),
    quantiles          = c(0.25, 0.5, 0.75),
    quantile_color     = "grey25",
    quantile_linewidth = 0.45,
    quantile_linetype  = "solid",
    add_jitter         = FALSE,
    jitter_width       = 0.08,
    jitter_alpha       = 0.45,
    jitter_size        = 1.3,
    violin_alpha       = 0.8,
    violin_color       = "#20242A",
    linewidth          = 0.35,
    box_width          = 0.15,
    box_fill           = c("group", "white"),
    box_alpha          = 0.9,
    trim               = TRUE,
    scale              = "width",
    orientation        = c("vertical", "horizontal")
) {
  inner       = match.arg(inner)
  box_fill    = match.arg(box_fill)
  orientation = match.arg(orientation)

  cliplot_type(
    draw = function(data, mapping, settings, ...) {
      if ("x" %in% names(data) && is.numeric(data[["x"]])) {
        data[["x"]] <- as.factor(data[["x"]])
      }

      by_var <- settings$by
      if (is.null(by_var) || !"..by.." %in% names(data)) {
        stop("type_split_violin requires a 2-level grouping variable specified via '| group' or 'by = \"group\"'.",
             call. = FALSE)
      }

      group_levels <- unique(as.character(data[["..by.."]]))
      if (length(group_levels) != 2) {
        warning(sprintf(
          "type_split_violin is designed for exactly 2 groups; found %d. Proceeding with split parity.",
          length(group_levels)), call. = FALSE)
      }

      p <- ggplot2::ggplot(data, mapping)

      # 1. Base Split Violin Layer
      p <- p + geom_split_violin(
        ggplot2::aes(fill = .data[["..by.."]]),
        alpha              = violin_alpha,
        color              = violin_color,
        linewidth          = linewidth,
        trim               = trim,
        scale              = scale,
        quantiles          = if (inner == "quartiles") quantiles else NULL,
        quantile_color     = quantile_color,
        quantile_linewidth = quantile_linewidth,
        quantile_linetype  = quantile_linetype,
        side               = "split"
      )

      # 2. Inner Summary Structure
      if (inner == "boxplot") {
        if (identical(box_fill, "white")) {
          p <- p + ggplot2::geom_boxplot(
            mapping       = ggplot2::aes(group = interaction(.data[["x"]], .data[["..by.."]])),
            width         = box_width,
            fill          = "white",
            alpha         = box_alpha,
            color         = violin_color,
            linewidth     = linewidth + 0.1,
            outlier.shape = NA,
            position      = ggplot2::position_dodge(width = 0.35),
            show.legend   = FALSE
          )
        } else {
          p <- p + ggplot2::geom_boxplot(
            mapping       = ggplot2::aes(group = interaction(.data[["x"]], .data[["..by.."]]),
                                         fill = .data[["..by.."]]),
            width         = box_width,
            alpha         = box_alpha,
            color         = violin_color,
            linewidth     = linewidth + 0.1,
            outlier.shape = NA,
            position      = ggplot2::position_dodge(width = 0.35),
            show.legend   = FALSE
          )
        }
      } else if (inner == "mean_se") {
        p <- p + ggplot2::stat_summary(
          ggplot2::aes(group = .data[["..by.."]]),
          fun.data  = ggplot2::mean_se,
          geom      = "errorbar",
          width     = 0.12,
          linewidth = linewidth + 0.35,
          color     = violin_color,
          position  = ggplot2::position_dodge(width = 0.35)
        ) + ggplot2::stat_summary(
          ggplot2::aes(group = .data[["..by.."]], fill = .data[["..by.."]]),
          fun       = mean,
          geom      = "point",
          shape     = 23,
          size      = 2.8,
          color     = violin_color,
          stroke    = 0.9,
          position  = ggplot2::position_dodge(width = 0.35),
          show.legend = FALSE
        )
      }

      # 3. Jittered Raw Points
      if (add_jitter) {
        p <- p + ggplot2::geom_point(
          ggplot2::aes(color = .data[["..by.."]]),
          position    = ggplot2::position_jitterdodge(
            jitter.width = jitter_width,
            dodge.width  = 0.35
          ),
          size        = jitter_size,
          alpha       = jitter_alpha,
          show.legend = FALSE
        )
      }

      # 4. Orientation
      if (orientation == "horizontal") {
        p <- p + ggplot2::coord_flip()
      }

      p
    },
    name = "split_violin"
  )
}
