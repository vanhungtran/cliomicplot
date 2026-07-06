# ===========================================================================
# cliomicplot: Text/Label Plot Type
# For adding text annotations or labeled points
# ===========================================================================

#' Text/Label Plot Type
#'
#' @description Adds text labels at (x, y) positions. Supports both plain text
#'   and label-style (with background rectangle). Useful for volcano plots,
#'   scatter plot annotations, and point labeling.
#'
#' @param labels Character vector of text labels, or column name in data
#' @param size Text size (default 3.5)
#' @param alpha Text transparency
#' @param style "text" for plain text or "label" for text with background box
#' @param repel Use ggrepel to avoid overlapping (default TRUE)
#' @param label.padding Padding around label text when style="label"
#' @param label.size Border size for label boxes
#'
#' @return A cliplot_type object for use with \code{\link{cliplot}}.
#'
#' @examples
#' \dontrun{
#' # Label top points in a scatter
#' top_cars <- head(mtcars[order(-mtcars$mpg), ], 5)
#' cliplot(mpg ~ wt, data = mtcars, type = "points")
#' cliplot(mpg ~ wt, data = top_cars, type = "text", add = TRUE)
#'
#' # Label with repel
#' cliplot(mpg ~ wt, data = mtcars,
#'         type = type_text(style = "label", repel = TRUE))
#' }
#'
#' @export
type_text = function(labels = NULL, size = 3.5, alpha = 0.9,
                      style = c("text", "label"), repel = TRUE,
                      label.padding = 0.25, label.size = 0.25) {
  style = match.arg(style)

  cliplot_type(
    data = function(settings, ...) {
      # Build from settings x/y, not from raw data
      df = data.frame(
        x = settings$x,
        y = settings$y,
        stringsAsFactors = FALSE
      )

      # Resolve labels
      if (is.null(labels)) {
        if (!is.null(settings$by)) {
          df$label = as.character(settings$by)
        } else {
          df$label = rownames(settings$data) %||% as.character(seq_len(nrow(df)))
        }
      } else if (is.character(labels) && length(labels) == 1) {
        # Try to get from original data
        orig = settings$data
        if (!is.null(orig) && labels %in% names(orig)) {
          df$label = orig[[labels]]
        } else {
          df$label = as.character(labels)
        }
      } else {
        df$label = as.character(labels)
      }
      settings$text_data = df
    },
    draw = function(data, mapping, settings, ...) {
      df = settings$text_data
      if (is.null(df)) df = data
      if (!"label" %in% names(df)) {
        df$label = rownames(df) %||% as.character(seq_len(nrow(df)))
      }

      mapping$label = quote(.data[["label"]])

      p = ggplot2::ggplot(df, mapping)

      if (style == "label") {
        if (repel && requireNamespace("ggrepel", quietly = TRUE)) {
          p = p + ggrepel::geom_label_repel(
            data = df,
            size = size,
            alpha = alpha,
            label.padding = label.padding,
            label.size = label.size,
            show.legend = FALSE
          )
        } else {
          p = p + ggplot2::geom_label(
            data = df,
            size = size,
            alpha = alpha,
            label.padding = label.padding,
            label.size = label.size,
            show.legend = FALSE
          )
        }
      } else {
        if (repel && requireNamespace("ggrepel", quietly = TRUE)) {
          p = p + ggrepel::geom_text_repel(
            data = df,
            size = size,
            alpha = alpha,
            show.legend = FALSE
          )
        } else {
          p = p + ggplot2::geom_text(
            data = df,
            size = size,
            alpha = alpha,
            show.legend = FALSE
          )
        }
      }
      p
    },
    name = "text"
  )
}
