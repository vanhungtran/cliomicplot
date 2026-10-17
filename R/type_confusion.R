# ===========================================================================
# cliomicplot: Confusion Matrix Type
# Inspired by cnsplots confusionplot() - classification performance
# ===========================================================================

#' Confusion Matrix Type
#'
#' @description Creates publication-ready confusion matrix heatmaps for
#'   classification results. Cell fill encodes the count, cell text shows the
#'   number of observations, and the caption reports accuracy (and, for 2x2
#'   tables, a Fisher exact test p-value). Inspired by cnsplots'
#'   \code{confusionplot()}.
#'
#' @param add_pvalue Add a Fisher exact test p-value to the caption for 2x2
#'   tables (default FALSE).
#' @param show_accuracy Show classification accuracy in the caption
#'   (default TRUE).
#' @param color_low Low end of the count color gradient (default "#F7F7F7").
#' @param color_high High end of the count color gradient (default "#B2182B").
#' @param text_color Color of the count labels (default "white").
#' @param label_size Size of the count labels (default 4.2).
#' @param show_totals Show row/column marginal totals (default FALSE).
#'
#' @return A \code{cliplot_type} object for use with \code{\link{cliplot}}.
#'
#' @details
#' Use the formula interface \code{cliplot(truth ~ predicted, data, type =
#' "confusion")} where \code{y} is the ground-truth variable and \code{x} the
#' predicted variable. A data frame with exactly two factor/character columns
#' is also accepted directly.
#'
#' @examples
#' \dontrun{
#' cliplot(truth ~ predicted, data = predictions, type = "confusion")
#' cliplot(truth ~ predicted, data = predictions,
#'         type = type_confusion(add_pvalue = TRUE))
#' }
#'
#' @export
type_confusion = function(
    add_pvalue   = FALSE,
    show_accuracy = TRUE,
    color_low    = "#F7F7F7",
    color_high   = "#B2182B",
    text_color   = "white",
    label_size   = 4.2,
    show_totals  = FALSE
) {
  cliplot_type(
    data = function(settings, ...) {
      df = settings$data
      truth = settings$y
      pred  = settings$x

      if (is.null(df) && (is.null(truth) || is.null(pred))) {
        stop("type_confusion needs `truth` and `predicted` variables.",
             call. = FALSE)
      }
      if (!is.null(df) && is.data.frame(df)) {
        # Identify the two categorical columns if formula vars are missing
        cat_cols = names(df)[vapply(df, function(col) {
          is.factor(col) || is.character(col) || is.logical(col)
        }, logical(1))]
        if (is.null(truth) && is.null(pred) && length(cat_cols) >= 2) {
          truth = df[[cat_cols[1]]]
          pred  = df[[cat_cols[2]]]
        }
      }
      if (is.null(truth) || is.null(pred)) {
        stop("Cannot determine truth/predicted variables for the confusion ",
             "matrix. Use `cliplot(truth ~ predicted, data, type = ",
             "\"confusion\")`.", call. = FALSE)
      }

      tab = table(Truth = truth, Predicted = pred)
      if (nrow(tab) == 0 || ncol(tab) == 0) {
        stop("Empty confusion table.", call. = FALSE)
      }

      settings$conf_tab = tab
      settings$conf_add_pvalue = add_pvalue
      settings$conf_show_accuracy = show_accuracy
      settings$conf_color_low  = color_low
      settings$conf_color_high = color_high
      settings$conf_text_color = text_color
      settings$conf_label_size = label_size
      settings$conf_show_totals = show_totals
    },
    draw = function(data, mapping, settings, ...) {
      tab = settings$conf_tab
      if (is.null(tab)) return(ggplot2::ggplot())

      tab_df = as.data.frame(tab)
      names(tab_df) = c("truth", "pred", "count")

      acc = sum(diag(tab)) / sum(tab)
      caption_parts = character(0)
      if (isTRUE(settings$conf_show_accuracy)) {
        caption_parts = c(caption_parts,
                          sprintf("Accuracy = %.1f%%", 100 * acc))
      }
      if (isTRUE(settings$conf_add_pvalue) &&
          nrow(tab) == 2 && ncol(tab) == 2) {
        ft = tryCatch(stats::fisher.test(tab, workspace = 2e7),
                      error = function(e) NULL)
        if (!is.null(ft)) {
          p = ft$p.value
          caption_parts = c(caption_parts,
                            sprintf("Fisher exact test p = %s",
                                    format.pval(p, digits = 3)))
        }
      }
      caption = if (length(caption_parts) > 0) {
        paste(caption_parts, collapse = "  |  ")
      } else NULL

      # Text color: white on dark cells, dark on light cells
      tab_df$txt_col = ifelse(
        tab_df$count > 0 &
          (as.numeric(tab_df$count) / max(as.numeric(tab_df$count))) > 0.5,
        settings$conf_text_color, "#333333"
      )

      p = ggplot2::ggplot(tab_df, ggplot2::aes(
        x = .data[["pred"]], y = .data[["truth"]]
      )) +
        ggplot2::geom_tile(
          ggplot2::aes(fill = .data[["count"]]),
          color = "white", linewidth = 0.7
        ) +
        ggplot2::geom_text(
          ggplot2::aes(label = .data[["count"]], color = .data[["txt_col"]]),
          size = settings$conf_label_size, fontface = "bold"
        ) +
        ggplot2::scale_fill_gradient(
          low = settings$conf_color_low,
          high = settings$conf_color_high,
          guide = "none"
        ) +
        ggplot2::scale_color_identity() +
        ggplot2::coord_equal() +
        ggplot2::labs(
          x = "Predicted", y = "Truth",
          caption = caption
        ) +
        ggplot2::theme(
          axis.text = ggplot2::element_text(size = 9, face = "bold")
        )

      p
    },
    name = "confusion"
  )
}
