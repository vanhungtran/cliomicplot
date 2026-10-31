# ===========================================================================
# cliomicplot: Host-Microbiome Longitudinal Transmission & Dyad Sharing Heatmap
# Clinical & Genomic Figure Archetype 28
# ===========================================================================

#' Longitudinal Transmission and Dyad Sharing Heatmap
#'
#' @description
#' Creates publication-ready discrete sharing heatmaps (Clinical Archetype 28)
#' to display bacterial strain transmission across mother-infant dyads, clinical
# cohort sharing matrices, or longitudinal persistence across study visits
#' (e.g. baseline infancy T1 versus 12 months T3).
#'
#' @param status_col Column name specifying the binary or categorical sharing
#'   status (e.g. "Shared", "Unique", "Persistent").
#' @param shared_label Label indicating sharing or transmission (default \code{"Shared"}).
#' @param shared_color Color for shared or transmitted state (default \code{"#E64B35"}).
#' @param unshared_color Color for unshared state (default \code{"#F0F3F8"}).
#' @param na_color Color for missing or unmeasured cells (default \code{"white"}).
#' @param border_color Cell outline color (default \code{"#DCE2EC"}).
#' @param border_size Cell outline stroke width (default \code{0.25}).
#' @param legend_title Title for the legend (default \code{"Transmission"}).
#' @param xlab,ylab Axis labels.
#'
#' @return A \code{cliplot_type} object for use with \code{\link{cliplot}}.
#'
#' @examples
#' \dontrun{
#' # Simulated mother-infant dyad sharing data
#' set.seed(42)
#' dyad_df <- expand.grid(
#'   Species = c("S. epidermidis", "S. hominis", "C. acnes", "M. restricta"),
#'   Dyad = paste0("Dyad_", 1:12)
#' )
#' dyad_df$Status <- ifelse(runif(nrow(dyad_df)) > 0.45, "Shared", "Not shared")
#'
#' cliplot(Species ~ Dyad, data = dyad_df,
#'         type = type_sharing_heatmap(status_col = "Status"))
#' }
#'
#' @export
type_sharing_heatmap = function(
    status_col     = NULL,
    shared_label   = "Shared",
    shared_color   = "#E64B35",
    unshared_color = "#F0F3F8",
    na_color       = "white",
    border_color   = "#DCE2EC",
    border_size    = 0.25,
    legend_title   = "Dyad Status",
    xlab           = NULL,
    ylab           = NULL
) {
  cliplot_type(
    data = function(settings, ...) {
      df <- settings$data
      if (is.null(df)) {
        df <- data.frame(
          x = as.character(settings$x),
          y = as.character(settings$y),
          stringsAsFactors = FALSE
        )
      } else {
        nms <- names(df)
        x_col <- if (is.character(settings$x) && length(settings$x) == 1 && settings$x %in% nms) {
          settings$x
        } else {
          intersect(c("dyad", "Dyad", "subject", "Subject", "id", "ID", "pair", "Pair"), nms)[1] %||% nms[1]
        }

        y_col <- if (is.character(settings$y) && length(settings$y) == 1 && settings$y %in% nms) {
          settings$y
        } else {
          intersect(c("species", "Species", "strain", "Strain", "feature", "Feature"), nms)[1] %||% nms[2]
        }

        df$.x <- as.character(if (!is.null(settings$x) && !is.character(settings$x)) settings$x else df[[x_col]])
        df$.y <- as.character(if (!is.null(settings$y) && !is.character(settings$y)) settings$y else df[[y_col]])

        st_col <- status_col %||% intersect(c("status", "Status", "shared", "Shared", "sharing", "Sharing"), nms)[1]
        if (!is.null(st_col) && st_col %in% nms) {
          df$.status <- as.character(df[[st_col]])
        } else if (!is.null(settings$by)) {
          df$.status <- as.character(settings$by)
        } else {
          df$.status <- rep(shared_label, nrow(df))
        }
      }

      df$.x <- factor(df$.x, levels = unique(df$.x))
      df$.y <- factor(df$.y, levels = rev(unique(df$.y)))
      df$.status <- factor(df$.status)

      settings$sharing_df <- df
    },

    draw = function(data, mapping, settings, ...) {
      df <- settings$sharing_df
      if (is.null(df)) return(ggplot2::ggplot())

      # Dynamic color vector based on levels
      u_levels <- levels(df$.status)
      val_cols <- character(length(u_levels))
      names(val_cols) <- u_levels
      for (lvl in u_levels) {
        if (grepl("share|transmit|persist|yes|true|1", tolower(lvl))) {
          val_cols[lvl] <- shared_color
        } else {
          val_cols[lvl] <- unshared_color
        }
      }

      p <- ggplot2::ggplot(df, ggplot2::aes(x = .data[[".x"]], y = .data[[".y"]])) +
        ggplot2::geom_tile(
          ggplot2::aes(fill = .data[[".status"]]),
          color = border_color,
          linewidth = border_size
        ) +
        ggplot2::scale_fill_manual(
          values = val_cols,
          na.value = na_color,
          name = legend_title
        ) +
        ggplot2::labs(
          x = xlab %||% "",
          y = ylab %||% ""
        ) +
        ggplot2::theme(
          axis.text.x = ggplot2::element_text(angle = 45, hjust = 1, vjust = 1),
          axis.text.y = ggplot2::element_text(face = "italic"),
          panel.grid = ggplot2::element_blank()
        )

      p
    },
    name = "sharing_heatmap"
  )
}
