# ===========================================================================
# cliomicplot: Single-Cell & Microbiome Lineage Composition Stacked Bars
# with Integrated Clinical Phenotypic Metadata Annotation Track
# Clinical & Omics Figure Archetype 37
# Inspired by blood_12b_figures.R & scRNA-seq PBMC Atlas Architectures
# ===========================================================================

#' Cellular & Taxonomic Lineage Composition Stacked Bars with Clinical Track
#'
#' @description
#' Creates publication-ready cellular or taxonomic lineage composition stacked
#' bar charts (Clinical Archetype 37) with an integrated bottom clinical
#' phenotype track (e.g. disease severity, treatment cohort, or responder status).
#'
#' Addresses the classic ggplot2 single-fill limitation by mapping lineage
#' fractions to stacked bar fill while rendering the clinical metadata track
#' via discrete colored segments, allowing both legends to coexist harmoniously.
#'
#' @param sample_col Column name for sample or library identifiers.
#' @param lineage_col Column name for cellular lineages or taxa.
#' @param share_col Column name for fractional proportions (0 to 1) or abundance.
#' @param track_col Column name for clinical group, disease stage, or treatment arm.
#' @param facet_col Column name for dataset or cohort faceting.
#' @param track_y Vertical position of the clinical phenotype strip below baseline (default \code{-0.05}).
#' @param track_linewidth Thickness of the clinical track segment (default 2.4).
#' @param palette Discrete color palette name or vector of colors for cell lineages (default \code{"nature"}).
#' @param track_palette Discrete color vector or palette name for clinical groups.
#' @param lineage_name Legend title for cell lineages (default \code{"Cell Lineage"}).
#' @param track_name Legend title for clinical phenotype track (default \code{"Clinical Group"}).
#' @param show_percentages Logical. If \code{TRUE} (default), formats y-axis as percentages (0\% to 100\%).
#' @param reorder_by_track Logical. If \code{TRUE} (default), orders samples along x-axis by clinical group then sample ID.
#' @param xlab,ylab Axis labels.
#'
#' @return A \code{cliplot_type} object for use with \code{\link{cliplot}}.
#'
#' @examples
#' \dontrun{
#' library(cliomicplot)
#' data(immune_cell_composition)
#'
#' # Lineage composition with clinical phenotype track
#' cliplot(
#'   share ~ gsm,
#'   data = immune_cell_composition,
#'   type = type_composition(
#'     lineage_col = "lineage",
#'     track_col = "group",
#'     facet_col = "dataset"
#'   ),
#'   title = "Single-Cell Leukocyte Lineage Composition Across Allergic Cohorts"
#' )
#' }
#'
#' @export
type_composition = function(
    sample_col       = NULL,
    lineage_col      = NULL,
    share_col        = NULL,
    track_col        = NULL,
    facet_col        = NULL,
    track_y          = -0.05,
    track_linewidth  = 2.4,
    palette          = "nature",
    track_palette    = NULL,
    lineage_name     = "Cell Lineage",
    track_name       = "Clinical Group",
    show_percentages = TRUE,
    reorder_by_track = TRUE,
    xlab             = NULL,
    ylab             = NULL
) {
  cliplot_type(
    data = function(settings, ...) {
      df <- settings$data
      if (is.null(df)) {
        df <- data.frame(
          x = as.character(settings$x),
          y = as.numeric(settings$y),
          stringsAsFactors = FALSE
        )
      } else {
        df <- as.data.frame(df)
      }

      nms <- names(df)

      find_col <- function(candidates, default_val = NULL) {
        low_nms <- tolower(nms)
        for (cand in candidates) {
          idx <- which(low_nms == tolower(cand))
          if (length(idx) > 0) return(nms[idx[1]])
        }
        default_val
      }

      # Resolve sample (x)
      sc <- sample_col %||% if (is.character(settings$x) && length(settings$x) == 1 && settings$x %in% nms) {
        settings$x
      } else {
        find_col(c("gsm", "sample", "library", "id", "donor", "patient", "subject", "x"), nms[1])
      }

      # Resolve share/proportion (y)
      yc <- share_col %||% if (is.character(settings$y) && length(settings$y) == 1 && settings$y %in% nms) {
        settings$y
      } else {
        find_col(c("share", "fraction", "proportion", "percentage", "abundance", "y"), nms[min(2, ncol(df))])
      }

      # Resolve lineage
      lc <- lineage_col %||% find_col(c("lineage", "cell_type", "celltype", "cluster", "taxon", "phylum", "species", "feature"))

      # Resolve track (phenotype / clinical group)
      tc <- track_col %||% find_col(c("group", "disease", "treatment", "condition", "phenotype", "status", "stage"))

      # Resolve facet (dataset / cohort)
      fc <- facet_col %||% if (!is.null(settings$facet)) {
        all.vars(settings$facet)[1]
      } else {
        find_col(c("dataset", "cohort", "study", "batch", "series"))
      }

      df$.sample <- as.character(df[[sc]])
      df$.share <- as.numeric(df[[yc]])
      df$.lineage <- if (!is.null(lc) && lc %in% nms) as.character(df[[lc]]) else "All"
      df$.track <- if (!is.null(tc) && tc %in% nms) as.character(df[[tc]]) else "Cohort"
      df$.facet <- if (!is.null(fc) && fc %in% nms) as.character(df[[fc]]) else NULL

      # Order samples by facet, track, sample
      if (isTRUE(reorder_by_track)) {
        if (!is.null(df$.facet)) {
          df <- df[order(df$.facet, df$.track, df$.sample), ]
          df$.x_pos <- ave(seq_len(nrow(df)), df$.facet, df$.sample, FUN = function(x) 1)
          # build distinct ranks per facet
          for (fac in unique(df$.facet)) {
            idx <- which(df$.facet == fac)
            u_samp <- unique(df$.sample[idx])
            samp_map <- setNames(seq_along(u_samp), u_samp)
            df$.x_pos[idx] <- samp_map[df$.sample[idx]]
          }
        } else {
          df <- df[order(df$.track, df$.sample), ]
          u_samp <- unique(df$.sample)
          samp_map <- setNames(seq_along(u_samp), u_samp)
          df$.x_pos <- samp_map[df$.sample]
        }
      } else {
        u_samp <- unique(df$.sample)
        samp_map <- setNames(seq_along(u_samp), u_samp)
        df$.x_pos <- samp_map[df$.sample]
      }

      # Distinct group track df
      track_vars <- c(".sample", ".x_pos", ".track")
      if (!is.null(df$.facet)) track_vars <- c(track_vars, ".facet")
      track_df <- unique(df[, track_vars, drop = FALSE])

      settings$comp_df <- df
      settings$comp_track_df <- track_df
      settings$skip_postprocess <- TRUE
    },

    draw = function(data, mapping, settings, ...) {
      df <- settings$comp_df
      track_df <- settings$comp_track_df
      if (is.null(df)) return(ggplot2::ggplot())

      # Lineage colors
      u_lineages <- unique(df$.lineage)
      pal_presets <- list(
        nature = c("#3B4992", "#EE0000", "#008B45", "#631879", "#56B4E9", "#E69F00", "#808180", "#CC79A7"),
        jco    = c("#0073C2", "#EFC000", "#868686", "#CD534C", "#7AA6DC", "#003C67", "#8F7700", "#3B3B3B"),
        nejm   = c("#BC3C29", "#0072B5", "#E18727", "#20854E", "#7876B1", "#6F99AD", "#FFDC91", "#EE4C97")
      )
      lin_cols <- if (is.character(palette) && length(palette) == 1 && palette %in% names(pal_presets)) {
        rep(pal_presets[[palette]], length.out = length(u_lineages))
      } else if (length(palette) >= length(u_lineages)) {
        palette
      } else {
        scales::hue_pal()(length(u_lineages))
      }
      names(lin_cols) <- u_lineages

      # Track colors
      u_tracks <- sort(unique(track_df$.track))
      trk_cols <- if (!is.null(track_palette)) {
        if (length(track_palette) >= length(u_tracks)) {
          track_palette[seq_along(u_tracks)]
        } else {
          rep(track_palette, length.out = length(u_tracks))
        }
      } else {
        pal24 <- c("#42949E", "#D98C21", "#6A51A3", "#E64B35", "#7E6148", "#00A087",
                   "#3C5488", "#F39B7F", "#8491B4", "#91D1C2", "#DC0000", "#4A6999")
        rep(pal24, length.out = length(u_tracks))
      }
      names(trk_cols) <- u_tracks

      p <- ggplot2::ggplot() +
        ggplot2::geom_col(
          data = df,
          ggplot2::aes(x = .data[[".x_pos"]], y = .data[[".share"]], fill = .data[[".lineage"]]),
          width = 1.0,
          colour = NA
        ) +
        ggplot2::geom_segment(
          data = track_df,
          ggplot2::aes(
            x = .data[[".x_pos"]] - 0.5,
            xend = .data[[".x_pos"]] + 0.5,
            y = track_y,
            yend = track_y,
            colour = .data[[".track"]]
          ),
          linewidth = track_linewidth,
          lineend = "butt"
        )

      # Faceting
      if (!is.null(df$.facet)) {
        p <- p + ggplot2::facet_grid(. ~ .facet, scales = "free_x", space = "free_x")
      }

      # Scales
      p <- p +
        ggplot2::scale_fill_manual(name = lineage_name, values = lin_cols) +
        ggplot2::scale_colour_manual(name = track_name, values = trk_cols) +
        ggplot2::guides(
          fill = ggplot2::guide_legend(nrow = 1, order = 1),
          colour = ggplot2::guide_legend(ncol = 3, order = 2, override.aes = list(linewidth = 3))
        ) +
        ggplot2::scale_x_continuous(expand = c(0, 0))

      if (isTRUE(show_percentages)) {
        p <- p + ggplot2::scale_y_continuous(
          breaks = c(0, 0.25, 0.5, 0.75, 1.0),
          labels = scales::percent_format(accuracy = 1),
          expand = ggplot2::expansion(mult = c(0.04, 0.02))
        )
      } else {
        p <- p + ggplot2::scale_y_continuous(
          breaks = c(0, 0.25, 0.5, 0.75, 1.0),
          expand = ggplot2::expansion(mult = c(0.04, 0.02))
        )
      }

      p <- p + ggplot2::labs(
        title    = settings$title,
        subtitle = settings$subtitle,
        caption  = settings$caption,
        x        = xlab %||% settings$xlab %||% "Samples (Ordered by Phenotype)",
        y        = ylab %||% settings$ylab %||% if (isTRUE(show_percentages)) "Proportion of Cells" else "Share"
      )

      p <- p + resolve_theme(settings$theme %||% "classic") +
        ggplot2::theme(
          axis.text.x = ggplot2::element_blank(),
          axis.ticks.x = ggplot2::element_blank(),
          strip.text.x = ggplot2::element_text(face = "bold", size = 7.5),
          strip.background = ggplot2::element_blank(),
          legend.position = "bottom",
          legend.box = "vertical",
          legend.key.size = grid::unit(3, "mm"),
          legend.text = ggplot2::element_text(size = 6.5),
          legend.title = ggplot2::element_text(size = 7, face = "bold"),
          panel.spacing = grid::unit(1.5, "mm"),
          plot.margin = ggplot2::margin(3, 4, 3, 4, "mm")
        )

      p
    },
    name = "composition"
  )
}
