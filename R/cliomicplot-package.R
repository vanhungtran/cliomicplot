#' cliomicplot: Publication-Ready Visualizations for Clinical and Multi-Omics Data
#'
#' @description
#' cliomicplot provides high-quality, publication-ready visualizations for
#' clinical and multi-omics data analysis. Built on ggplot2, it offers
#' specialized plot types and journal-specific themes.
#'
#' @section Design Philosophy:
#' The API design draws on patterns established in the broader R graphics
#' ecosystem — including the formula interface popularized by base R's
#' \code{plot()} and the type-system approach used in several modern
#' graphics packages.  Colour palettes are curated from published
#' journal style guides and open-source bioinformatics tools; see
#' the package REFERENCES for full attributions.
#'
#' @section Main Functions:
#' \itemize{
#'   \item \code{\link{cliplot}} - Main plotting function
#'   \item \code{\link{cliplot_add}} - Add layers to existing plot
#'   \item \code{\link{clitheme}} - Set plot themes
#'   \item \code{\link{clipar}} - Set/query graphical parameters
#' }
#'
#' @section Plot Types:
#' \itemize{
#'   \item Clinical: \code{\link{type_volcano}}, \code{\link{type_forest}}, \code{\link{type_km}},
#'         \code{\link{type_waterfall}}, \code{\link{type_swimmer}}
#'   \item Omics: \code{\link{type_heatmap}}, \code{\link{type_pca}}, \code{\link{type_ma}},
#'         \code{\link{type_correlation}}, \code{\link{type_boxplot}}, \code{\link{type_violin}}
#'   \item Gallery: \code{\link{type_ridge}}, \code{\link{type_lm}}, \code{\link{type_loess}},
#'         \code{\link{type_spineplot}}, \code{\link{type_rug}}, \code{\link{type_abline}},
#'         \code{\link{type_qq}}
#'   \item R Graph Gallery: \code{\link{type_chord}}, \code{\link{type_treemap}},
#'         \code{\link{type_streamgraph}}, \code{\link{type_connected}},
#'         \code{\link{type_circular_bar}}, \code{\link{type_density2d}},
#'         \code{\link{type_parallel}}, \code{\link{type_dendrogram}}
#'   \item Premier: \code{\link{type_raincloud}}, \code{\link{type_dumbbell}},
#'         \code{\link{type_lollipop}}, \code{\link{type_beeswarm}}, \code{\link{type_radar}},
#'         \code{\link{type_alluvial}}, \code{\link{type_waffle}}
#'   \item Basic: \code{\link{type_points}}, \code{\link{type_jitter}}, \code{\link{type_barplot}},
#'         \code{\link{type_lines}}, \code{\link{type_histogram}}, \code{\link{type_density}},
#'         \code{\link{type_errorbar}}, \code{\link{type_ribbon}}, \code{\link{type_text}},
#'         \code{\link{type_bubble}}
#'   \item Interactive: \code{\link{cliplot_interactive}} for plotly-powered zoom/pan/hover
#' }
#'
#' @keywords internal
#' @import ggplot2
"_PACKAGE"

#' Base cliomicplot ggplot2 theme
#' @keywords internal
theme_cli_base = function(base_size = 11, base_family = "") {
  ggplot2::theme_bw(base_size = base_size, base_family = base_family) +
    ggplot2::theme(
      plot.background = ggplot2::element_rect(fill = "white", color = NA),
      panel.background = ggplot2::element_rect(fill = "white", color = NA),
      panel.grid.minor = ggplot2::element_blank(),
      panel.grid.major = ggplot2::element_line(linewidth = 0.25, color = "#E8EBEF"),
      panel.border = ggplot2::element_rect(color = "#30343B", fill = NA, linewidth = 0.45),
      axis.line = ggplot2::element_blank(),
      axis.ticks = ggplot2::element_line(color = "#30343B", linewidth = 0.35),
      axis.ticks.length = ggplot2::unit(2.5, "pt"),
      plot.title = ggplot2::element_text(
        face = "bold", hjust = 0, size = base_size + 3,
        color = "#20242A", margin = ggplot2::margin(b = 6)
      ),
      plot.subtitle = ggplot2::element_text(
        hjust = 0, color = "#59616C", size = base_size,
        margin = ggplot2::margin(b = 10)
      ),
      plot.caption = ggplot2::element_text(
        hjust = 1, color = "#6C737D", size = base_size - 2,
        margin = ggplot2::margin(t = 10)
      ),
      plot.margin = ggplot2::margin(10, 12, 10, 12),
      strip.background = ggplot2::element_rect(fill = "#F0F3F7", color = "#D7DCE2"),
      strip.text = ggplot2::element_text(face = "bold", color = "#20242A"),
      legend.position = "right",
      legend.title = ggplot2::element_text(face = "bold", color = "#20242A"),
      legend.text = ggplot2::element_text(color = "#30343B"),
      legend.key = ggplot2::element_rect(fill = "white", color = NA),
      legend.background = ggplot2::element_rect(fill = "white", color = NA),
      axis.title = ggplot2::element_text(size = base_size, color = "#20242A"),
      axis.text = ggplot2::element_text(size = base_size - 1, color = "#30343B")
    )
}
