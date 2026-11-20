# ===========================================================================
# cliomicplot: Correlation Matrix Type
# ===========================================================================

#' Correlation Matrix Plot Type
#'
#' @description Creates publication-ready correlation matrix plots with
#'   customizable color scales, significance indicators, clustering, and
#'   adaptive text and border scaling.
#'
#' @param method Correlation method: "pearson", "spearman", "kendall"
#' @param type Visualization type: "full", "upper", "lower"
#' @param add_coef Add correlation coefficients to cells (default TRUE)
#' @param coef_size Size of coefficient text (default 3.5)
#' @param color_low Low end of color gradient (default "#4575B4")
#' @param color_mid Midpoint color (default "white")
#' @param color_high High end of color gradient (default "#D73027")
#' @param cluster Cluster variables (default FALSE)
#' @param show_diag Show diagonal (default FALSE)
#' @param sig_level Significance level for stars (default 0.05)
#' @param axis_text_size Size of axis labels. If NULL (default), adaptively scaled
#'   based on number of variables (e.g. 9pt for <=8 vars down to 4.5pt for 50 vars).
#' @param axis_text_angle Angle for x-axis labels (default 45).
#' @param show_axis_text Logical indicating whether to display axis labels (default TRUE).
#' @param tile_border_color Color of tile borders (default "white"). Set to NA or NULL for no border.
#' @param tile_border_width Linewidth of tile borders. If NULL (default), adaptively scaled
#'   from 0.8 for <=15 variables down to 0.18 for 50 variables.
#' @param legend_title Legend title for colorbar (default "Correlation").
#' @param legend_barheight Height of colorbar (default 55, or a grid unit).
#' @param legend_barwidth Width of colorbar (default 8, or a grid unit).
#'
#' @return A cliplot_type object for use with \code{\link{cliplot}}.
#'
#' @examples
#' \dontrun{
#' # Correlation of numeric columns
#' cliplot(mtcars, type = "correlation")
#'
#' # Custom correlation
#' cliplot(mtcars, type = type_correlation(method = "spearman", type = "upper"))
#' }
#'
#' @export
type_correlation = function(
    method     = "pearson",
    type       = "full",
    add_coef   = TRUE,
    coef_size  = 3.5,
    color_low  = "#4575B4",
    color_mid  = "white",
    color_high = "#D73027",
    cluster    = FALSE,
    show_diag  = FALSE,
    sig_level  = 0.05,
    axis_text_size    = NULL,
    axis_text_angle   = 45,
    show_axis_text    = TRUE,
    tile_border_color = "white",
    tile_border_width = NULL,
    legend_title      = "Correlation",
    legend_barheight  = 55,
    legend_barwidth   = 8
) {
  cliplot_type(
    data = function(settings, ...) {
      df = settings$data
      if (is.null(df)) {
        stop("Correlation plot requires a data frame")
      }

      # Select numeric columns only
      num_cols = names(df)[vapply(df, is.numeric, logical(1))]
      if (length(num_cols) < 2) {
        stop("Need at least 2 numeric columns for correlation plot")
      }
      mat = as.matrix(df[, num_cols, drop = FALSE])
      n_vars = ncol(mat)

      # Adaptive axis text sizing
      if (is.null(axis_text_size)) {
        adaptive_axis_size = if (n_vars <= 8) 9.0
                             else if (n_vars <= 15) 7.5
                             else if (n_vars <= 25) 6.5
                             else if (n_vars <= 35) 5.5
                             else if (n_vars <= 50) 4.5
                             else 3.5
      } else {
        adaptive_axis_size = axis_text_size
      }

      settings$cor_axis_size  = adaptive_axis_size
      settings$cor_axis_angle = axis_text_angle
      settings$cor_show_axis  = show_axis_text

      # Compute correlation
      cor_mat = stats::cor(mat, use = "pairwise.complete.obs", method = method)

      # Compute p-values
      p_mat = matrix(NA, ncol(cor_mat), ncol(cor_mat))
      dimnames(p_mat) = dimnames(cor_mat)
      for (i in 1:ncol(mat)) {
        for (j in 1:ncol(mat)) {
          p_mat[i, j] = stats::cor.test(mat[, i], mat[, j], method = method)$p.value
        }
      }

      # Reshape
      if (type == "upper") {
        cor_mat[lower.tri(cor_mat, diag = !show_diag)] = NA
        p_mat[lower.tri(p_mat, diag = !show_diag)] = NA
      } else if (type == "lower") {
        cor_mat[upper.tri(cor_mat, diag = !show_diag)] = NA
        p_mat[upper.tri(p_mat, diag = !show_diag)] = NA
      }

      # Cluster
      if (cluster) {
        ord = stats::hclust(stats::as.dist(1 - cor_mat))$order
        cor_mat = cor_mat[ord, ord]
        p_mat   = p_mat[ord, ord]
      }

      # Melt
      melted_cor = reshape2::melt(cor_mat, na.rm = TRUE)
      names(melted_cor) = c("Var1", "Var2", "Correlation")
      melted_p = reshape2::melt(p_mat, na.rm = TRUE)
      melted_cor$PValue = melted_p$value

      # Significance stars
      melted_cor$Stars = ""
      melted_cor$Stars[melted_cor$PValue < sig_level] = "*"
      melted_cor$Stars[melted_cor$PValue < 0.01]  = "**"
      melted_cor$Stars[melted_cor$PValue < 0.001] = "***"
      melted_cor$TextColor = ifelse(abs(melted_cor$Correlation) > 0.55,
                                    "white", "#20242A")

      settings$cor_data = melted_cor
    },
    draw = function(data, mapping, settings, ...) {
      df = settings$cor_data
      if (is.null(df)) return(ggplot2::ggplot())

      # Adaptive coefficient size based on number of variables
      n_vars = length(unique(df$Var1))
      adaptive_size = if (n_vars <= 5) coef_size
                      else if (n_vars <= 10) min(coef_size, 3.0)
                      else if (n_vars <= 15) min(coef_size, 2.5)
                      else min(coef_size, 2.2)

      # Adaptive tile border linewidth
      b_width = if (!is.null(tile_border_width)) {
        tile_border_width
      } else if (n_vars <= 15) {
        0.8
      } else if (n_vars <= 30) {
        0.4
      } else if (n_vars <= 50) {
        0.18
      } else {
        0.08
      }

      b_col = if (is.null(tile_border_color) || is.na(tile_border_color)) NA else tile_border_color

      b_height = if (inherits(legend_barheight, "unit")) legend_barheight else ggplot2::unit(legend_barheight, "pt")
      b_width_bar = if (inherits(legend_barwidth, "unit")) legend_barwidth else ggplot2::unit(legend_barwidth, "pt")

      p = ggplot2::ggplot(df, ggplot2::aes(
        x = .data[["Var1"]], y = .data[["Var2"]], fill = .data[["Correlation"]]
      )) +
        ggplot2::geom_tile(color = b_col, linewidth = b_width) +
        ggplot2::scale_fill_gradient2(
          low      = color_low,
          mid      = color_mid,
          high     = color_high,
          midpoint = 0,
          limits   = c(-1, 1),
          name     = legend_title,
          guide    = ggplot2::guide_colorbar(
            barheight = b_height,
            barwidth  = b_width_bar,
            title.position = "top",
            title.hjust = 0.5
          )
        ) +
        ggplot2::labs(x = "", y = "") +
        ggplot2::coord_fixed() +
        ggplot2::theme(
          panel.grid   = ggplot2::element_blank(),
          panel.border = ggplot2::element_blank()
        )

      # Add correlation coefficients
      if (add_coef) {
        # Format label: round coefficient with significance stars on new line
        df$Label = ifelse(
          df$Stars != "",
          sprintf("%.2f\n%s", df$Correlation, df$Stars),
          sprintf("%.2f", df$Correlation)
        )
        p = p + ggplot2::geom_text(
          ggplot2::aes(
            label = .data[["Label"]],
            color = .data[["TextColor"]]
          ),
          size = adaptive_size,
          fontface = "bold",
          lineheight = 0.85,
          show.legend = FALSE
        ) +
        ggplot2::scale_color_identity()
      }

      p
    },
    name = "correlation"
  )
}
