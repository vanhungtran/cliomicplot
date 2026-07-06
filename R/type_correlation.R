# ===========================================================================
# cliomicplot: Correlation Matrix Type
# ===========================================================================

#' Correlation Matrix Plot Type
#'
#' @description Creates publication-ready correlation matrix plots with
#'   customizable color scales, significance indicators, and clustering.
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
    sig_level  = 0.05
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

      p = ggplot2::ggplot(df, ggplot2::aes(
        x = .data[["Var1"]], y = .data[["Var2"]], fill = .data[["Correlation"]]
      )) +
        ggplot2::geom_tile(color = "white", linewidth = 0.8) +
        ggplot2::scale_fill_gradient2(
          low      = color_low,
          mid      = color_mid,
          high     = color_high,
          midpoint = 0,
          limits   = c(-1, 1),
          name     = "Correlation",
          guide    = ggplot2::guide_colorbar(
            barheight = ggplot2::unit(55, "pt"),
            barwidth = ggplot2::unit(8, "pt")
          )
        ) +
        ggplot2::labs(x = "", y = "") +
        ggplot2::coord_fixed() +
        ggplot2::theme(
          axis.text.x = ggplot2::element_text(angle = 45, hjust = 1),
          panel.grid  = ggplot2::element_blank(),
          panel.border = ggplot2::element_blank()
        )

      # Add correlation coefficients
      if (add_coef) {
        p = p + ggplot2::geom_text(
          ggplot2::aes(
            label = paste0(round(.data[["Correlation"]], 2), "\n", .data[["Stars"]]),
            color = .data[["TextColor"]]
          ),
          size = coef_size,
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
