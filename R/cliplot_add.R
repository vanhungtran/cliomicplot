# ===========================================================================
# cliomicplot: Add layers to existing plot (cliplot_add)
# Inspired by tinyplot's tinyplot_add.R
# ===========================================================================

#' Add Layers to an Existing Cliomicplot
#'
#' @description
#' Adds additional layers (points, lines, text, etc.) to the last cliomicplot
#' created. Similar to tinyplot's `tinyplot_add()`.
#'
#' @param type Plot type for the added layer.
#' @param ... Additional arguments passed to the type's draw function.
#'
#' @details
#' Currently a placeholder. Full layer addition requires tracking the last
#' ggplot object, which will be implemented in a future version.
#'
#' @export
cliplot_add = function(type = "points", ...) {
  last_call = get_environment_variable(".last_call")

  if (is.null(last_call)) {
    stop("No previous cliplot found. Create a plot with cliplot() first.")
  }

  message("cliplot_add() is a work in progress. Full layer addition coming soon.")
  invisible(NULL)
}
