# ===========================================================================
# cliomicplot: Palette-to-Scale Bridge
#
# Instead of per-palette scale_*_jco() / scale_*_nejm() functions, we provide
# a single generic palette_scale() factory that creates a ggplot2 Scale from
# any named palette in the cliomicplot registry.  The result is a standard
# ggplot2 Scale object that can be added with + like any other scale.
# ===========================================================================

# Internal factory: creates a discrete ggplot2 Scale from a named palette
palette_discrete = function(aesthetic, palette, alpha, reverse, ...) {
  pal = get_cli_palette(palette, n = 100)
  if (isTRUE(reverse)) pal = rev(pal)
  if (alpha < 1) pal = scales::alpha(pal, alpha)

  if (aesthetic == "fill") {
    ggplot2::scale_fill_manual(values = pal, ...)
  } else {
    ggplot2::scale_color_manual(values = pal, ...)
  }
}

# Internal factory: creates a continuous ggplot2 Scale from a named palette
palette_continuous = function(aesthetic, palette, alpha, reverse, ...) {
  pal = get_cli_palette(palette, n = 100)
  if (isTRUE(reverse)) pal = rev(pal)
  if (alpha < 1) pal = scales::alpha(pal, alpha)

  if (aesthetic == "fill") {
    ggplot2::scale_fill_gradientn(colors = pal, ...)
  } else {
    ggplot2::scale_color_gradientn(colors = pal, ...)
  }
}

#' Build a ggplot2 Scale from a Cliomicplot Palette
#'
#' @description
#' A generic scale builder that resolves any named palette from the
#' cliomicplot registry into a ggplot2 Scale object.  Add it to any plot
#' with `+`.  This avoids the per-palette function explosion pattern and
#' keeps the API surface small.
#'
#' @param palette Character. Name of a palette in the registry (use
#'   `cli_palette_list()` to see choices).
#' @param aesthetic One of `"color"` or `"fill"`.
#' @param type One of `"discrete"` (default) or `"continuous"`.  Continuous
#'   works best with sequential/diverging palettes.
#' @param alpha Numeric in (0, 1]. Opacity applied to all colours.
#' @param reverse Logical. If `TRUE`, reverse the palette order.
#' @param ... Further arguments passed to the underlying
#'   `ggplot2::scale_*_manual()` or `ggplot2::scale_*_gradientn()`.
#'
#' @returns A ggplot2 Scale object.
#'
#' @examples
#' \dontrun{
#' library(ggplot2)
#'
#' # Discrete colour
#' ggplot(iris, aes(Sepal.Length, Petal.Length, colour = Species)) +
#'   geom_point(size = 3) +
#'   palette_scale("jama", "color")
#'
#' # Discrete fill
#' ggplot(mpg, aes(class, fill = class)) +
#'   geom_bar() +
#'   palette_scale("cosmic", "fill")
#'
#' # Continuous fill (heatmap)
#' ggplot(faithfuld, aes(waiting, eruptions, fill = density)) +
#'   geom_tile() +
#'   palette_scale("heatmap_rdbu", "fill", type = "continuous")
#' }
#'
#' @export
palette_scale = function(palette = "jco",
                          aesthetic = c("color", "fill"),
                          type = c("discrete", "continuous"),
                          alpha = 1,
                          reverse = FALSE,
                          ...) {
  aesthetic = match.arg(aesthetic)
  type = match.arg(type)

  if (type == "continuous") {
    palette_continuous(aesthetic, palette, alpha, reverse, ...)
  } else {
    palette_discrete(aesthetic, palette, alpha, reverse, ...)
  }
}

# ==========================================================================
# Convenience wrappers kept short
# ==========================================================================

#' @rdname palette_scale
#' @export
scale_color_cli = function(palette = "jco", alpha = 1, reverse = FALSE, ...) {
  palette_scale(palette, "color", "discrete", alpha, reverse, ...)
}

#' @rdname palette_scale
#' @export
scale_fill_cli = function(palette = "jco", alpha = 1, reverse = FALSE, ...) {
  palette_scale(palette, "fill", "discrete", alpha, reverse, ...)
}

#' @rdname palette_scale
#' @export
scale_color_cli_c = function(palette = "blues", alpha = 1, reverse = FALSE, ...) {
  palette_scale(palette, "color", "continuous", alpha, reverse, ...)
}

#' @rdname palette_scale
#' @export
scale_fill_cli_c = function(palette = "blues", alpha = 1, reverse = FALSE, ...) {
  palette_scale(palette, "fill", "continuous", alpha, reverse, ...)
}
