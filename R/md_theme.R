# ===========================================================================
# cliomicplot: Rich Text / Markdown Rendering in Plot Labels
#
# Uses ggtext::element_markdown() to enable markdown syntax in ggplot2 text
# elements. The design uses a target-list approach: the user specifies which
# text elements should render markdown, rather than blanket-converting a theme.
# ===========================================================================

# Named list mapping user-friendly element names to ggplot2 theme element names
.md_element_map = list(
  title    = "plot.title",
  subtitle = "plot.subtitle",
  caption  = "plot.caption",
  xlab     = "axis.title.x",
  ylab     = "axis.title.y",
  xtitle   = "axis.title.x",
  ytitle   = "axis.title.y",
  xtext    = "axis.text.x",
  ytext    = "axis.text.y",
  axis_title = c("axis.title.x", "axis.title.y"),
  axis_text  = c("axis.text.x", "axis.text.y"),
  legend_title = "legend.title",
  legend_text  = "legend.text",
  strip     = "strip.text",
  strip_x   = "strip.text.x",
  strip_y   = "strip.text.y",
  all       = "__all__"
)

#' Enable Markdown Rendering in Plot Text Elements
#'
#' @description
#' Selectively enables markdown rendering for specific ggplot2 text elements
#' using `ggtext::element_markdown()`. Unlike a blanket theme converter, this
#' lets you target exactly which labels should interpret markdown (e.g., only
#' the title and subtitle, leaving axis labels as plain text).
#'
#' @param ... Character strings naming which text elements to enable markdown
#'   for. Supported names: `"title"`, `"subtitle"`, `"caption"`, `"xlab"`,
#'   `"ylab"`, `"xtext"`, `"ytext"`, `"axis_title"`, `"axis_text"`,
#'   `"legend_title"`, `"legend_text"`, `"strip"`, `"all"`. If empty, enables
#'   markdown for `"title"`, `"subtitle"`, and `"caption"` (the most common
#'   use case). Use `"all"` to enable everywhere.
#' @param base_family Base font family for markdown text. Default `""` uses
#'   the current theme default.
#' @param base_size Base font size. `NULL` preserves the current theme's size.
#'
#' @returns A ggplot2 theme object that can be added to any plot.
#'
#' @details
#' Supported markdown syntax in labels:
#' \itemize{
#'   \item `**bold**` — bold text
#'   \item `*italics*` — italic text
#'   \item `***bold italic***` — combined
#'   \item `<span style='color:red'>text</span>` — colored spans
#'   \item `<br>` — line break
#'   \item `<sub>subscript</sub>` / `<sup>superscript</sup>`
#' }
#'
#' @examples
#' \dontrun{
#' library(ggplot2)
#'
#' p = ggplot(mtcars, aes(hp, mpg)) +
#'   geom_point() +
#'   labs(
#'     title = "**MPG** vs *Horsepower*",
#'     subtitle = "n = 32 vehicles",
#'     caption = "Source: *Motor Trend* (1974)"
#'   )
#'
#' # Enable markdown only on title + subtitle + caption
#' p + cli_markdown()
#'
#' # Enable on everything
#' p + cli_markdown("all")
#'
#' # Only on title
#' p + cli_markdown("title")
#' }
#'
#' @export
cli_markdown = function(..., base_family = "", base_size = NULL) {
  targets = unlist(list(...))

  # Default: title, subtitle, caption (most common use case)
  if (length(targets) == 0) {
    targets = c("title", "subtitle", "caption")
  }

  # Resolve "all" shortcut
  if ("all" %in% targets) {
    targets = c("title", "subtitle", "caption", "xlab", "ylab",
                "xtext", "ytext", "legend_title", "legend_text",
                "strip")
  }

  # Map user names to theme element names
  element_names = character(0)
  for (tgt in targets) {
    if (tgt %in% names(.md_element_map)) {
      mapped = .md_element_map[[tgt]]
      element_names = c(element_names, mapped)
    } else {
      warning("Unknown markdown target: ", tgt, ". Ignored.")
    }
  }
  element_names = unique(element_names)

  # Build the markdown element
  md_elem_args = list()
  if (nzchar(base_family)) md_elem_args$family = base_family
  if (!is.null(base_size))  md_elem_args$size = base_size

  md_element = do.call(ggtext::element_markdown, md_elem_args)

  # Construct theme overrides
  theme_overrides = list()
  for (en in element_names) {
    theme_overrides[[en]] = md_element
  }

  do.call(ggplot2::theme, theme_overrides)
}

#' Enable Markdown Globally for the Active Theme
#'
#' @description
#' Applies `cli_markdown()` to the currently active theme and sets it
#' globally so all subsequent plots render markdown in their text.
#'
#' @param ... Passed to `cli_markdown()`.
#'
#' @export
clitheme_md = function(...) {
  md_addon = cli_markdown(...)
  current = ggplot2::theme_get()
  new_theme = current + md_addon
  ggplot2::theme_set(new_theme)
  message("Markdown rendering enabled for targets: ",
          paste(if (length(list(...)) == 0) c("title","subtitle","caption")
                else unlist(list(...)), collapse = ", "))
  invisible(new_theme)
}

#' Disable Global Markdown Rendering
#'
#' @description
#' Resets the active ggplot2 theme to the base cliomicplot theme
#' without markdown support.
#'
#' @export
clitheme_md_reset = function() {
  thm_name = get_environment_variable(".active_theme") %||% "cli_bw"
  new_theme = resolve_theme(thm_name)
  ggplot2::theme_set(new_theme)
  message("Markdown rendering disabled; theme reset to: ", thm_name)
  invisible(new_theme)
}
