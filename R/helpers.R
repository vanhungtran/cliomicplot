# ===========================================================================
# cliomicplot: Publication Helpers
# Panel labels, journal-size export, and legend helpers.
# Inspired by cnsplots figure_setup.py / multipanel.py
# ===========================================================================

#' Add a Panel Label (A, B, C ...) to a ggplot
#'
#' @description Adds a bold panel label in the top-left corner of a ggplot,
#'   following the convention used in Nature/Cell/Science multi-panel figures.
#'   Returns a ggplot layer so it composes with \code{+}.
#'
#' @param label Panel label, e.g. \code{"A"}, \code{"B"}, or \code{"Fig. 1a"}.
#' @param x,y Position of the label. Defaults to the top-left corner of the
#'   plotting region (\code{-Inf}, \code{Inf}).
#' @param hjust,vjust Justification of the label.
#' @param size Label size in ggplot units (default 3.2).
#' @param fontface Font face (default "bold").
#' @param color Label color (default "black").
#' @param family Font family.
#' @param ... Additional arguments passed to \code{ggplot2::annotate()}.
#'
#' @return A ggplot layer (from \code{ggplot2::annotate()}).
#'
#' @examples
#' \dontrun{
#' p1 = cliplot(mpg ~ wt, data = mtcars, theme = "nature") + cli_panel_label("A")
#' p2 = cliplot(~ cyl, data = mtcars, type = "barplot", theme = "nature") + cli_panel_label("B")
#' }
#'
#' @export
cli_panel_label = function(label, x = NULL, y = NULL, hjust = NULL, vjust = NULL,
                           size = 3.2, fontface = "bold", color = "black",
                           family = "", ...) {
  ggplot2::annotate(
    "text",
    x      = x %||% -Inf,
    y      = y %||% Inf,
    label  = label,
    hjust  = hjust %||% -0.25,
    vjust  = vjust %||% 1.3,
    size   = size,
    fontface = fontface,
    color  = color,
    family = family,
    ...
  )
}

#' Move the Legend Outside the Plot
#'
#' @description Theme helper that moves a ggplot legend outside the plotting
#'   area, the default layout for multi-panel Nature/Cell/Science figures.
#'
#' @param position Legend position: \code{"right"} (default), \code{"left"},
#'   \code{"top"}, or \code{"bottom"}.
#' @param margin Extra margin between the plot and the legend (default 8 pt on
#'   the inner side).
#' @param ... Additional arguments passed to \code{ggplot2::theme()}.
#'
#' @return A ggplot theme object for use with \code{+}.
#'
#' @examples
#' \dontrun{
#' cliplot(Sepal.Length ~ Petal.Length | Species, data = iris) + cli_legend_out()
#' }
#'
#' @export
cli_legend_out = function(position = "right", margin = 8, ...) {
  side_margin = switch(position,
    right  = ggplot2::margin(0, 0, 0, margin),
    left   = ggplot2::margin(0, margin, 0, 0),
    top    = ggplot2::margin(0, 0, margin, 0),
    bottom = ggplot2::margin(margin, 0, 0, 0),
    ggplot2::margin(0, 0, 0, margin)
  )
  ggplot2::theme(
    legend.position = position,
    legend.box.margin = side_margin,
    ...
  )
}

#' Get Exact Figure Dimensions for Specific Scientific Journals
#'
#' @description Returns the exact column widths in millimeters and inches for 9
#'   major scientific publishers, derived from verified publisher presets.
#'   Useful together with \code{\link{cli_save}}.
#'
#' @param journal One of \code{"nature"}, \code{"natcomms"}, \code{"science"},
#'   \code{"cell"}, \code{"jama"}, \code{"nejm"}, \code{"lancet"},
#'   \code{"plos"}, \code{"ieee"}.
#' @param column One of \code{"single"}, \code{"one_half"}, \code{"double"},
#'   \code{"full"}.
#'
#' @return A list with elements \code{width_mm}, \code{width_in},
#'   \code{height_in} (default 3:4 aspect), \code{max_height_mm},
#'   \code{journal}, and \code{column}.
#'
#' @examples
#' cli_journal_size("nature", "single")
#' cli_journal_size("jama", "double")
#'
#' @export
cli_journal_size = function(journal = c("nature", "natcomms", "science", "cell",
                                        "jama", "nejm", "lancet", "plos", "ieee"),
                            column = c("single", "one_half", "double", "full")) {
  j_raw = tolower(trimws(if (missing(journal)) "nature" else journal[1]))
  j_alias = c(
    "nature" = "nature",
    "nature communications" = "natcomms",
    "natcomms" = "natcomms",
    "science" = "science",
    "cell" = "cell",
    "jama" = "jama",
    "nejm" = "nejm",
    "lancet" = "lancet",
    "the lancet" = "lancet",
    "plos" = "plos",
    "plos one" = "plos",
    "ieee" = "ieee"
  )
  j_key = if (j_raw %in% names(j_alias)) j_alias[[j_raw]] else j_raw
  if (!j_key %in% c("nature", "natcomms", "science", "cell", "jama", "nejm", "lancet", "plos", "ieee")) {
    stop("Unknown journal: '", j_raw, "'. Available: nature, natcomms, science, cell, jama, nejm, lancet, plos, ieee.")
  }

  if (is.numeric(column)) {
    if (column == 1) column = "single"
    else if (column == 1.5) column = "one_half"
    else if (column >= 2) column = "double"
    else column = "single"
  } else {
    column = match.arg(column)
  }

  # Exact publisher column widths in millimeters (verified publisher presets)
  widths = list(
    nature   = c(single = 89.0, one_half = 120.0, double = 183.0, full = 183.0),
    natcomms = c(single = 88.0, one_half = 135.0, double = 180.0, full = 180.0),
    science  = c(single = 90.0, one_half = 121.0, double = 183.0, full = 183.0),
    cell     = c(single = 85.0, one_half = 114.0, double = 180.0, full = 180.0),
    jama     = c(single = 86.4, one_half = 127.0, double = 177.8, full = 177.8),
    nejm     = c(single = 89.0, one_half = 120.0, double = 180.0, full = 180.0),
    lancet   = c(single = 85.0, one_half = 120.0, double = 175.0, full = 175.0),
    plos     = c(single = 83.0, one_half = 132.0, double = 190.5, full = 190.5),
    ieee     = c(single = 88.9, one_half = 135.0, double = 181.9, full = 181.9)
  )

  max_heights = list(
    nature   = 170.0,
    natcomms = 225.0,
    science  = 230.0,
    cell     = 203.0,
    jama     = 220.0,
    nejm     = 220.0,
    lancet   = 220.0,
    plos     = 222.3,
    ieee     = 220.0
  )

  w_mm = widths[[j_key]][[column]]
  w_in = w_mm / 25.4
  h_mm = min(w_mm * 0.8, max_heights[[j_key]])
  h_in = h_mm / 25.4

  list(
    journal       = j_key,
    column        = column,
    width_mm      = w_mm,
    height_mm     = h_mm,
    width_in      = w_in,
    height_in     = h_in,
    max_height_mm = max_heights[[j_key]]
  )
}

#' Save a Figure at Journal-Reference Dimensions
#'
#' @description Saves a ggplot at the exact width (and optional height) required
#'   by Nature, Nature Communications, Science, Cell, JAMA, NEJM, Lancet, PLOS,
#'   or IEEE at print-resolution DPI. Emits publisher format compliance guidance
#'   and height checks so submission figures pass publisher gates.
#'
#' @param plot A ggplot object (defaults to the last plot).
#' @param file Output file path; the extension determines the format (pdf,
#'   png, svg, jpg, tiff).
#' @param journal One of \code{"nature"}, \code{"natcomms"}, \code{"science"},
#'   \code{"cell"}, \code{"jama"}, \code{"nejm"}, \code{"lancet"},
#'   \code{"plos"}, \code{"ieee"}. Ignored when \code{width} is supplied directly.
#' @param column One of \code{"single"}, \code{"one_half"}, \code{"double"},
#'   \code{"full"}.
#' @param width,height Plot dimensions in inches (or in \code{units}). When
#'   \code{NULL}, derived from the journal reference specifications.
#' @param dpi Resolution in dots per inch (default 300; use 600 for
#'   submission-ready line art).
#' @param units Units for \code{width}/\code{height} (default "in").
#' @param ... Additional arguments passed to \code{ggplot2::ggsave()}.
#'
#' @return Invisibly returns the file path.
#'
#' @examples
#' \dontrun{
#' p = cliplot(mpg ~ wt, data = mtcars, theme = "nature")
#' cli_save(p, "figure1.pdf", journal = "nature", column = "double", dpi = 600)
#' }
#'
#' @export
cli_save = function(plot = ggplot2::last_plot(), file = "figure.png",
                    journal = c("nature", "natcomms", "science", "cell",
                                "jama", "nejm", "lancet", "plos", "ieee"),
                    column = c("single", "one_half", "double", "full"),
                    width = NULL, height = NULL, dpi = 300,
                    units = "in", ...) {
  j_used = NULL
  if (is.null(width) && !missing(journal)) {
    sz = cli_journal_size(journal, column)
    width  = sz$width_in
    height = height %||% sz$height_in
    j_used = sz$journal

    # Format check guidance from publisher specs
    ext = tolower(tools::file_ext(file))
    if (ext %in% c("png", "jpg", "jpeg") && j_used %in% c("nature", "science")) {
      message("Note [Publication Standards]: ", toupper(j_used), " guidelines specify vector PDF/EPS/SVG ",
              "for final submission figures. PNG is suitable for previews.")
    }

    # Height check
    height_mm = height * (if (units == "mm") 1 else if (units == "cm") 10 else 25.4)
    if (!is.null(sz$max_height_mm) && height_mm > sz$max_height_mm) {
      warning("Figure height (", round(height_mm, 1), " mm) exceeds ", j_used,
              " maximum printable height (", sz$max_height_mm, " mm).", call. = FALSE)
    }
  } else {
    width  = width %||% 7
    height = height %||% 5
  }

  ggplot2::ggsave(
    filename = file,
    plot     = plot,
    width    = width,
    height   = height,
    dpi      = dpi,
    units    = units,
    ...
  )
  invisible(file)
}
