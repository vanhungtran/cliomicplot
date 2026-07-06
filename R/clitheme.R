# ===========================================================================
# cliomicplot: Theme System (clitheme)
# Inspired by tinyplot's tinytheme.R
# ===========================================================================

# Internal registry of theme definitions
.theme_registry = list()

# ==========================================================================
# Theme Definitions
# ==========================================================================

# Core theme templates (applied on top of theme_cli_base)

theme_cli_bw = function(base_size = 11, base_family = "") {
  theme_cli_base(base_size, base_family) +
    ggplot2::theme(
      panel.border     = ggplot2::element_rect(color = "black", fill = NA, linewidth = 0.5),
      panel.grid.major = ggplot2::element_line(linewidth = 0.3, color = "grey90"),
      panel.grid.minor = ggplot2::element_blank(),
      axis.line        = ggplot2::element_blank(),
      axis.ticks       = ggplot2::element_line(color = "black", linewidth = 0.4),
      legend.background = ggplot2::element_rect(color = NA, fill = "white"),
      legend.key       = ggplot2::element_rect(color = NA, fill = "white"),
      strip.background = ggplot2::element_rect(fill = "grey95", color = "grey80"),
      strip.text       = ggplot2::element_text(face = "bold", size = base_size - 1)
    )
}

theme_cli_classic = function(base_size = 11, base_family = "") {
  theme_cli_base(base_size, base_family) +
    ggplot2::theme(
      panel.border     = ggplot2::element_blank(),
      panel.grid.major = ggplot2::element_line(linewidth = 0.3, color = "grey92"),
      panel.grid.minor = ggplot2::element_blank(),
      axis.line        = ggplot2::element_line(color = "black", linewidth = 0.5),
      axis.ticks       = ggplot2::element_line(color = "black", linewidth = 0.4),
      legend.background = ggplot2::element_rect(color = NA, fill = "white"),
      legend.key       = ggplot2::element_rect(color = NA, fill = "white"),
      strip.background = ggplot2::element_blank(),
      strip.text       = ggplot2::element_text(face = "bold", size = base_size - 1)
    )
}

theme_cli_minimal = function(base_size = 11, base_family = "") {
  theme_cli_base(base_size, base_family) +
    ggplot2::theme(
      panel.border     = ggplot2::element_blank(),
      panel.grid.major = ggplot2::element_line(linewidth = 0.3, color = "grey95"),
      panel.grid.minor = ggplot2::element_blank(),
      axis.line        = ggplot2::element_line(color = "black", linewidth = 0.4),
      axis.ticks       = ggplot2::element_line(color = "black", linewidth = 0.3),
      legend.background = ggplot2::element_rect(color = NA, fill = "white"),
      legend.key       = ggplot2::element_rect(color = NA, fill = "white"),
      strip.background = ggplot2::element_blank(),
      strip.text       = ggplot2::element_text(face = "bold", size = base_size - 1)
    )
}

theme_cli_nature = function(base_size = 10, base_family = "") {
  # Nature Publishing Group style
  theme_cli_minimal(base_size, base_family) +
    ggplot2::theme(
      axis.line        = ggplot2::element_line(color = "black", linewidth = 0.3),
      axis.ticks       = ggplot2::element_line(color = "black", linewidth = 0.3),
      axis.text        = ggplot2::element_text(size = 8, color = "black"),
      axis.title       = ggplot2::element_text(size = 9),
      plot.title       = ggplot2::element_text(size = 10, face = "bold", hjust = 0),
      plot.subtitle    = ggplot2::element_text(size = 9, hjust = 0, color = "grey30"),
      plot.margin      = ggplot2::margin(6, 6, 6, 6),
      panel.grid.major = ggplot2::element_blank(),
      panel.grid.minor = ggplot2::element_blank(),
      legend.text      = ggplot2::element_text(size = 8),
      legend.title     = ggplot2::element_text(size = 8, face = "bold"),
      legend.key.size  = ggplot2::unit(0.5, "cm")
    )
}

theme_cli_science = function(base_size = 10, base_family = "") {
  # Science/AAAS style
  theme_cli_minimal(base_size, base_family) +
    ggplot2::theme(
      axis.line        = ggplot2::element_line(color = "black", linewidth = 0.3),
      axis.ticks       = ggplot2::element_line(color = "black", linewidth = 0.3),
      axis.text        = ggplot2::element_text(size = 7, color = "black"),
      axis.title       = ggplot2::element_text(size = 8),
      plot.title       = ggplot2::element_text(size = 9, face = "bold", hjust = 0),
      plot.subtitle    = ggplot2::element_text(size = 8, hjust = 0, color = "grey40"),
      plot.margin      = ggplot2::margin(5, 5, 5, 5),
      panel.grid.major = ggplot2::element_blank(),
      panel.grid.minor = ggplot2::element_blank(),
      legend.text      = ggplot2::element_text(size = 7),
      legend.title     = ggplot2::element_text(size = 7, face = "bold"),
      legend.key.size  = ggplot2::unit(0.4, "cm"),
      strip.text       = ggplot2::element_text(size = 8, face = "bold")
    )
}

theme_cli_nejm = function(base_size = 10, base_family = "") {
  # NEJM style
  theme_cli_minimal(base_size, base_family) +
    ggplot2::theme(
      axis.line        = ggplot2::element_line(color = "black", linewidth = 0.4),
      axis.ticks       = ggplot2::element_line(color = "black", linewidth = 0.4),
      axis.text        = ggplot2::element_text(size = 8, color = "black"),
      axis.title       = ggplot2::element_text(size = 9),
      plot.title       = ggplot2::element_text(size = 10, face = "bold", hjust = 0.5),
      panel.grid.major = ggplot2::element_line(linewidth = 0.2, color = "grey95"),
      legend.text      = ggplot2::element_text(size = 8),
      legend.title     = ggplot2::element_text(size = 8, face = "bold"),
      strip.text       = ggplot2::element_text(size = 9, face = "bold")
    )
}

theme_cli_lancet = function(base_size = 10, base_family = "") {
  # The Lancet style
  theme_cli_minimal(base_size, base_family) +
    ggplot2::theme(
      axis.line        = ggplot2::element_line(color = "black", linewidth = 0.3),
      axis.ticks       = ggplot2::element_line(color = "black", linewidth = 0.3),
      axis.text        = ggplot2::element_text(size = 8, color = "black"),
      axis.title       = ggplot2::element_text(size = 9),
      plot.title       = ggplot2::element_text(size = 10, face = "bold", hjust = 0),
      panel.grid.major = ggplot2::element_line(linewidth = 0.2, color = "grey95"),
      legend.text      = ggplot2::element_text(size = 8),
      legend.title     = ggplot2::element_text(size = 8, face = "bold"),
      strip.text       = ggplot2::element_text(size = 9, face = "bold")
    )
}

theme_cli_cell = function(base_size = 10, base_family = "") {
  # Cell Press style
  theme_cli_minimal(base_size, base_family) +
    ggplot2::theme(
      axis.line        = ggplot2::element_line(color = "black", linewidth = 0.3),
      axis.ticks       = ggplot2::element_line(color = "black", linewidth = 0.3),
      axis.text        = ggplot2::element_text(size = 7, color = "black"),
      axis.title       = ggplot2::element_text(size = 8),
      plot.title       = ggplot2::element_text(size = 9, face = "bold", hjust = 0),
      panel.grid.major = ggplot2::element_blank(),
      panel.grid.minor = ggplot2::element_blank(),
      legend.text      = ggplot2::element_text(size = 7),
      legend.title     = ggplot2::element_text(size = 7, face = "bold"),
      legend.key.size  = ggplot2::unit(0.4, "cm"),
      strip.text       = ggplot2::element_text(size = 8, face = "bold")
    )
}

theme_cli_dark = function(base_size = 11, base_family = "") {
  theme_cli_minimal(base_size, base_family) +
    ggplot2::theme(
      plot.background  = ggplot2::element_rect(fill = "#2C2C2C", color = NA),
      panel.background = ggplot2::element_rect(fill = "#2C2C2C", color = NA),
      panel.grid.major = ggplot2::element_line(linewidth = 0.2, color = "grey30"),
      panel.grid.minor = ggplot2::element_blank(),
      axis.line        = ggplot2::element_line(color = "grey60", linewidth = 0.4),
      axis.ticks       = ggplot2::element_line(color = "grey60", linewidth = 0.3),
      axis.text        = ggplot2::element_text(color = "grey90"),
      axis.title       = ggplot2::element_text(color = "grey90"),
      plot.title       = ggplot2::element_text(color = "white", face = "bold"),
      plot.subtitle    = ggplot2::element_text(color = "grey80"),
      plot.caption     = ggplot2::element_text(color = "grey70"),
      legend.background = ggplot2::element_rect(fill = "#2C2C2C", color = NA),
      legend.key       = ggplot2::element_rect(fill = "#2C2C2C", color = NA),
      legend.text      = ggplot2::element_text(color = "grey90"),
      legend.title     = ggplot2::element_text(color = "grey90"),
      strip.background = ggplot2::element_rect(fill = "grey20", color = NA),
      strip.text       = ggplot2::element_text(color = "grey90", face = "bold")
    )
}

theme_cli_showcase = function(base_size = 12, base_family = "") {
  theme_cli_base(base_size, base_family) +
    ggplot2::theme(
      plot.background  = ggplot2::element_rect(fill = "#FBFCFE", color = NA),
      panel.background = ggplot2::element_rect(fill = "white", color = NA),
      panel.border     = ggplot2::element_rect(color = "#C7D0DD", fill = NA, linewidth = 0.5),
      panel.grid.major = ggplot2::element_line(linewidth = 0.25, color = "#E5EAF1"),
      panel.grid.minor = ggplot2::element_blank(),
      axis.ticks       = ggplot2::element_line(color = "#59616C", linewidth = 0.35),
      plot.title       = ggplot2::element_text(
        size = base_size + 5, face = "bold", color = "#17212F",
        hjust = 0, margin = ggplot2::margin(b = 7)
      ),
      plot.subtitle    = ggplot2::element_text(
        size = base_size, color = "#5B6472", hjust = 0,
        margin = ggplot2::margin(b = 12)
      ),
      axis.title       = ggplot2::element_text(size = base_size, color = "#17212F"),
      axis.text        = ggplot2::element_text(size = base_size - 1, color = "#28313D"),
      strip.background = ggplot2::element_rect(fill = "#EAF0F8", color = "#C7D0DD", linewidth = 0.4),
      strip.text       = ggplot2::element_text(size = base_size - 1, face = "bold", color = "#17212F"),
      legend.position  = "right",
      legend.box.background = ggplot2::element_rect(color = "#D7DEE8", fill = "white", linewidth = 0.35),
      legend.box.margin = ggplot2::margin(3, 3, 3, 3),
      legend.key       = ggplot2::element_rect(fill = "white", color = NA),
      legend.title     = ggplot2::element_text(size = base_size - 1, face = "bold", color = "#17212F"),
      legend.text      = ggplot2::element_text(size = base_size - 1, color = "#28313D")
    )
}

# Publication-style theme with controlled grid and subtle axis ticks
theme_cli_broadsheet = function(base_size = 14, base_family = "") {
  ggplot2::theme_bw(base_size = base_size, base_family = base_family) +
    ggplot2::theme(
      # --- Text: two-tier hierarchy for title/body ---
      plot.title    = ggplot2::element_text(
        size   = base_size + 5, face = "bold",
        color  = "#2c2c2c", margin = ggplot2::margin(b = 12),
        hjust  = 0),
      plot.subtitle = ggplot2::element_text(
        size   = base_size + 1, color = "#6b6b6b",
        margin = ggplot2::margin(b = 14), hjust = 0),
      plot.caption  = ggplot2::element_text(
        size   = base_size - 2, color = "#8c8c8c",
        margin = ggplot2::margin(t = 18), hjust = 1, face = "italic"),
      axis.title    = ggplot2::element_text(
        size   = base_size, color = "#444444"),
      axis.text     = ggplot2::element_text(
        size   = base_size - 2, color = "#333333"),

      # --- Panel: light fill, subtle border ---
      panel.background = ggplot2::element_rect(fill = "#fafafa", color = NA),
      panel.border     = ggplot2::element_rect(
        color = "#cccccc", fill = NA, linewidth = 0.4),

      # --- Grid: horizontal only, very faint ---
      panel.grid.major.y = ggplot2::element_line(
        linewidth = 0.18, color = "#e0e0e0", linetype = "solid"),
      panel.grid.major.x = ggplot2::element_blank(),
      panel.grid.minor   = ggplot2::element_blank(),

      # --- Ticks: short, outward on both axes ---
      axis.ticks         = ggplot2::element_line(
        color = "#888888", linewidth = 0.3),
      axis.ticks.length  = ggplot2::unit(3, "pt"),

      # --- Facet strips: clean outline style ---
      strip.background = ggplot2::element_rect(
        fill = "white", color = "#cccccc", linewidth = 0.4),
      strip.text = ggplot2::element_text(
        size = base_size - 2, face = "bold", color = "#444444"),

      # --- Legend: compact, matching style ---
      legend.background = ggplot2::element_rect(fill = "white", color = NA),
      legend.key        = ggplot2::element_rect(fill = "white", color = NA),
      legend.text       = ggplot2::element_text(
        size = base_size - 2, color = "#444444"),
      legend.title      = ggplot2::element_text(
        size = base_size - 1, face = "bold", color = "#2c2c2c")
    )
}

# Map of available themes
.theme_map = list(
  cli_bw      = theme_cli_bw,
  cli_classic = theme_cli_classic,
  cli_minimal = theme_cli_minimal,
  nature      = theme_cli_nature,
  science     = theme_cli_science,
  nejm        = theme_cli_nejm,
  lancet      = theme_cli_lancet,
  cell        = theme_cli_cell,
  dark        = theme_cli_dark,
  showcase    = theme_cli_showcase,
  broadsheet  = theme_cli_broadsheet
)

# ==========================================================================
# Public Theme Functions
# ==========================================================================

#' Set or Reset Plot Themes for cliomicplot
#'
#' @description
#' Sets or resets the theme for cliomicplot plots. Themes control the overall
#' appearance including fonts, grid lines, axis styles, and colors. Inspired by
#' tinyplot's `tinytheme()`, themes can be persistent (applied globally) or
#' ephemeral (for a single plot call).
#'
#' @param theme Character string specifying the theme name. Available themes:
#'   \itemize{
#'     \item \code{"cli_bw"} - Clean black-and-white theme (default)
#'     \item \code{"cli_classic"} - Classic theme with axis lines, no panel border
#'     \item \code{"cli_minimal"} - Minimal theme, clean and modern
#'     \item \code{"nature"} - Nature Publishing Group style
#'     \item \code{"science"} - Science/AAAS journal style
#'     \item \code{"nejm"} - NEJM (New England Journal of Medicine) style
#'     \item \code{"lancet"} - The Lancet style
#'     \item \code{"cell"} - Cell Press style
#'     \item \code{"broadsheet"} - Publication print style with subtle grid
#'     \item \code{"showcase"} - Polished presentation style with stronger hierarchy
#'     \item \code{"dark"} - Dark background theme
#'   }
#'   Use \code{clitheme()} without arguments to reset to the default theme.
#' @param ... Named arguments to override specific theme settings (passed to
#'   ggplot2's \code{theme()}).
#' @param register Optional character string to register this theme under a
#'   custom name via \code{\link{clitheme_register}}.
#'
#' @details
#' **Persistent vs. ephemeral themes.** Calling \code{clitheme("nature")}
#' triggers a persistent theme applied to all subsequent ggplot2 plots. Reset
#' by calling \code{clitheme()} without arguments. For ephemeral (single-plot)
#' themes, pass \code{theme = "nature"} directly to \code{\link{cliplot}}.
#'
#' @examples
#' \dontrun{
#' # Set a persistent theme
#' clitheme("nature")
#' cliplot(mpg ~ wt, data = mtcars)
#'
#' # Reset to default
#' clitheme()
#'
#' # Ephemeral theme for one plot
#' cliplot(mpg ~ wt, data = mtcars, theme = "dark")
#' }
#'
#' @export
clitheme = function(theme = NULL, ..., register = NULL) {
  if (is.null(theme)) {
    # Reset to default
    ggplot2::theme_set(theme_cli_base())
    set_environment_variable(.active_theme = "cli_bw")
    return(invisible())
  }

  if (is.character(theme) && theme %in% names(.theme_map)) {
    thm = .theme_map[[theme]]()

    # Apply overrides from ...
    dots = list(...)
    if (length(dots) > 0) {
      thm = thm + do.call(ggplot2::theme, dots)
    }

    ggplot2::theme_set(thm)
    set_environment_variable(.active_theme = theme)

    # Also register if requested
    if (!is.null(register)) {
      clitheme_register(register, theme, ...)
    }

    message("Theme set to: ", theme)
  } else if (is.character(theme)) {
    # Check registered themes
    reg = get_environment_variable(".registered_themes")
    if (theme %in% names(reg)) {
      ggplot2::theme_set(reg[[theme]])
      set_environment_variable(.active_theme = theme)
      message("Theme set to registered theme: ", theme)
    } else {
      stop("Unknown theme: ", theme, "\nAvailable themes: ",
           paste(c(names(.theme_map), names(reg)), collapse = ", "))
    }
  }

  invisible()
}

#' List Available Themes
#'
#' @description Returns a character vector of all available and registered themes.
#'
#' @return Character vector of theme names.
#' @export
clitheme_list = function() {
  builtin = names(.theme_map)
  registered = names(get_environment_variable(".registered_themes"))
  unique(c(builtin, registered))
}

#' Register a Custom Theme
#'
#' @description Register a custom ggplot2 theme under a given name for use
#'   with \code{\link{clitheme}} and \code{\link{cliplot}}.
#'
#' @param name Character string. Name to register the theme under.
#' @param base Character string. Base theme name to build on.
#' @param ... Additional theme elements passed to \code{ggplot2::theme()}.
#'
#' @return Invisibly returns the registered theme name.
#' @export
clitheme_register = function(name, base = "cli_bw", ...) {
  if (!base %in% names(.theme_map)) {
    stop("Unknown base theme: ", base)
  }

  thm = .theme_map[[base]]()
  dots = list(...)
  if (length(dots) > 0) {
    thm = thm + do.call(ggplot2::theme, dots)
  }

  reg = get_environment_variable(".registered_themes")
  reg[[name]] = thm
  set_environment_variable(.registered_themes = reg)

  message("Theme registered: ", name)
  invisible(name)
}

#' Unregister a Custom Theme
#'
#' @description Remove a previously registered custom theme.
#'
#' @param name Character string. Name of the theme to unregister.
#' @export
clitheme_unregister = function(name) {
  reg = get_environment_variable(".registered_themes")
  if (name %in% names(reg)) {
    reg[[name]] = NULL
    set_environment_variable(.registered_themes = reg)
    message("Theme unregistered: ", name)
  } else {
    warning("Theme not found: ", name)
  }
  invisible()
}

#' Resolve a theme: returns a ggplot2 theme object from a name or object
#' @keywords internal
resolve_theme = function(theme) {
  if (is.null(theme)) {
    theme = get_environment_variable(".active_theme") %||% "cli_bw"
  }
  if (is.character(theme)) {
    if (theme %in% names(.theme_map)) {
      .theme_map[[theme]]()
    } else {
      reg = get_environment_variable(".registered_themes")
      if (theme %in% names(reg)) reg[[theme]] else theme_cli_base()
    }
  } else if (inherits(theme, "theme")) {
    theme
  } else if (inherits(theme, "gg")) {
    theme
  } else {
    theme_cli_base()
  }
}

# Apply a theme ephemerally (save current, apply new, restore on exit)
with_ephemeral_theme = function(theme, expr) {
  old = ggplot2::theme_get()
  on.exit(ggplot2::theme_set(old), add = TRUE)
  new_theme = resolve_theme(theme)
  ggplot2::theme_set(new_theme)
  eval(expr)
}
