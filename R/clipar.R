# ===========================================================================
# cliomicplot: Extended Graphical Parameters (clipar)
# Inspired by tinyplot's tpar.R
# ===========================================================================

# Internal storage for cliomicplot-specific parameters
.clipar = new.env()

# Default parameters
.clipar_defaults = list(
  # Color
  palette.qualitative  = "npg",
  palette.sequential   = "blues",
  palette.diverging    = "heatmap_rdbu",
  col.default          = "#2B6CB0",

  # Themes
  theme.default        = "cli_bw",

  # Facet
  facet.bg             = "grey95",
  facet.border         = NA,
  facet.cex            = 1,
  facet.col            = "black",
  facet.font           = 1,
  fmar                 = c(1, 1, 1, 1),

  # Legend
  legend.position      = "right",
  legend.direction     = "vertical",
  lmar                 = c(1.0, 0.1),

  # Grid
  grid                 = FALSE,
  grid.col             = "grey90",
  grid.lty             = "dotted",
  grid.lwd             = 0.3,

  # File output
  file.width           = 8,
  file.height          = 6,
  file.res             = 300,

  # Statistical annotations
  stat.test            = NULL,
  stat.label           = "p.format",
  stat.pcutoff         = 0.05,

  # Text
  cex.main             = 1.1,
  cex.sub              = 0.9,
  cex.cap              = 0.8,
  cex.axis             = 0.9,
  cex.lab              = 1.0,
  font.main            = 2,
  font.sub             = 1,
  font.cap             = 1,

  # Ribbon/area alpha
  ribbon.alpha         = 0.2
)

# Initialize defaults
for (nm in names(.clipar_defaults)) {
  .clipar[[nm]] = .clipar_defaults[[nm]]
}

#' Set or Query Cliomicplot Graphical Parameters
#'
#' @description
#' Sets or queries graphical parameters for cliomicplot, extending ggplot2's
#' theme system with cliomicplot-specific options. Similar to tinyplot's
#' `tpar()`, parameters are set by passing `key = value` pairs.
#'
#' @param ... Arguments of the form `key = value`, or character strings to
#'   query specific parameter values. If empty, returns all current parameters.
#'
#' @returns When setting, invisibly returns the previous values. When querying,
#'   returns the current value(s).
#'
#' @examples
#' # Query all parameters
#' clipar()
#'
#' # Set palette
#' clipar(palette.qualitative = "nejm")
#'
#' # Query a specific parameter
#' clipar("palette.qualitative")
#'
#' @export
clipar = function(...) {
  dots = list(...)

  if (length(dots) == 0) {
    # Return all parameters
    return(as.list(.clipar))
  }

  # Check if querying (all character strings, no names)
  is_query = all(names(dots) == "" | is.null(names(dots))) &&
             all(vapply(dots, is.character, logical(1)))

  if (is_query) {
    result = lapply(unlist(dots), function(nm) {
      if (exists(nm, envir = .clipar)) {
        .clipar[[nm]]
      } else {
        warning("Unknown parameter: ", nm)
        NULL
      }
    })
    if (length(result) == 1) return(result[[1]])
    names(result) = unlist(dots)
    return(result)
  }

  # Setting mode
  old = list()
  for (nm in names(dots)) {
    if (exists(nm, envir = .clipar)) {
      old[[nm]] = .clipar[[nm]]
    }
    .clipar[[nm]] = dots[[nm]]
  }

  invisible(old)
}

# Internal getter
get_clipar = function(name, default = NULL) {
  if (exists(name, envir = .clipar)) {
    .clipar[[name]]
  } else {
    default
  }
}
