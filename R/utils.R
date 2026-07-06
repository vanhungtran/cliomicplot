# ===========================================================================
# cliomicplot: Utility Functions
# ===========================================================================

# NULL coalescing operator (inspired by tinyplot)
`%||%` = function(x, y) {
  if (is.null(x)) y else x
}

# Assertion helpers --------------------------------------------------------

assert_logical = function(x, name = deparse(substitute(x))) {
  if (!is.null(x) && !is.logical(x)) {
    stop(sprintf("`%s` must be logical, not %s", name, class(x)[1]), call. = FALSE)
  }
}

assert_numeric = function(x, name = deparse(substitute(x))) {
  if (!is.null(x) && !is.numeric(x)) {
    stop(sprintf("`%s` must be numeric, not %s", name, class(x)[1]), call. = FALSE)
  }
}

assert_character = function(x, name = deparse(substitute(x))) {
  if (!is.null(x) && !is.character(x)) {
    stop(sprintf("`%s` must be character, not %s", name, class(x)[1]), call. = FALSE)
  }
}

assert_factor = function(x, name = deparse(substitute(x))) {
  if (!is.null(x) && !is.factor(x)) {
    stop(sprintf("`%s` must be a factor, not %s", name, class(x)[1]), call. = FALSE)
  }
}

# Environment-to-environment copy helper -----------------------------------
# Copies named variables between environments (inspired by tinyplot's env2env)
env2env = function(from, to, keys) {
  for (k in keys) {
    if (exists(k, envir = from, inherits = FALSE)) {
      assign(k, get(k, envir = from, inherits = FALSE), envir = to)
    }
  }
}

# Formula parsing -----------------------------------------------------------
# Parse formula into x, y, and by components
# Supports: y ~ x | group,  ~ x | group, y ~ x, ~ x
parse_cli_formula = function(formula, data, env = parent.frame()) {
  # Helper: evaluate a formula component, preferring data-frame lookups
  eval_part <- function(sym, data, env) {
    nm <- deparse(sym)
    if (!is.null(data) && is.data.frame(data) && nm %in% names(data)) {
      data[[nm]]
    } else {
      eval(sym, data, env)
    }
  }
  if (length(formula) == 2L) {
    # One-sided formula: ~ x | group  or  ~ x
    x = eval_part(formula[[2]], data, env)
    y = NULL
    by = NULL
  } else if (length(formula) == 3L) {
    # Two-sided formula: y ~ x | group
    rhs = formula[[3L]]
    if (is.call(rhs) && rhs[[1L]] == as.name("|")) {
      # Has grouping: y ~ x | group
      x  = eval_part(rhs[[2L]], data, env)
      by = eval_part(rhs[[3L]], data, env)
      y  = eval_part(formula[[2L]], data, env)
    } else {
      x  = eval_part(rhs, data, env)
      y  = eval_part(formula[[2L]], data, env)
      by = NULL
    }
  }

  list(x = x, y = y, by = by,
       xlab = deparse(formula[[if (length(formula) == 2L) 2L else 3L]]),
       ylab = if (length(formula) == 3L) deparse(formula[[2L]]) else NULL)
}

# Color palette helpers -----------------------------------------------------
#
# Palette sources / attributions:
#   Journal palettes (jco, nejm, lancet, etc.) are based on official journal
#     style guides and are widely reproduced across the R ecosystem.
#   Genomics palettes (ucscgb, igv, locuszoom, cosmic, gsea) are derived
#     from the respective tool/documentation colour specifications.
#   D3, Material Design, Bootstrap 5, Flat UI are from published design specs.
#   Sci-fi palettes are fan-created colour extractions from media properties.
#   Colour hex values are factual data, not original creative expression.
#   See REFERENCES.md for a complete list of sources.

# Complete palette registry
.palette_registry = list(
  # --- Journal / Clinical palettes ---
  jco = c("#0073C2", "#EFC000", "#868686", "#CD534C", "#7AA6DC",
          "#003C67", "#8F7700", "#3B3B3B", "#A73030", "#4A6990"),
  nejm = c("#BC3C29", "#0072B5", "#E18727", "#20854E", "#7876B1",
           "#6F99AD", "#FFDC91", "#EE4C97"),
  lancet = c("#00468B", "#ED0000", "#42B540", "#0099B4", "#925E9F",
             "#FDAF91", "#AD002A", "#ADB6B6", "#1B1919"),
  nature = c("#E64B35", "#4DBBD5", "#00A087", "#3C5488", "#F39B7F",
             "#8491B4", "#91D1C2", "#DC0000", "#7E6148"),
  science = c("#3B4992", "#EE0000", "#008B45", "#631879", "#008280",
              "#BB0021", "#5F559B", "#A20056", "#808180"),

  # --- Accessibility-focused palettes ---
  okabe_ito = c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442",
                "#0072B2", "#D55E00", "#CC79A7"),
  tableau10 = c("#4E79A7", "#F28E2B", "#E15759", "#76B7B2", "#59A14F",
                "#EDC948", "#B07AA1", "#FF9DA7", "#9C755F", "#BAB0AC"),
  tol_muted = c("#332288", "#88CCEE", "#44AA99", "#117733", "#999933",
                "#DDCC77", "#CC6677", "#882255", "#AA4499", "#DDDDDD"),

  # --- Omics-specific palettes ---
  # Brewer-inspired for heatmaps
  heatmap_rdbu   = c("#67001F", "#B2182B", "#D6604D", "#F4A582", "#FDDBC7",
                     "#F7F7F7", "#D1E5F0", "#92C5DE", "#4393C3", "#2166AC", "#053061"),
  heatmap_rdylbu = c("#A50026", "#D73027", "#F46D43", "#FDAE61", "#FEE090",
                     "#FFFFBF", "#E0F3F8", "#ABD9E9", "#74ADD1", "#4575B4", "#313695"),
  heatmap_prgn   = c("#40004B", "#762A83", "#9970AB", "#C2A5CF", "#E7D4E8",
                     "#F7F7F7", "#D9F0D3", "#A6DBA0", "#5AAE61", "#1B7837", "#00441B"),

  # Volcano plot palette
  volcano = c("#4575B4", "#74ADD1", "#ABD9E9", "#E0F3F8", "#FFFFBF",
              "#FEE090", "#FDAE61", "#F46D43", "#D73027", "#A50026"),

  # --- Pastel / Soft palettes (good for presentations) ---
  pastel = c("#66C2A5", "#FC8D62", "#8DA0CB", "#E78AC3", "#A6D854",
             "#FFD92F", "#E5C494", "#B3B3B3"),
  soft = c("#7FC97F", "#BEAED4", "#FDC086", "#FFFF99", "#386CB0",
           "#F0027F", "#BF5B17", "#666666"),

  # --- Dark-background palettes ---
  neon = c("#00FF41", "#FF6EC7", "#00BFFF", "#FFD700", "#FF4500",
           "#7FFF00", "#FF1493", "#00CED1"),
  cyberpunk = c("#FF003C", "#FFD100", "#00E5FF", "#B900FF", "#00FF41",
                "#FF6B00", "#0099FF", "#FF00A0"),

  # --- Sequential palettes for continuous data ---
  blues  = c("#F7FBFF", "#DEEBF7", "#C6DBEF", "#9ECAE1", "#6BAED6",
             "#4292C6", "#2171B5", "#08519C", "#08306B"),
  reds   = c("#FFF5F0", "#FEE0D2", "#FCBBA1", "#FC9272", "#FB6A4A",
             "#EF3B2C", "#CB181D", "#A50F15", "#67000D"),
  greens = c("#F7FCF5", "#E5F5E0", "#C7E9C0", "#A1D99B", "#74C476",
             "#41AB5D", "#238B45", "#006D2C", "#00441B"),
  purples = c("#FCFBFD", "#EFEDF5", "#DADAEB", "#BCBDDC", "#9E9AC8",
              "#807DBA", "#6A51A3", "#54278F", "#3F007D"),
  oranges = c("#FFF5EB", "#FEE6CE", "#FDD0A2", "#FDAE6B", "#FD8D3C",
              "#F16913", "#D94801", "#A63603", "#7F2704"),

  # --- Diverging palettes ---
  rd_yl_gn = c("#D73027", "#F46D43", "#FDAE61", "#FEE08B", "#FFFFBF",
               "#D9EF8B", "#A6D96A", "#66BD63", "#1A9850", "#006837"),
  spectral = c("#D53E4F", "#F46D43", "#FDAE61", "#FEE08B", "#FFFFBF",
               "#E6F598", "#ABDDA4", "#66C2A5", "#3288BD", "#5E4FA2"),
  pi_yg    = c("#8E0152", "#C51B7D", "#DE77AE", "#F1B6DA", "#FDE0EF",
               "#F7F7F7", "#E6F5D0", "#B8E186", "#7FBC41", "#4D9221", "#276419"),

  # --- ggsci-inspired: additional journal palettes ---
  # NPG (Nature Reviews Cancer) — official ggsci values
  npg = c("#E64B35", "#4DBBD5", "#00A087", "#3C5488", "#F39B7F",
          "#8491B4", "#91D1C2", "#DC0000", "#7E6148", "#B09C85"),
  # JAMA (Journal of the American Medical Association)
  jama = c("#374E55", "#DF8F44", "#00A1D5", "#B24745",
           "#79AF97", "#6A6599", "#80796B"),
  # BMJ (British Medical Journal)
  bmj = c("#2A6EBB", "#F0AB00", "#C50084", "#7D5CC6",
          "#E37222", "#69BE28", "#00B2A9", "#CD202C", "#747678"),

  # --- ggsci-inspired: genomics/bioinformatics palettes ---
  # UCSC Genome Browser chromosome colors
  ucscgb = c("#FF0000", "#FF9900", "#FFCC00", "#00FF00", "#6699FF",
             "#CC33FF", "#99991E", "#999999", "#FF00CC", "#CC0000",
             "#FFCCCC", "#FFFF00", "#CCFF00", "#358000", "#0000CC",
             "#99CCFF", "#00FFFF", "#CCFFFF", "#9900CC", "#CC99FF",
             "#996600", "#666600", "#666666", "#CCCCCC", "#79CC3D", "#CCCC99"),
  # IGV (Integrative Genomics Viewer)
  igv = c("#5050FF", "#CE3D32", "#749B58", "#F0E685", "#466983",
          "#BA6338", "#5DB1DD", "#802268", "#6BD76B", "#D595A7",
          "#924822", "#837B8D", "#C75127", "#D58F5C", "#7A65A5",
          "#E4AF69", "#3B1B53", "#CDDEB7", "#612A79", "#AE1F63",
          "#E7C76F", "#5A655E", "#CC9900", "#99CC00"),
  # LocusZoom (GWAS visualization)
  locuszoom = c("#D43F3A", "#EEA236", "#5CB85C", "#46B8DA",
                "#357EBD", "#9632B8", "#B8B8B8"),

  # --- ggsci-inspired: cancer/omics-specific palettes ---
  # COSMIC Hallmarks of Cancer (light)
  cosmic = c("#2E2A2B", "#CF4E9C", "#8C57A2", "#358DB9", "#82581F",
             "#2F509E", "#E5614C", "#97A1A7", "#3DA873", "#DC9445"),
  # COSMIC signature substitutions
  cosmic_sig = c("#5ABCEB", "#050708", "#D33C32", "#CBCACB",
                 "#A1CD64", "#EDC8C5"),
  # GSEA GenePattern
  gsea = c("#4500AC", "#2D00CA", "#1D00E2", "#1B5CF2", "#20B1F4",
           "#00D0CB", "#00C58B", "#17BB4B", "#94BC21", "#D2C12E",
           "#E7A627", "#F2891B", "#F45A15", "#DC1913"),

  # --- ggsci-inspired: Frontiers journal ---
  frontiers = c("#D51317", "#F39200", "#EFD500", "#95C11F",
                "#007B3D", "#31B7BC", "#0076BD", "#882D88",
                "#D8307D"),

  # --- ggsci-inspired: D3.js palettes ---
  d3_category10 = c("#1F77B4", "#FF7F0E", "#2CA02C", "#D62728",
                    "#9467BD", "#8C564B", "#E377C2", "#7F7F7F",
                    "#BCBD22", "#17BECF"),
  d3_category20 = c("#1F77B4", "#AEC7E8", "#FF7F0E", "#FFBB78",
                    "#2CA02C", "#98DF8A", "#D62728", "#FF9896",
                    "#9467BD", "#C5B0D5", "#8C564B", "#C49C94",
                    "#E377C2", "#F7B6D2", "#7F7F7F", "#C7C7C7",
                    "#BCBD22", "#DBDB8D", "#17BECF", "#9EDAE5"),

  # --- ggsci-inspired: UChicago ---
  uchicago = c("#800000", "#767676", "#FFA319", "#8A9045",
               "#155F83", "#C16622", "#8F3931", "#58593F", "#350E20"),

  # --- ggsci-inspired: Flat UI ---
  flatui = c("#2C3E50", "#E74C3C", "#ECF0F1", "#3498DB",
             "#2980B9", "#1ABC9C", "#16A085", "#27AE60",
             "#2ECC71", "#F1C40F", "#F39C12", "#E67E22",
             "#D35400", "#9B59B6", "#8E44AD", "#BDC3C7",
             "#95A5A6", "#7F8C8D"),

  # --- ggsci-inspired: Material Design ---
  material = c("#F44336", "#E91E63", "#9C27B0", "#673AB7",
               "#3F51B5", "#2196F3", "#03A9F4", "#00BCD4",
               "#009688", "#4CAF50", "#8BC34A", "#CDDC39",
               "#FFEB3B", "#FFC107", "#FF9800", "#FF5722"),

  # --- ggsci-inspired: Bootstrap 5 ---
  bs5 = c("#0d6efd", "#6610f2", "#6f42c1", "#d63384",
          "#dc3545", "#fd7e14", "#ffc107", "#198754",
          "#20c997", "#0dcaf0"),

  # --- ggsci-inspired: Sci-fi pop culture (from mytheme tvthemes connection) ---
  futurama = c("#FF6F00", "#C71000", "#008EA0", "#8A4198",
               "#5A9599", "#FF6348", "#84D7E1", "#FF95A8",
               "#3D3B25", "#ADE2D0", "#1A5354", "#3F4041"),
  rickandmorty = c("#FAFD7C", "#82491E", "#24325F", "#B7E4F9",
                   "#FB6467", "#526E2D", "#E762D7", "#E89242",
                   "#FAE48B", "#A6EEE6", "#917C5D", "#69C8EC"),
  simpsons = c("#FED439", "#709AE1", "#8A9197", "#D2AF81",
               "#FD7446", "#D5E4A2", "#197EC0", "#F05C3B",
               "#46732E", "#71D0F5", "#370335", "#075149",
               "#C80813", "#91331F", "#1A9993", "#FD8CC1"),
  startrek = c("#CC0C00", "#5C88DA", "#84BD00", "#FFCD00",
               "#7C878E", "#00B5E2", "#00AF66")
)

# Color palette helper: get colors by name and count
get_cli_palette = function(name = "jco", n = 7) {
  if (name %in% names(.palette_registry)) {
    pal = .palette_registry[[name]]
    if (n <= length(pal)) {
      pal[1:n]
    } else {
      rep(pal, length.out = n)
    }
  } else {
    scales::hue_pal()(n)
  }
}

#' List Available Cliomicplot Palettes
#'
#' @description Returns the names of all built-in cliomicplot color palettes.
#'
#' @return Character vector of palette names.
#' @export
cli_palette_list = function() {
  names(.palette_registry)
}

#' Show a Cliomicplot Palette
#'
#' @description Displays a named palette as labeled color swatches.
#'
#' @param name Character. Palette name from \code{\link{cli_palette_list}}.
#'
#' @return Invisibly returns the plotted palette from \code{scales::show_col()}.
#' @export
cli_palette_show = function(name = "jco") {
  if (!name %in% names(.palette_registry)) {
    stop("Unknown palette: ", name, "\nAvailable: ",
         paste(names(.palette_registry), collapse = ", "))
  }
  pal = .palette_registry[[name]]
  scales::show_col(pal, labels = TRUE, borders = NA)
}

# Theme element helpers -----------------------------------------------------

#' Parse theme overrides from ... arguments
#' @keywords internal
parse_theme_overrides = function(...) {
  dots = list(...)
  if (length(dots) == 0) return(list())
  dots
}

# Safe data frame extraction ------------------------------------------------

#' Extract columns safely from a data frame
#' @keywords internal
extract_data = function(data, x_var, y_var = NULL, by_var = NULL) {
  result = list()
  if (is.character(x_var) && x_var %in% names(data)) {
    result$x = data[[x_var]]
  } else {
    result$x = x_var
  }
  if (!is.null(y_var) && is.character(y_var) && y_var %in% names(data)) {
    result$y = data[[y_var]]
  } else if (!is.null(y_var)) {
    result$y = y_var
  }
  if (!is.null(by_var) && is.character(by_var) && by_var %in% names(data)) {
    result$by = data[[by_var]]
  } else if (!is.null(by_var)) {
    result$by = by_var
  }
  result
}

# Smooth color gradient -----------------------------------------------------

#' Generate a smooth color gradient
#' @keywords internal
gradient_pal = function(n, low = "#132B43", high = "#56B1F7") {
  scales::seq_gradient_pal(low = low, high = high)(seq(0, 1, length.out = n))
}
