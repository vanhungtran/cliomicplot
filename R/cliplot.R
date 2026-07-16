# ===========================================================================
# cliomicplot: Main Plotting Engine (cliplot)
# Inspired by tinyplot's tinyplot.R
# ===========================================================================

#' Publication-Ready Plots for Clinical and Multi-Omics Data
#'
#' @description
#' Main plotting function for creating publication-ready visualizations.
#' Supports a formula interface with automatic grouping, faceting, and
#' journal-specific themes. Built on ggplot2.
#'
#' @param x,y x and y variables. Can be vectors, data frame columns, or
#'   specified via formula.
#' @param formula A formula of the form \code{y ~ x | group} or \code{~ x | group}
#'   for one-sided plots. The grouping variable after \code{|} is optional.
#' @param data A data.frame containing the variables.
#' @param type Plot type. Can be a character string (\code{"points"},
#'   \code{"boxplot"}, \code{"violin"}, \code{"histogram"}, \code{"density"},
#'   \code{"barplot"}, \code{"lines"}, \code{"volcano"}, \code{"forest"},
#'   \code{"km"}, \code{"waterfall"}, \code{"swimmer"}, \code{"heatmap"},
#'   \code{"pca"}, \code{"ma"}, \code{"correlation"}) or a call to a
#'   \code{type_*()} function for customization.
#' @param by Grouping variable. Alternative to the \code{|} in formula.
#' @param facet Faceting variable(s) as a formula. One-sided formulas
#'   (e.g. \code{~ group}) use \code{facet_wrap}. Two-sided formulas
#'   (e.g. \code{rows ~ cols}) or (e.g. \code{panel ~ logfc + fdr}) use
#'   \code{facet_grid}.
#' @param palette Color palette name or vector of colors. Built-in options:
#'   \code{"jco"}, \code{"nejm"}, \code{"lancet"}, \code{"nature"},
#'   \code{"science"}, \code{"okabe_ito"}.
#' @param theme Theme name or ggplot2 theme object. See \code{\link{clitheme}}.
#' @param title Plot title.
#' @param subtitle Plot subtitle.
#' @param caption Plot caption.
#' @param xlab,ylab Axis labels.
#' @param legend Legend position (\code{"right"}, \code{"left"}, \code{"top"},
#'   \code{"bottom"}, \code{"none"}).
#' @param add Logical. If TRUE, adds to an existing cliplot. Not yet implemented.
#' @param stat.test Statistical test for comparisons. One of \code{"wilcox.test"},
#'   \code{"t.test"}, \code{"kruskal.test"}, \code{"anova"}, \code{NULL}.
#' @param stat.label Format for statistical labels. \code{"p.format"},
#'   \code{"p.signif"}, or \code{"p.adj"}.
#' @param file Optional file path to save the plot (pdf, png, jpg, svg).
#' @param width,height Plot dimensions in inches.
#' @param km_time,km_event,km_group Internal. Column names extracted from a
#'   \code{Surv()} formula for Kaplan-Meier plots; set automatically by the
#'   formula method and not intended to be supplied directly.
#' @param ... Additional arguments passed to the plot type's draw function.
#'
#' @return A ggplot object (invisibly).
#'
#' @examples
#' \dontrun{
#' # Basic scatter plot
#' cliplot(mpg ~ wt, data = mtcars, title = "Mileage vs Weight")
#'
#' # Grouped by variable with automatic legend
#' cliplot(Sepal.Length ~ Petal.Length | Species, data = iris)
#'
#' # Box plot with statistical test
#' cliplot(Sepal.Length ~ Species, data = iris, type = "boxplot",
#'         stat.test = "kruskal.test", palette = "jco")
#'
#' # Volcano plot (y ~ x: significance ~ fold-change)
#' cliplot(-log10(padj) ~ log2FC, data = deg_results, type = "volcano")
#'
#' # Forest plot
#' cliplot(HR ~ Variable | Group, data = cox_results, type = "forest")
#'
#' # Journal theme
#' cliplot(mpg ~ wt, data = mtcars, theme = "nature")
#' }
#'
#' @export
cliplot = function(x, ...) {
  UseMethod("cliplot")
}

#' @rdname cliplot
#' @export
cliplot.default = function(
    x = NULL, y = NULL, ...,
    data      = NULL,
    type      = NULL,
    by        = NULL,
    facet     = NULL,
    palette   = NULL,
    theme     = NULL,
    title     = NULL,
    subtitle  = NULL,
    caption   = NULL,
    xlab      = NULL,
    ylab      = NULL,
    legend    = NULL,
    add       = FALSE,
    stat.test = NULL,
    stat.label = NULL,
    file      = NULL,
    width     = NULL,
    height    = NULL,
    km_time   = NULL,
    km_event  = NULL,
    km_group  = NULL
) {

  dots = list(...)

  # ---- Build settings environment (inspired by tinyplot) ----
  settings = new.env(parent = emptyenv())

  # Resolve data
  if (!is.null(data) && is.matrix(data)) {
    data = as.data.frame(data)
  }

  # Store raw inputs
  settings$x     = x
  settings$y     = y
  settings$data  = data
  settings$by    = by
  settings$facet = facet

  # KM-specific: time/event column names extracted from Surv() formula
  settings$km_time  = km_time
  settings$km_event = km_event
  settings$km_group = km_group

  # Optional range aesthetics used by error bars, ribbons, and custom types
  for (nm in c("xmin", "xmax", "ymin", "ymax", "extra_data")) {
    if (nm %in% names(dots)) {
      settings[[nm]] = dots[[nm]]
    }
  }

  # Theme
  settings$theme = theme %||% get_environment_variable(".active_theme") %||% "cli_bw"

  # Color palette
  settings$palette = palette %||% get_clipar("palette.qualitative")

  # Text elements
  settings$title     = title
  settings$subtitle  = subtitle
  settings$caption   = caption
  settings$xlab      = xlab
  settings$ylab      = ylab

  # Legend
  settings$legend    = legend %||% "right"

  # Statistical
  settings$stat.test  = stat.test %||% get_clipar("stat.test")
  settings$stat.label = stat.label %||% get_clipar("stat.label")

  # File output
  settings$file   = file
  settings$width  = width %||% get_clipar("file.width")
  settings$height = height %||% get_clipar("file.height")

  # Default color
  settings$col.default = get_clipar("col.default")

  # ---- Resolve plot type ----
  if (is.null(type)) {
    type = default_type(x, y)
  }
  settings$type_name = if (inherits(type, "cliplot_type")) type$name else type
  settings$type = resolve_type(type)

  # ---- Let plot types normalize their data-specific settings ----
  if (is.function(settings$type$data)) {
    settings$type$data(settings, ...)
  }

  # ---- Build data frame for ggplot ----
  plot_data = build_plot_data(settings)

  # ---- Build ggplot mapping ----
  mapping = build_mapping(settings, plot_data)

  # ---- Create the plot ----
  p = settings$type$draw(plot_data, mapping, settings, ...)

  if (!isTRUE(settings$skip_postprocess)) {
    # ---- Apply labels ----
    p = p +
      ggplot2::labs(
        title    = settings$title,
        subtitle = settings$subtitle,
        caption  = settings$caption,
        x        = settings$xlab,
        y        = settings$ylab
      )

    # ---- Apply color palette ----
    if (!is.null(settings$by)) {
      p = apply_palette(p, settings)
    }

    # ---- Apply statistical tests ----
    if (!is.null(settings$stat.test) && !is.null(settings$by)) {
      p = add_stat_annotations(p, settings, plot_data)
    }

    # ---- Apply faceting ----
    if (!is.null(settings$facet)) {
      if (length(settings$facet) == 3L) {
        # Two-sided formula (e.g. panel ~ logfc + fdr) → facet_grid
        p <- p + ggplot2::facet_grid(settings$facet)
      } else {
        # One-sided formula (e.g. ~ group) → facet_wrap
        p <- p + ggplot2::facet_wrap(settings$facet)
      }
    }

    # ---- Apply theme ----
    p = p + resolve_theme(settings$theme)

    # ---- Apply legend position ----
    p = p + ggplot2::theme(legend.position = settings$legend)
  }

  # ---- Save to file if requested ----
  if (!is.null(settings$file)) {
    ggplot2::ggsave(
      filename = settings$file,
      plot     = p,
      width    = settings$width,
      height   = settings$height,
      dpi      = get_clipar("file.res")
    )
    message("Plot saved to: ", settings$file)
  }

  # ---- Store last call for cliplot_add ----
  set_environment_variable(.last_call = match.call())

  # ---- Return visibly so knitr and interactive top-level use print once ----
  p
}

#' @rdname cliplot
#' @export
cliplot.formula = function(
    x, data = NULL, ...,
    type      = NULL,
    by        = NULL,
    facet     = NULL,
    palette   = NULL,
    theme     = NULL,
    title     = NULL,
    subtitle  = NULL,
    caption   = NULL,
    xlab      = NULL,
    ylab      = NULL,
    legend    = NULL,
    add       = FALSE,
    stat.test = NULL,
    stat.label = NULL,
    file      = NULL,
    width     = NULL,
    height    = NULL
) {

  formula = x
  parsed = parse_cli_formula(formula, data)

  # Detect Surv() on LHS and extract time/event column names for KM plots.
  # Also: for Surv() formulas, the RHS is the grouping variable, so
  # route it through `by` instead of `x`.
  km_time  = NULL
  km_event = NULL
  km_group = NULL
  is_surv  = FALSE
  if (length(formula) == 3L) {
    lhs = formula[[2L]]
    if (is.call(lhs) && identical(lhs[[1L]], as.name("Surv"))) {
      is_surv  = TRUE
      km_time  = deparse(lhs[[2L]])
      if (length(lhs) > 2L) km_event = deparse(lhs[[3L]])
      # Store the grouping variable name
      rhs = formula[[3L]]
      km_group = if (is.call(rhs) && rhs[[1L]] == as.name("|"))
                   deparse(rhs[[3L]]) else deparse(rhs)
    }
  }

  cliplot.default(
    x        = if (is_surv) NULL else parsed$x,
    y        = if (is_surv) parsed$y else parsed$y,
    data     = data,
    type     = type,
    by       = if (is_surv) parsed$x else (parsed$by %||% by),
    facet    = facet,
    palette  = palette,
    theme    = theme,
    title    = title,
    subtitle = subtitle,
    caption  = caption,
    xlab     = xlab %||% parsed$xlab,
    ylab     = ylab %||% parsed$ylab,
    legend   = legend,
    add      = add,
    stat.test = stat.test,
    stat.label = stat.label,
    file     = file,
    width    = width,
    height   = height,
    km_time  = km_time,
    km_event = km_event,
    km_group = km_group,
    ...
  )
}

#' @rdname cliplot
#' @export
cliplot.data.frame = function(
    x, ...,
    formula   = NULL,
    type      = NULL,
    by        = NULL,
    facet     = NULL,
    palette   = NULL,
    theme     = NULL,
    title     = NULL,
    subtitle  = NULL,
    caption   = NULL,
    xlab      = NULL,
    ylab      = NULL,
    legend    = NULL,
    stat.test = NULL,
    file      = NULL,
    width     = NULL,
    height    = NULL
) {
  # Resolve type name for special handling
  type_name = if (inherits(type, "cliplot_type")) type$name else type

  # PCA and correlation types need the full data matrix, not just two columns
  if (type_name %in% c("pca", "correlation")) {
    cliplot.default(
      x        = NULL,
      y        = NULL,
      data     = x,
      type     = type,
      by       = by,
      facet    = facet,
      palette  = palette,
      theme    = theme,
      title    = title,
      subtitle = subtitle,
      caption  = caption,
      xlab     = xlab,
      ylab     = ylab,
      legend   = legend,
      stat.test = stat.test,
      file     = file,
      width    = width,
      height   = height,
      ...
    )
  } else if (is.null(formula)) {
    numeric_cols = names(x)[vapply(x, is.numeric, logical(1))]
    if (length(numeric_cols) < 2) {
      stop("Need at least 2 numeric columns when no formula is specified")
    }
    # Default: first column as y, second as x
    cliplot.default(
      x        = x[[numeric_cols[2]]],
      y        = x[[numeric_cols[1]]],
      data     = x,
      type     = type %||% "points",
      by       = by,
      facet    = facet,
      palette  = palette,
      theme    = theme,
      title    = title,
      subtitle = subtitle,
      caption  = caption,
      xlab     = xlab %||% numeric_cols[2],
      ylab     = ylab %||% numeric_cols[1],
      legend   = legend,
      stat.test = stat.test,
      file     = file,
      width    = width,
      height   = height,
      ...
    )
  } else {
    cliplot.formula(
      formula   = formula,
      data      = x,
      type      = type,
      by        = by,
      facet     = facet,
      palette   = palette,
      theme     = theme,
      title     = title,
      subtitle  = subtitle,
      caption   = caption,
      xlab      = xlab,
      ylab      = ylab,
      legend    = legend,
      stat.test = stat.test,
      file      = file,
      width     = width,
      height    = height,
      ...
    )
  }
}

#' @rdname cliplot
#' @export
cliplot.matrix = function(
    x, ...,
    type      = NULL,
    theme     = NULL,
    title     = NULL,
    subtitle  = NULL,
    caption   = NULL,
    file      = NULL,
    width     = NULL,
    height    = NULL
) {
  # Matrix dispatch: default to heatmap or correlation
  if (is.null(type)) {
    type = "heatmap"
  }
  cliplot.default(
    x        = NULL,
    y        = NULL,
    data     = as.data.frame(x),
    type     = type,
    theme    = theme,
    title    = title,
    subtitle = subtitle,
    caption  = caption,
    file     = file,
    width    = width,
    height   = height,
    ...
  )
}

# ==========================================================================
# Internal Helpers
# ==========================================================================

#' Resolve a type string or cliplot_type object to a type definition
#' @keywords internal
resolve_type = function(type) {
  if (inherits(type, "cliplot_type")) return(type)

  type_map = list(
    points      = type_points,
    jitter      = type_jitter,
    barplot     = type_barplot,
    lines       = type_lines,
    histogram   = type_histogram,
    density     = type_density,
    errorbar    = type_errorbar,
    ribbon      = type_ribbon,
    boxplot     = type_boxplot,
    violin      = type_violin,
    volcano     = type_volcano,
    forest      = type_forest,
    km          = type_km,
    waterfall   = type_waterfall,
    swimmer     = type_swimmer,
    heatmap     = type_heatmap,
    pca         = type_pca,
    ma          = type_ma,
    correlation = type_correlation,
    text        = type_text,
    bubble      = type_bubble,
    ridge       = type_ridge,
    lm          = type_lm,
    loess       = type_loess,
    spineplot   = type_spineplot,
    rug         = type_rug,
    abline      = type_abline,
    qq          = type_qq,
    raincloud   = type_raincloud,
    dumbbell    = type_dumbbell,
    lollipop    = type_lollipop,
    beeswarm    = type_beeswarm,
    radar       = type_radar,
    alluvial    = type_alluvial,
    waffle      = type_waffle,
    chord       = type_chord,
    treemap     = type_treemap,
    streamgraph = type_streamgraph,
    connected   = type_connected,
    circular_bar = type_circular_bar,
    density2d   = type_density2d,
    parallel    = type_parallel,
    dendrogram  = type_dendrogram,
    trials      = type_trials,
    infobar     = type_infobar,
    hstack      = type_trials     # alias
  )

  if (type %in% names(type_map)) {
    type_map[[type]]()
  } else {
    type_points()  # Fallback
  }
}

#' Build a clean data frame for plotting
#' @keywords internal
build_plot_data = function(settings) {
  df = list()

  if (!is.null(settings$x)) df$x = settings$x
  if (!is.null(settings$y)) df$y = settings$y
  if (!is.null(settings$by)) df[["..by.."]] = as.factor(settings$by)

  # Errorbar/ribbon extras
  if (!is.null(settings$ymin)) df[["..ymin.."]] = settings$ymin
  if (!is.null(settings$ymax)) df[["..ymax.."]] = settings$ymax
  if (!is.null(settings$xmin)) df[["..xmin.."]] = settings$xmin
  if (!is.null(settings$xmax)) df[["..xmax.."]] = settings$xmax

  # Inject facet variables from original data so facet_grid/facet_wrap can
  # reference columns not in the core x/y/by mapping
  if (!is.null(settings$facet) && !is.null(settings$data) &&
      is.data.frame(settings$data)) {
    facet_vars <- all.vars(settings$facet)
    for (fv in facet_vars) {
      if (fv %in% names(settings$data) && !fv %in% names(df)) {
        df[[fv]] <- settings$data[[fv]]
      }
    }
  }

  # Add any extra data from settings
  if (!is.null(settings$extra_data)) {
    for (nm in names(settings$extra_data)) {
      df[[nm]] = settings$extra_data[[nm]]
    }
  }

  as.data.frame(df, stringsAsFactors = FALSE)
}

#' Build ggplot2 aes mapping
#' @keywords internal
build_mapping = function(settings, data) {
  nms = names(data)
  mapping = ggplot2::aes()

  if ("x" %in% nms) mapping = ggplot2::aes(x = .data[["x"]])
  if ("y" %in% nms) mapping = ggplot2::aes(x = !!mapping$x, y = .data[["y"]])

  # For one-variable plots (histogram, density, barplot)
  if (!"y" %in% nms && "x" %in% nms) {
    mapping = ggplot2::aes(x = .data[["x"]])
  }

  mapping
}

#' Apply color palette to plot
#' @keywords internal
apply_palette = function(p, settings) {
  pal_name = settings$palette
  n_groups = length(unique(stats::na.omit(settings$by)))

  if (is.character(pal_name) && length(pal_name) == 1) {
    colors = get_cli_palette(pal_name, n_groups)
  } else if (is.character(pal_name)) {
    colors = pal_name
  } else {
    colors = scales::hue_pal()(n_groups)
  }

  p = p +
    ggplot2::scale_color_manual(values = colors, na.value = "grey70") +
    ggplot2::scale_fill_manual(values = colors, na.value = "grey85")

  p
}

#' Add statistical test annotations
#' @keywords internal
add_stat_annotations = function(p, settings, data) {
  if (is.null(settings$stat.test)) return(p)

  # Skip if x-axis is continuous (ggsignif requires categorical x)
  if (is.numeric(data[["x"]] %||% data[[1]])) {
    warning("`stat.test` was requested but the x-axis is continuous; ",
            "statistical annotations require a categorical x. Skipping.",
            call. = FALSE)
    return(p)
  }

  # Build comparisons: all pairwise combinations of by groups
  by_var = data[["..by.."]]
  if (is.null(by_var)) {
    warning("`stat.test` was requested but no grouping variable is available; ",
            "skipping statistical annotations.", call. = FALSE)
    return(p)
  }

  groups = unique(by_var)
  if (length(groups) < 2) {
    warning("`stat.test` was requested but fewer than two groups are present; ",
            "skipping statistical annotations.", call. = FALSE)
    return(p)
  }

  if (!requireNamespace("ggpubr", quietly = TRUE)) {
    warning("`stat.test` annotations require the 'ggpubr' package; ",
            "skipping. Install it with install.packages('ggpubr').",
            call. = FALSE)
    return(p)
  }

  comparisons = utils::combn(as.character(groups), 2, simplify = FALSE)

  # Warn about multiple comparisons without correction so users are not
  # misled into treating raw pairwise p-values as adjusted.
  if (length(comparisons) > 1 &&
      settings$stat.test %in% c("t.test", "wilcox.test")) {
    warning(sprintf(
      "%d pairwise comparisons are shown without multiple-testing correction. ",
      length(comparisons)),
      "Interpret raw p-values with caution; consider an omnibus test ",
      "(kruskal.test/anova) for >2 groups.", call. = FALSE)
  }

  # Map stat.test to ggpubr method
  method_map = list(
    "t.test"       = "t.test",
    "wilcox.test"  = "wilcox.test",
    "kruskal.test" = "kruskal.test",
    "anova"        = "anova"
  )

  method = method_map[[settings$stat.test]] %||% "wilcox.test"

  # For >2 groups, use global test, else pairwise
  if (length(groups) > 2 && settings$stat.test %in% c("kruskal.test", "anova")) {
    p = p + ggpubr::stat_compare_means(
      method = method,
      label  = settings$stat.label %||% "p.format"
    )
  } else {
    p = p + ggpubr::stat_compare_means(
      comparisons = comparisons,
      method      = method,
      label       = settings$stat.label %||% "p.format"
    )
  }

  p
}
