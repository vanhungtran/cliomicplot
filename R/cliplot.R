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
#'   \code{"pca"}, \code{"ma"}, \code{"correlation"}, \code{"roc"},
#'   \code{"gsea"}, \code{"upset"}, \code{"venn"}, \code{"slope"},
#'   \code{"confusion"}, \code{"donut"}, \code{"mcid"}, \code{"trajectory"}, \code{"manhattan"}, \code{"bubble_matrix"}, \code{"sharing_heatmap"}, \code{"module_compare"}, \code{"cross_network"}, \code{"module_trait"}, \code{"module_env"}, \code{"zipi"}, \code{"mantel_triangle"}, \code{"polygon_network"}, \code{"bipartite"}, \code{"edge_dynamics"}, \code{"mr_coloc"}, \code{"composition"})
#'   or a call to a \code{type_*()} function for customization.
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
    by_name   = NULL,
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
    stat.annotations = NULL,
    stat.y    = NULL,
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
  settings$by_name = by_name %||% (if (is.character(by) && length(by) == 1L) by else NULL)

  # KM-specific: time/event column names extracted from Surv() formula
  settings$km_time  = km_time
  settings$km_event = km_event
  settings$km_group = km_group

  # Optional range aesthetics used by error bars, ribbons, and custom types
  for (nm in c("xmin", "xmax", "ymin", "ymax", "extra_data", "group")) {
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

  # Legend (do not force default yet so plot types can set specialized defaults)
  settings$legend    = legend

  # Statistical
  settings$stat.test        = stat.test %||% get_clipar("stat.test")
  settings$stat.label       = stat.label %||% get_clipar("stat.label")
  settings$stat.annotations = stat.annotations %||% dots$stat.annotations
  settings$stat.y           = stat.y %||% dots$stat.y

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

  # For categorical distribution plots (boxplot, violin, beeswarm, raincloud, pathology, ecology),
  # convert numeric x to factor so discrete levels (e.g. dose 0.5, 1.0, 2.0)
  # dodge cleanly without continuous-scale interval overlaps
  if (settings$type_name %in% c("boxplot", "violin", "split_violin", "half_violin", "beeswarm", "raincloud", "pathology", "pathology_boxplot", "ecology", "half_boxed", "ecology_raincloud") &&
      !is.null(x) && is.numeric(x)) {
    x = as.factor(x)
    settings$x = x
  }

  # Resolve by from data if passed as a column name string
  if (is.character(by) && length(by) == 1L && !is.null(data) && is.data.frame(data) && by %in% names(data)) {
    by = data[[by]]
  }

  # If by is not supplied, but x is categorical and a discrete palette or grouping type
  # is requested, default by to x so boxes/points color by categories and stat.test works
  if (is.null(by) && !is.null(x) && (is.character(x) || is.factor(x)) &&
      (settings$type_name %in% c("boxplot", "violin", "split_violin", "half_violin", "beeswarm", "raincloud", "pathology", "pathology_boxplot", "ecology", "half_boxed", "ecology_raincloud") || !is.null(palette))) {
    by = x
    if (is.null(legend)) {
      settings$legend = "none"
    }
  }
  settings$by = by

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
    if (!inherits(p, "wrapped_patch") && !inherits(p, "patchwork")) {
      guide_title = settings$by_name %||% "Group"
      p = p +
        ggplot2::labs(
          title    = settings$title,
          subtitle = settings$subtitle,
          caption  = settings$caption,
          x        = settings$xlab,
          y        = settings$ylab,
          fill     = guide_title
        )
      has_col <- any(vapply(p$layers, function(l) any(c("colour", "color") %in% names(l$mapping)), logical(1)))
      if (has_col) {
        p = p + ggplot2::labs(color = guide_title)
      }
    }

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
    leg_pos = settings$legend %||% "right"
    p = p + ggplot2::theme(legend.position = leg_pos)

    # ---- Maintain angled x-axis text for matrix, heatmap, and waterfall types ----
    if (settings$type_name %in% c("waterfall", "module_compare", "cross_network", "module_trait", "module_env", "module_cor", "correlation", "sharing_heatmap", "bubble_matrix", "bubble_heatmap")) {
      sz_x = settings$axis_text_size %||% settings$cor_axis_size %||% settings$bh_axis_x_size
      sz_y = settings$axis_text_size %||% settings$cor_axis_size %||% settings$bh_axis_y_size
      ang = settings$axis_text_angle %||% settings$cor_axis_angle %||% settings$bh_axis_angle %||% 45
      show_ax = if (!is.null(settings$cor_show_axis)) settings$cor_show_axis else TRUE

      if (!isTRUE(show_ax) || isTRUE(settings$axis_text_blank)) {
        p = p + ggplot2::theme(
          axis.text.x = ggplot2::element_blank(),
          axis.ticks.x = if (isTRUE(settings$axis_text_blank)) ggplot2::element_blank() else ggplot2::element_line(),
          axis.text.y = if (isTRUE(settings$axis_text_blank)) ggplot2::element_text() else ggplot2::element_blank()
        )
      } else if (!is.null(sz_x) || !is.null(sz_y)) {
        p = p + ggplot2::theme(
          axis.text.x = ggplot2::element_text(angle = ang, hjust = 1, vjust = 1, size = sz_x),
          axis.text.y = ggplot2::element_text(size = sz_y)
        )
      } else {
        p = p + ggplot2::theme(axis.text.x = ggplot2::element_text(angle = ang, hjust = 1, vjust = 1))
      }
    }

    # ---- Maintain clean matrix borders and backgrounds for correlation matrices and bubble heatmaps ----
    if (settings$type_name %in% c("correlation", "bubble_heatmap")) {
      p = p + ggplot2::theme(
        panel.grid   = ggplot2::element_blank(),
        panel.border = ggplot2::element_blank()
      )
    }
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
    stat.annotations = NULL,
    stat.y    = NULL,
    file      = NULL,
    width     = NULL,
    height    = NULL
) {

  formula = x
  form_env = environment(formula)
  if (is.null(form_env)) form_env = parent.frame()
  parsed = parse_cli_formula(formula, data, env = form_env)

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

  is_forest = isTRUE(identical(type, "forest") || (is.list(type) && identical(type$name, "forest")))

  cliplot.default(
    x        = if (is_surv) NULL else parsed$x,
    y        = if (is_surv) parsed$y else parsed$y,
    data     = data,
    type     = type,
    by       = if (is_surv) parsed$x else (parsed$by %||% by),
    by_name  = parsed$by_name,
    facet    = facet,
    palette  = palette,
    theme    = theme,
    title    = title,
    subtitle = subtitle,
    caption  = caption,
    xlab     = xlab %||% (if (is_forest) "Hazard Ratio (95% CI)" else parsed$xlab),
    ylab     = ylab %||% (if (is_forest) "" else parsed$ylab),
    legend   = legend,
    add      = add,
    stat.test = stat.test,
    stat.label = stat.label,
    stat.annotations = stat.annotations,
    stat.y   = stat.y,
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

  # PCA, correlation, and module types need the full data matrix, not just two columns
  if (type_name %in% c("pca", "correlation", "roc", "gsea", "upset",
                           "venn", "slope", "confusion", "donut",
                           "module_trait", "module_env", "module_cor",
                           "module_compare", "cross_network",
                           "cross_network_graph", "cross_net",
                           "env_network", "microbial_network",
                           "module_preservation", "network_robustness")) {
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
    pathology   = type_pathology_boxplot,
    pathology_boxplot = type_pathology_boxplot,
    violin      = type_violin,
    split_violin = type_split_violin,
    split_vln   = type_split_violin,
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
    ecology     = type_ecology_raincloud,
    half_boxed  = type_ecology_raincloud,
    ecology_raincloud = type_ecology_raincloud,
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
    hstack      = type_trials,     # alias
    bubble_heatmap = type_bubble_heatmap,
    roc         = type_roc,
    gsea        = type_gsea,
    upset       = type_upset,
    venn        = type_venn,
    slope       = type_slope,
    confusion   = type_confusion,
    donut       = type_donut,
    mcid        = type_mcid,
    trajectory  = type_trajectory,
    longitudinal = type_trajectory,
    manhattan   = type_manhattan,
    mgwas       = type_manhattan,
    bubble_matrix = type_bubble_matrix,
    sharing_heatmap = type_sharing_heatmap,
    module_compare = type_module_compare,
    cross_network = type_module_compare,
    module_overlap = type_module_compare,
    cross_network_graph = type_cross_network_graph,
    cross_net   = type_cross_network_graph,
    module_trait = type_module_trait,
    module_env = type_module_trait,
    module_cor = type_module_trait,
    env_network = type_env_network,
    microbial_network = type_env_network,
    module_preservation = type_module_preservation,
    network_robustness = type_network_robustness,
    zipi            = type_zipi,
    zipi_plot       = type_zipi,
    keystone        = type_zipi,
    mantel_triangle = type_mantel_triangle,
    mantel_link     = type_mantel_triangle,
    matcor          = type_mantel_triangle,
    polygon_network = type_polygon_network,
    polygon_cluster = type_polygon_network,
    bipartite       = type_bipartite,
    bipartite_network = type_bipartite,
    transkingdom    = type_bipartite,
    edge_dynamics   = type_edge_dynamics,
    edge_sign       = type_edge_dynamics,
    mr_coloc        = type_mr_coloc,
    mr_heatmap      = type_mr_coloc,
    coloc           = type_mr_coloc,
    pqtl_mr         = type_mr_coloc,
    composition     = type_composition,
    cell_composition = type_composition,
    lineage_bars    = type_composition,
    attribution     = type_attribution,
    shapley         = type_shapley,
    stepwise        = type_stepwise,
    benchmark_bars  = type_benchmark_bars,
    benchmark       = type_benchmark_bars,
    identifiability = type_identifiability
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

  if (!is.null(settings$x) && !is.list(settings$x)) df$x = settings$x
  if (!is.null(settings$y) && !is.list(settings$y)) df$y = settings$y
  if (!is.null(settings$by)) df[["..by.."]] = as.factor(settings$by)

  # Errorbar/ribbon extras
  if (!is.null(settings$ymin)) df[["..ymin.."]] = settings$ymin
  if (!is.null(settings$ymax)) df[["..ymax.."]] = settings$ymax
  if (!is.null(settings$xmin)) df[["..xmin.."]] = settings$xmin
  if (!is.null(settings$xmax)) df[["..xmax.."]] = settings$xmax

  # Group aesthetic (e.g. for individual lines in longitudinal / time series)
  if (!is.null(settings$group)) {
    if (is.character(settings$group) && length(settings$group) == 1L &&
        !is.null(settings$data) && is.data.frame(settings$data) &&
        settings$group %in% names(settings$data)) {
      df[["..group.."]] = as.factor(settings$data[[settings$group]])
    } else {
      df[["..group.."]] = as.factor(settings$group)
    }
  } else if (!is.null(settings$data) && is.data.frame(settings$data)) {
    for (id_col in c("Individual", "Patient", "Subject", "mouse_id", "PatientID")) {
      if (id_col %in% names(settings$data) && !id_col %in% names(df)) {
        df[["..group.."]] = as.factor(settings$data[[id_col]])
        break
      }
    }
  }

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
  if (!"package:ggpubr" %in% search()) {
    suppressPackageStartupMessages(require("ggpubr", quietly = TRUE, character.only = TRUE))
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

  # Ensure ggpubr helper create_p_label is available in plot environment
  if (requireNamespace("ggpubr", quietly = TRUE) &&
      exists("create_p_label", envir = asNamespace("ggpubr")) &&
      !is.null(p$plot_env) &&
      !exists("create_p_label", envir = p$plot_env, inherits = FALSE)) {
    assign("create_p_label", get("create_p_label", envir = asNamespace("ggpubr")), envir = p$plot_env)
  }

  is_grouped <- !is.null(data[["x"]]) && !identical(as.character(data[["x"]]), as.character(by_var))

  if (is_grouped) {
    if (length(groups) == 2 && requireNamespace("ggsignif", quietly = TRUE)) {
      x_factor  <- factor(data[["x"]])
      x_levels  <- levels(x_factor)
      by_factor <- factor(data[["..by.."]])
      y_values  <- data[["y"]]

      p_vals  <- numeric(length(x_levels))
      y_maxes <- numeric(length(x_levels))

      for (i in seq_along(x_levels)) {
        lvl    <- x_levels[i]
        sub_y  <- y_values[x_factor == lvl]
        sub_by <- by_factor[x_factor == lvl]

        pval <- tryCatch({
          if (method == "t.test") {
            stats::t.test(sub_y ~ sub_by)$p.value
          } else {
            stats::wilcox.test(sub_y ~ sub_by)$p.value
          }
        }, error = function(e) NA_real_)

        p_vals[i]  <- pval
        y_maxes[i] <- if (length(sub_y) > 0) max(sub_y, na.rm = TRUE) else 0
      }

      fmt <- settings$stat.label %||% "p.format"
      labels <- if (!is.null(settings$stat.annotations)) {
        if (length(settings$stat.annotations) == length(x_levels)) {
          settings$stat.annotations
        } else {
          rep(settings$stat.annotations, length.out = length(x_levels))
        }
      } else {
        vapply(p_vals, function(p) {
          if (is.na(p)) return("ns")
          if (fmt %in% c("p.nature", "pathology", "p.math", "nature")) {
            stars <- if (p < 0.001) "***" else if (p < 0.01) "**" else if (p < 0.05) "*" else "n.s."
            p_str <- if (p < 0.0001) {
              exp_val   <- floor(log10(p))
              base_val  <- p / (10^exp_val)
              sups      <- c("0"="⁰", "1"="¹", "2"="²", "3"="³", "4"="⁴", "5"="⁵", "6"="⁶", "7"="⁷", "8"="⁸", "9"="⁹", "-"="⁻")
              exp_chars <- strsplit(as.character(exp_val), "")[[1]]
              exp_super <- paste0(vapply(exp_chars, function(ch) sups[[ch]] %||% ch, character(1)), collapse = "")
              sprintf("%.1f \u00d7 10%s", base_val, exp_super)
            } else if (p < 0.01) {
              sprintf("%.3f", p)
            } else {
              sprintf("%.2f", p)
            }
            if (stars == "n.s.") {
              sprintf("n.s.\n(P = %s)", p_str)
            } else {
              sprintf("%s\n(P = %s)", stars, p_str)
            }
          } else if (fmt == "p.signif") {
            if (p < 0.001) "***" else if (p < 0.01) "**" else if (p < 0.05) "*" else "ns"
          } else {
            if (p < 0.001) "p < 0.001" else if (p < 0.01) sprintf("p = %.3f", p) else sprintf("p = %.2f", p)
          }
        }, character(1))
      }

      y_positions <- if (!is.null(settings$stat.y)) {
        if (length(settings$stat.y) == length(x_levels)) settings$stat.y else rep(settings$stat.y, length.out = length(x_levels))
      } else {
        y_span <- diff(range(y_values, na.rm = TRUE))
        y_maxes + y_span * 0.08
      }

      bracket_offset <- if (identical(settings$type_name, "pathology_boxplot") || identical(settings$type_name, "pathology")) 0.105 else 0.18

      p = p + ggsignif::geom_signif(
        y_position = y_positions,
        xmin       = seq_along(x_levels) - bracket_offset,
        xmax       = seq_along(x_levels) + bracket_offset,
        annotation = labels,
        tip_length = 0.02,
        textsize   = settings$stat_text_size %||% 2.3,
        vjust      = -0.2,
        color      = "#20242A",
        size       = 0.35
      )
      if (is.null(settings$stat.y)) {
        p = p + ggplot2::scale_y_continuous(expand = ggplot2::expansion(mult = c(0.06, 0.18)))
      }
    } else {
      # Fallback for >2 groups or without ggsignif
      lbl <- if (identical(settings$stat.label, "p.nature") || identical(settings$stat.label, "pathology")) "p.signif" else (settings$stat.label %||% "p.format")
      p = p + ggpubr::stat_compare_means(
        ggplot2::aes(group = .data[["..by.."]]),
        method = method,
        label  = lbl
      )
    }
  } else if (!is.null(settings$stat.comparisons)) {
    lbl <- if (identical(settings$stat.label, "p.nature") || identical(settings$stat.label, "pathology")) "p.signif" else (settings$stat.label %||% "p.format")
    p = p + ggpubr::stat_compare_means(
      comparisons = settings$stat.comparisons,
      method      = method,
      label       = lbl
    )
  } else if (length(groups) > 2 && settings$stat.test %in% c("kruskal.test", "anova")) {
    if (is.null(settings$stat.label)) {
      p = p + ggpubr::stat_compare_means(method = method)
    } else {
      lbl <- if (identical(settings$stat.label, "p.nature") || identical(settings$stat.label, "pathology")) "p.signif" else settings$stat.label
      p = p + ggpubr::stat_compare_means(
        method = method,
        label  = lbl
      )
    }
  } else {
    lbl <- if (identical(settings$stat.label, "p.nature") || identical(settings$stat.label, "pathology")) "p.signif" else (settings$stat.label %||% "p.format")
    p = p + ggpubr::stat_compare_means(
      comparisons = comparisons,
      method      = method,
      label       = lbl
    )
  }

  p
}
