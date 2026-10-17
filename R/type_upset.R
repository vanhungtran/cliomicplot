# ===========================================================================
# cliomicplot: UpSet Plot Type
# Inspired by cnsplots upsetplot() - set intersection visualization
# ===========================================================================

# Internal helpers -----------------------------------------------------------

# Resolve sets from a named list, binary data frame, or list-like x
.resolve_sets = function(sets, x, df) {
  if (!is.null(sets)) {
    s = sets
  } else if (is.list(x) && !is.data.frame(x)) {
    s = x
  } else if (!is.null(df) && is.data.frame(df)) {
    # Binary membership columns: rows = items, columns = sets
    num_cols = names(df)[vapply(df, function(col) {
      is.numeric(col) && all(unique(col) %in% c(0, 1, NA))
    }, logical(1))]
    if (length(num_cols) == 0) {
      stop("UpSet input must be a named list of sets or a data frame of ",
           "binary membership columns.", call. = FALSE)
    }
    s = lapply(num_cols, function(nm) rownames(df)[as.logical(df[[nm]])])
    names(s) = num_cols
  } else {
    stop("UpSet input must be a named list of sets or a data frame of ",
         "binary membership columns.", call. = FALSE)
  }
  s = s[vapply(s, function(v) length(v) > 0, logical(1))]
  s = lapply(s, function(v) unique(as.character(v)))
  if (length(s) < 2) {
    stop("UpSet plots need at least 2 non-empty sets.", call. = FALSE)
  }
  s
}

# Compute observed intersections from a named list of sets
.compute_intersections = function(sets, min_size = 0, max_rank = NULL,
                                  sort_by = c("size", "degree")) {
  sort_by = match.arg(sort_by)
  k = length(sets)
  nms = names(sets)
  elements = unique(unlist(sets, use.names = FALSE))
  mem = vapply(elements, function(el) {
    vapply(sets, function(s) el %in% s, logical(1))
  }, logical(k))
  # Pattern string per element: 1 = in set
  patterns = apply(mem, 2, function(col) paste(as.integer(col), collapse = ""))
  tab = table(patterns)
  inter = data.frame(
    pattern = names(tab),
    size    = as.integer(tab),
    stringsAsFactors = FALSE
  )
  # Membership columns
  for (j in seq_len(k)) {
    inter[[nms[j]]] = as.integer(substr(inter$pattern, j, j))
  }
  inter$degree = rowSums(inter[nms])

  if (sort_by == "degree") {
    inter = inter[order(-inter$degree, -inter$size), ]
  } else {
    inter = inter[order(-inter$size, -inter$degree), ]
  }
  inter = inter[inter$size >= min_size, ]
  if (!is.null(max_rank) && nrow(inter) > max_rank) {
    inter = inter[seq_len(max_rank), ]
  }
  inter$id = seq_len(nrow(inter))
  rownames(inter) = NULL
  inter
}

#' UpSet Plot Type
#'
#' @description Creates publication-ready UpSet plots for visualizing set
#'   intersections. An alternative to Venn diagrams that scales to many sets:
#'   the top panel shows intersection sizes as bars, the bottom panel shows a
#'   membership dot matrix, and (optionally) the left panel shows set sizes.
#'   Inspired by cnsplots' \code{upsetplot()}.
#'
#' @param sets Optional named list of sets (vectors of elements). When
#'   \code{NULL}, sets are resolved from the data: a named list passed as
#'   \code{x}, or a data frame of binary membership columns.
#' @param sort_by Sort intersections by \code{"size"} (default) or
#'   \code{"degree"} (number of sets in the intersection).
#' @param max_rank Maximum number of intersections to show (default NULL =
#'   all observed intersections).
#' @param min_size Only show intersections with at least this many elements
#'   (default 0).
#' @param bar_color Color of the intersection-size bars (default
#'   \code{"#3C5488"}).
#' @param point_color Color of the membership dots (default \code{"#3C5488"}).
#' @param set_color Color of the set-size bars (default \code{"#E64B35"}).
#' @param show_set_sizes Show the set-size panel on the left (default TRUE).
#' @param show_size_labels Label intersection sizes above bars (default TRUE).
#' @param bar_width Width of the intersection bars (default 0.68).
#'
#' @return A \code{cliplot_type} object for use with \code{\link{cliplot}}.
#'   The composed figure requires \pkg{patchwork}; when patchwork is not
#'   installed a single-panel bar chart of intersection sizes is returned.
#'
#' @details
#' Pass a named list of sets as the first argument, or a data frame of binary
#' membership columns (rows = items, columns = sets). Only intersections that
#' actually occur in the data are shown, matching UpSetR behaviour.
#'
#' @examples
#' \dontrun{
#' # Named list of sets
#' sets = list(Treatment_A = c("TP53", "BRCA1", "MYC"),
#'             Treatment_B = c("TP53", "KRAS", "APC"),
#'             Control     = c("TP53", "MYC", "RB1"))
#' cliplot(sets, type = "upset")
#'
#' # Binary membership data frame
#' cliplot(deg_binary, type = "upset", theme = "nature")
#' }
#'
#' @export
type_upset = function(
    sets            = NULL,
    sort_by         = c("size", "degree"),
    max_rank        = NULL,
    min_size        = 0,
    bar_color       = "#3C5488",
    point_color     = "#3C5488",
    set_color       = "#E64B35",
    show_set_sizes  = TRUE,
    show_size_labels = TRUE,
    bar_width       = 0.68
) {
  cliplot_type(
    data = function(settings, ...) {
      settings$upset_sets = .resolve_sets(sets, settings$x, settings$data)
      settings$upset_sort_by = sort_by
      settings$upset_max_rank = max_rank
      settings$upset_min_size = min_size
      settings$upset_bar_color = bar_color
      settings$upset_point_color = point_color
      settings$upset_set_color = set_color
      settings$upset_show_set_sizes = show_set_sizes
      settings$upset_show_size_labels = show_size_labels
      settings$upset_bar_width = bar_width
    },
    draw = function(data, mapping, settings, ...) {
      sets = settings$upset_sets
      if (is.null(sets)) return(ggplot2::ggplot())
      k = length(sets)
      nms = names(sets)

      inter = .compute_intersections(sets,
        min_size  = settings$upset_min_size,
        max_rank  = settings$upset_max_rank,
        sort_by   = settings$upset_sort_by
      )
      if (nrow(inter) == 0) {
        return(ggplot2::ggplot() +
          ggplot2::labs(title = "No intersections found"))
      }

      # ---- Top: intersection size bars ----
      inter$idf = factor(inter$id, levels = inter$id)
      p_bars = ggplot2::ggplot(inter, ggplot2::aes(
        x = .data[["idf"]], y = .data[["size"]]
      )) +
        ggplot2::geom_col(
          fill = settings$upset_bar_color,
          width = settings$upset_bar_width
        ) +
        ggplot2::scale_y_continuous(expand = ggplot2::expansion(mult = c(0, 0.12))) +
        ggplot2::theme(
          axis.text.x = ggplot2::element_blank(),
          axis.ticks.x = ggplot2::element_blank(),
          panel.grid.major.x = ggplot2::element_blank()
        ) +
        ggplot2::labs(y = "Intersection Size", x = NULL)

      if (isTRUE(settings$upset_show_size_labels)) {
        p_bars = p_bars + ggplot2::geom_text(
          ggplot2::aes(label = .data[["size"]]),
          vjust = -0.4, size = 3, fontface = "bold"
        )
      }

      # ---- Bottom: membership dot matrix ----
      mat_long = utils::stack(inter[nms])
      names(mat_long) = c("value", "set")
      mat_long$id = rep(inter$id, times = k)
      mat_long$idf = factor(mat_long$id, levels = inter$id)
      mat_long$set = factor(mat_long$set, levels = rev(nms))
      mat_long$value = as.factor(mat_long$value)

      p_matrix = ggplot2::ggplot(mat_long, ggplot2::aes(
        x = .data[["idf"]], y = .data[["set"]]
      )) +
        ggplot2::geom_point(
          ggplot2::aes(fill = .data[["value"]]),
          shape = 21, size = 3.4, color = "white", stroke = 0.25
        ) +
        ggplot2::scale_fill_manual(
          values = c("0" = "grey88", "1" = settings$upset_point_color),
          guide = "none"
        ) +
        ggplot2::theme(
          axis.text.x = ggplot2::element_blank(),
          axis.ticks = ggplot2::element_blank(),
          panel.grid.major = ggplot2::element_blank()
        ) +
        ggplot2::labs(y = NULL, x = NULL)

      # ---- Left: set sizes ----
      set_df = data.frame(
        set = factor(nms, levels = rev(nms)),
        size = vapply(sets, length, integer(1)),
        stringsAsFactors = FALSE
      )
      p_sets = ggplot2::ggplot(set_df, ggplot2::aes(
        x = .data[["size"]], y = .data[["set"]]
      )) +
        ggplot2::geom_col(
          fill = settings$upset_set_color,
          width = 0.68
        ) +
        ggplot2::geom_text(
          ggplot2::aes(label = .data[["size"]]),
          hjust = -0.3, size = 3
        ) +
        ggplot2::scale_x_continuous(expand = ggplot2::expansion(mult = c(0, 0.15))) +
        ggplot2::theme(
          axis.text.y = ggplot2::element_text(face = "bold", size = 9),
          panel.grid.major.y = ggplot2::element_blank()
        ) +
        ggplot2::labs(x = "Set Size", y = NULL)

      if (!isTRUE(settings$upset_show_set_sizes)) {
        p_matrix = p_matrix +
          ggplot2::theme(axis.text.y = ggplot2::element_blank())
        p_bars = p_bars + ggplot2::theme(axis.text.y = ggplot2::element_blank())
        return(p_bars / p_matrix)
      }

      if (!requireNamespace("patchwork", quietly = TRUE)) {
        return(p_bars)
      }

      # ---- Compose ----
      right = p_bars / p_matrix +
        patchwork::plot_layout(heights = c(2, 3))
      layout = p_sets + right +
        patchwork::plot_layout(widths = c(1, 4))
      layout
    },
    name = "upset"
  )
}
