# ===========================================================================
# cliomicplot: Waterfall Plot Type
# For tumor response / change from baseline visualization
# ===========================================================================

#' Waterfall Plot Type
#'
#' @description Creates publication-ready waterfall plots commonly used in
#'   oncology to display best tumor response for each patient. Bars are
#'   sorted by response and colored by response category.
#'
#' @param bar_fill Bar fill color for default (default "#0073C2")
#' @param bar_width Bar width (default 0.7)
#' @param bar_alpha Bar transparency (default 0.9)
#' @param response_thresholds Named vector of thresholds for coloring:
#'   \code{c("PD" = 20, "PR" = -30, "CR" = -100)}.
#'   PD = Progressive Disease (at least +20 percent), SD = Stable Disease
#'   (between thresholds), PR = Partial Response (at most -30 percent),
#'   CR = Complete Response.
#' @param show_labels Show patient IDs on bars (default FALSE)
#' @param label_size Size of patient labels (default 2.5)
#' @param show_threshold_labels Add PD/PR threshold labels.
#' @param n_skip_label Number of rows to stagger x-axis labels across
#'   to prevent overlap (default: auto — 1 row for ≤15 patients, 2 for
#'   16–35, 3 for >35). Set to 1 to force single-row labels.
#'
#' @return A cliplot_type object for use with \code{\link{cliplot}}.
#'
#' @examples
#' \dontrun{
#' # Generate example tumor response data
#' set.seed(123)
#' tumor_data = data.frame(
#'   Patient   = paste0("Pt", 1:30),
#'   Response  = sort(rnorm(30, mean = -20, sd = 30)),
#'   BestResp  = sample(c("CR", "PR", "SD", "PD"), 30, replace = TRUE)
#' )
#'
#' cliplot(Response ~ Patient, data = tumor_data, type = "waterfall")
#' }
#'
#' @export
type_waterfall = function(
    bar_fill    = "#0073C2",
    bar_width   = 0.7,
    bar_alpha   = 0.9,
    response_thresholds = c("PD" = 20, "PR" = -30, "CR" = -100),
    show_labels = FALSE,
    label_size  = 2.5,
    show_threshold_labels = TRUE,
    n_skip_label = NULL
) {
  cliplot_type(
    data = function(settings, ...) {
      df = settings$data
      if (is.null(df)) {
        df = data.frame(
          patient  = if (!is.null(names(settings$y))) names(settings$y)
                     else settings$x,
          response = settings$y,
          stringsAsFactors = FALSE
        )
      } else {
        df$patient  = settings$x %||% df[[1]]
        df$response = settings$y %||% df[[2]]
      }

      # Sort by response (descending)
      df = df[order(-df$response), ]
      df$patient = factor(df$patient, levels = df$patient)

      # Classify response (safely check for named threshold values)
      pd_thresh = if ("PD" %in% names(response_thresholds)) response_thresholds[["PD"]] else 20
      pr_thresh = if ("PR" %in% names(response_thresholds)) response_thresholds[["PR"]] else -30
      cr_thresh = if ("CR" %in% names(response_thresholds)) response_thresholds[["CR"]] else -100
      df$category = "SD"
      df$category[df$response >= pd_thresh] = "PD"
      df$category[df$response <= pr_thresh] = "PR"
      df$category[df$response <= cr_thresh] = "CR"
      # Override with provided BestResp if available
      if ("BestResp" %in% names(df) || "category" %in% names(df)) {
        if ("BestResp" %in% names(df)) df$category = df$BestResp
      }
      df$category = factor(df$category, levels = c("CR", "PR", "SD", "PD"))

      # Color mapping
      resp_colors = c(
        "CR" = "#20854E",  # green
        "PR" = "#4DBBD5",  # blue
        "SD" = "#8A949E",  # grey
        "PD" = "#E64B35"   # red
      )

      # Determine x-axis label formatting based on patient count
      n_patients = nrow(df)
      if (is.null(n_skip_label)) {
        axis_angle = if (n_patients > 20) 90 else 45
        axis_vjust = if (n_patients > 20) 0.5 else 1
      } else {
        axis_angle = if (n_skip_label > 1) 90 else 45
        axis_vjust = if (n_skip_label > 1) 0.5 else 1
      }

      # Adaptive text size
      if (n_patients > 40) axis_text_size = 5.5
      else if (n_patients > 25) axis_text_size = 6.5
      else axis_text_size = 7

      # Legend placement: default to top horizontal for spacious waterfall layouts
      if (is.null(settings$legend)) {
        settings$legend = "top"
      }

      # When bar labels are shown, mark axis text blank to eliminate double-label collision
      if (isTRUE(show_labels)) {
        settings$axis_text_blank = TRUE
      }

      settings$waterfall_data     = df
      settings$bar_width          = bar_width
      settings$bar_alpha          = bar_alpha
      settings$show_labels        = show_labels
      settings$label_size         = label_size
      settings$resp_colors        = resp_colors
      settings$response_thresholds = response_thresholds
      settings$show_threshold_labels = show_threshold_labels
      settings$axis_text_size     = axis_text_size
      settings$axis_angle         = axis_angle
      settings$axis_vjust         = axis_vjust
    },
    draw = function(data, mapping, settings, ...) {
      df = settings$waterfall_data
      if (is.null(df)) return(ggplot2::ggplot())

      p = ggplot2::ggplot(df, ggplot2::aes(
        x    = .data[["patient"]],
        y    = .data[["response"]],
        fill = .data[["category"]]
      )) +
        ggplot2::geom_col(
          width = settings$bar_width,
          alpha = settings$bar_alpha,
          color = "white",
          linewidth = 0.25
        ) +
        ggplot2::scale_fill_manual(
          values = settings$resp_colors,
          name   = "Response: ",
          drop   = FALSE
        ) +
        ggplot2::guides(
          fill = ggplot2::guide_legend(
            nrow           = 1,
            byrow          = TRUE,
            title.position = "left",
            title.vjust    = 0.85,
            keywidth       = ggplot2::unit(4, "mm"),
            keyheight      = ggplot2::unit(3.5, "mm")
          )
        ) +
        ggplot2::scale_y_continuous(
          expand = ggplot2::expansion(mult = c(0.12, 0.12))
        ) +
        ggplot2::labs(x = "", y = "Best Change from Baseline (%)") +
        ggplot2::theme(
          panel.grid.major.x = ggplot2::element_blank()
        )

      if (isTRUE(settings$show_labels)) {
        p = p + ggplot2::theme(
          axis.text.x  = ggplot2::element_blank(),
          axis.ticks.x = ggplot2::element_blank()
        )
      } else {
        p = p + ggplot2::theme(
          axis.text.x = ggplot2::element_text(
            angle = settings$axis_angle,
            hjust = 1,
            vjust = settings$axis_vjust,
            size  = settings$axis_text_size
          )
        )
      }

      # Add horizontal reference lines
      ref_lines = c(0,
        if ("PD" %in% names(settings$response_thresholds)) settings$response_thresholds[["PD"]] else 20,
        if ("PR" %in% names(settings$response_thresholds)) settings$response_thresholds[["PR"]] else -30)
      p = p + ggplot2::geom_hline(
        yintercept = ref_lines,
        linetype   = c("solid", "dashed", "dashed"),
        color      = c("#4A4F57", "#6C737D", "#6C737D"),
        linewidth  = c(0.45, 0.4, 0.4)
      )

      if (settings$show_threshold_labels) {
        p = p +
          ggplot2::annotate(
            "label", x = Inf, y = ref_lines[2], label = "PD (+20%)",
            hjust = 1.05, vjust = -0.3, size = 3,
            linewidth = 0, fill = "white", color = "#E64B35", fontface = "bold", alpha = 0.85
          ) +
          ggplot2::annotate(
            "label", x = -Inf, y = ref_lines[3], label = "PR (-30%)",
            hjust = -0.05, vjust = 1.3, size = 3,
            linewidth = 0, fill = "white", color = "#4DBBD5", fontface = "bold", alpha = 0.85
          )
      }

      # Add patient labels on bars if requested
      if (settings$show_labels) {
        p = p + ggplot2::geom_text(
          ggplot2::aes(
            label = .data[["patient"]],
            vjust = ifelse(.data[["response"]] >= 0, -0.4, 1.4)
          ),
          size        = settings$label_size,
          angle       = 90,
          color       = "#30343B",
          show.legend = FALSE
        )
      }

      p
    },
    name = "waterfall"
  )
}
