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
#' @param n_skip_label Skip every nth x-axis label to prevent overlap when
#'   many patients are plotted (default 1 = show all). Set to 2 for every
#'   other label, 3 for every third, etc. Automatically enabled when
#'   there are more than 20 patients if not explicitly set.
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

      # Determine x-axis label skipping
      n_patients = nrow(df)
      if (is.null(n_skip_label)) {
        # Auto: skip labels when > 20 patients to prevent overlap
        if (n_patients > 40) n_skip_label = 3
        else if (n_patients > 20) n_skip_label = 2
        else n_skip_label = 1
      }

      # Build display labels: blank out skipped positions
      df$xlabel = as.character(df$patient)
      df$xlabel[seq_len(n_patients) %% n_skip_label != 1] = ""

      settings$waterfall_data     = df
      settings$bar_width          = bar_width
      settings$bar_alpha          = bar_alpha
      settings$show_labels        = show_labels
      settings$label_size         = label_size
      settings$resp_colors        = resp_colors
      settings$response_thresholds = response_thresholds
      settings$show_threshold_labels = show_threshold_labels
      settings$n_skip_label       = n_skip_label
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
          name   = "Response",
          drop   = FALSE
        ) +
        ggplot2::scale_x_discrete(labels = stats::setNames(df$xlabel, df$patient)) +
        ggplot2::labs(x = "", y = "Best Change from Baseline (%)") +
        ggplot2::theme(
          axis.text.x = ggplot2::element_text(angle = 45, hjust = 1, size = 7),
          panel.grid.major.x = ggplot2::element_blank()
        )

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
            "label", x = Inf, y = ref_lines[2], label = "PD +20%",
            hjust = 1.05, vjust = -0.25, size = 3,
            linewidth = 0, fill = "white", color = "#4A4F57", alpha = 0.9
          ) +
          ggplot2::annotate(
            "label", x = Inf, y = ref_lines[3], label = "PR -30%",
            hjust = 1.05, vjust = 1.25, size = 3,
            linewidth = 0, fill = "white", color = "#4A4F57", alpha = 0.9
          )
      }

      # Add patient labels if requested
      if (settings$show_labels) {
        p = p + ggrepel::geom_text_repel(
          ggplot2::aes(
            label = .data[["patient"]],
            y     = .data[["response"]]
          ),
          size             = settings$label_size,
          angle            = 90,
          direction        = "y",
          max.overlaps     = 100,
          min.segment.length = 0,
          box.padding      = 0.15,
          point.padding    = 0.1,
          show.legend      = FALSE
        )
      }

      p
    },
    name = "waterfall"
  )
}
