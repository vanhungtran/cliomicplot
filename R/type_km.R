# ===========================================================================
# cliomicplot: Kaplan-Meier Survival Curve Type
# ===========================================================================

#' Kaplan-Meier Survival Curve Type
#'
#' @description Creates publication-ready Kaplan-Meier survival curves with
#'   risk tables and log-rank test p-values. Uses survminer under the hood.
#'
#' @param time Column name or vector of survival times
#' @param event Column name or vector of event indicators (1 = event, 0 = censored)
#' @param risk_table Show risk table below the plot (default TRUE)
#' @param pval Show log-rank p-value (default TRUE)
#' @param conf_int Show confidence intervals (default FALSE)
#' @param median_line Show median survival line (default FALSE)
#' @param palette Color palette for groups
#' @param linewidth Line width for survival curves (default 0.8)
#' @param xlab Label for x-axis (default "Time")
#' @param ylab Label for y-axis (default "Survival Probability")
#'
#' @return A cliplot_type object for use with \code{\link{cliplot}}.
#'
#' @examples
#' \dontrun{
#' library(survival)
#' data(lung)
#'
#' # Basic KM plot
#' cliplot(Surv(time, status) ~ sex, data = lung, type = "km")
#'
#' # With risk table and CI
#' cliplot(Surv(time, status) ~ sex, data = lung,
#'         type = type_km(risk_table = TRUE, conf_int = TRUE))
#' }
#'
#' @export
type_km = function(
    time        = NULL,
    event       = NULL,
    risk_table  = TRUE,
    pval        = TRUE,
    conf_int    = FALSE,
    median_line = FALSE,
    palette     = NULL,
    linewidth   = 0.8,
    xlab        = "Time",
    ylab        = "Survival Probability"
) {
  cliplot_type(
    data = function(settings, ...) {
      # KM needs a Surv object or time + event columns
      # The formula interface: Surv(time, status) ~ group
      # We expect settings to have the necessary components

      df = settings$data
      group_var = settings$by

      if (is.null(df)) {
        stop("Kaplan-Meier requires a data frame with time, event, and group columns")
      }

      # Determine time/event column names
      time_col  = time  %||% settings$km_time
      event_col = event %||% settings$km_event

      # Validate we have column names
      if (is.null(time_col) || is.null(event_col)) {
        stop("Cannot determine time/event columns. Provide `time` and `event` to type_km().")
      }

      # Use the real group variable name (from Surv formula) if available,
      # otherwise use ..by..
      group_col = settings$km_group %||% "..by.."

      # Add group variable to df if needed
      if (!is.null(group_var)) {
        df[[group_col]] = as.factor(group_var)
        settings$km_n_groups = nlevels(df[[group_col]])
      } else {
        settings$km_n_groups = 1L
      }

      # Build the formula text for later use
      if (is.null(group_var)) {
        surv_formula_text = paste0("survival::Surv(", time_col, ", ", event_col, ") ~ 1")
      } else {
        surv_formula_text = paste0("survival::Surv(", time_col, ", ", event_col, ") ~ ", group_col)
      }
      surv_formula = stats::as.formula(surv_formula_text)
      # Attach the data frame as the formula environment for survminer
      df_env = new.env(parent = globalenv())
      for (nm in names(df)) assign(nm, df[[nm]], envir = df_env)
      environment(surv_formula) = df_env

      settings$surv_formula  = surv_formula
      settings$surv_formula_text = surv_formula_text
      settings$km_time       = time_col
      settings$km_event      = event_col
      settings$km_group_col  = group_col
      settings$risk_table    = risk_table
      settings$pval          = pval
      settings$conf_int      = conf_int
      settings$median_line   = median_line
      settings$palette       = palette %||% settings$palette
      settings$linewidth     = linewidth
      settings$xlab          = xlab
      settings$ylab          = ylab
      settings$km_data       = df
    },
    draw = function(data, mapping, settings, ...) {
      df = settings$km_data
      if (is.null(df)) return(ggplot2::ggplot())

      group_col = settings$km_group_col %||% "..by.."
      n_groups  = settings$km_n_groups %||% 1L

      # Get palette colors
      pal = get_cli_palette(settings$palette %||% "jco", n_groups)

      # Fit survival model
      fit = survival::survfit(settings$surv_formula, data = df)

      # Patch: survminer re-evaluates fit$call$formula which was
      # constructed from text. Store a formula with df columns accessible.
      df_env = new.env(parent = globalenv())
      for (nm in names(df)) assign(nm, df[[nm]], envir = df_env)
      fit$call$formula = stats::as.formula(settings$surv_formula_text,
                                            env = df_env)

      # Build the ggsurvplot
      legend_labs = if (n_groups > 1 && group_col %in% names(df))
                      levels(as.factor(df[[group_col]])) else NULL

      p = survminer::ggsurvplot(
        fit,
        data          = df,
        pval          = settings$pval,
        conf.int      = settings$conf_int,
        risk.table    = settings$risk_table,
        surv.median.line = if (settings$median_line) "hv" else "none",
        palette       = pal,
        size          = settings$linewidth,
        xlab          = settings$xlab,
        ylab          = settings$ylab,
        ggtheme       = theme_cli_base(),
        legend.title  = "",
        legend.labs   = legend_labs
      )

      if (settings$risk_table) {
        if (requireNamespace("patchwork", quietly = TRUE)) {
          p = patchwork::wrap_plots(p$plot, p$table, ncol = 1, heights = c(3, 1))
        } else {
          p = p$plot
        }
      } else {
        p = p$plot
      }

      p
    },
    name = "km"
  )
}
