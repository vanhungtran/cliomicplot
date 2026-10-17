# ===========================================================================
# cliomicplot: ROC Curve Type
# Inspired by cnsplots rocplot() - classifier / biomarker evaluation
# ===========================================================================

# Internal helpers -----------------------------------------------------------

# Convert a response vector to binary 0/1
.as_binary_response = function(x, positive = NULL) {
  if (is.logical(x)) return(as.numeric(x))
  if (is.factor(x) || is.character(x)) {
    lev = unique(as.character(x))
    if (length(lev) != 2L) {
      stop("ROC response must be binary (two levels), got ", length(lev),
           " levels.", call. = FALSE)
    }
    if (is.null(positive)) positive = lev[2L]
    if (!positive %in% lev) {
      stop("`positive` level not found in response.", call. = FALSE)
    }
    return(as.numeric(as.character(x) == positive))
  }
  if (is.numeric(x)) {
    vals = unique(x)
    if (!all(vals %in% c(0, 1))) {
      stop("Numeric ROC response must contain only 0/1 values.", call. = FALSE)
    }
    return(x)
  }
  stop("Cannot convert response to a binary indicator.", call. = FALSE)
}

# Compute ROC curve points + AUC with base R (no external dependency)
.compute_roc_curve = function(response, score, positive = NULL) {
  resp = .as_binary_response(response, positive)
  ok = !is.na(resp) & !is.na(score)
  resp = resp[ok]
  score = score[ok]
  n = length(resp)
  n_pos = sum(resp == 1)
  n_neg = sum(resp == 0)
  if (n_pos == 0 || n_neg == 0) {
    warning("ROC requires at least one positive and one negative observation.",
            call. = FALSE)
    return(NULL)
  }
  ord = order(score, decreasing = TRUE)
  score = score[ord]
  resp = resp[ord]
  tp = cumsum(resp)
  fp = cumsum(1 - resp)
  fpr = c(0, fp / n_neg, 1)
  tpr = c(0, tp / n_pos, 1)
  # Collapse ties so the polygon area is computed correctly
  keep = !duplicated(paste(fpr, tpr, sep = ":")) | seq_along(fpr) == length(fpr)
  fpr = fpr[keep]
  tpr = tpr[keep]
  ord2 = order(fpr)
  fpr = fpr[ord2]
  tpr = tpr[ord2]
  auc = sum(diff(fpr) * (tpr[-1L] + tpr[-length(tpr)]) / 2)
  list(fpr = fpr, tpr = tpr, auc = auc, n = n, n_pos = n_pos, n_neg = n_neg)
}

# AUC 95% CI via pROC when available (bootstrap)
.auc_ci = function(response, score, positive = NULL) {
  if (!requireNamespace("pROC", quietly = TRUE)) return(NULL)
  resp = .as_binary_response(response, positive)
  ok = !is.na(resp) & !is.na(score)
  roc_obj = suppressWarnings(
    pROC::roc(resp[ok], score[ok], quiet = TRUE, direction = "<")
  )
  ci = tryCatch(pROC::ci.auc(roc_obj, quiet = TRUE), error = function(e) NULL)
  if (is.null(ci) || length(ci) != 3L) return(NULL)
  ci
}

# Auto-detect the response column in a data frame
.detect_response_col = function(df, response = NULL) {
  if (!is.null(response) && response %in% names(df)) return(response)
  for (nm in c("outcome", "response", "class", "label", "status", "disease",
               "group", "truth", "y")) {
    if (nm %in% names(df)) return(nm)
  }
  # First column with exactly two unique non-NA values
  for (nm in names(df)) {
    col = df[[nm]]
    if (is.logical(col)) return(nm)
    if ((is.factor(col) || is.character(col)) &&
        length(unique(stats::na.omit(col))) == 2L) return(nm)
    if (is.numeric(col) && all(col %in% c(0, 1, NA))) return(nm)
  }
  NULL
}

#' ROC Curve Type
#'
#' @description Creates publication-ready ROC (Receiver Operating
#'   Characteristic) curves for classifier and biomarker evaluation. Curves
#'   are drawn with the AUC annotated directly in the legend, mirroring the
#'   style of cnsplots' \code{rocplot()}. Multiple predictors can be compared
#'   in a single panel.
#'
#' @param predictors Optional character vector of score-column names in
#'   \code{data} to compare. When supplied, one ROC curve is drawn per column.
#'   If \code{NULL}, a single curve is built from the formula variables
#'   (\code{cliplot(outcome ~ score, ...)}).
#' @param response Optional name of the binary outcome column in \code{data}.
#'   Auto-detected when \code{NULL} (looks for \code{outcome}, \code{response},
#'   \code{class}, \code{label}, \code{status}, \code{disease}, \code{group},
#'   then the first binary column).
#' @param positive Label of the positive class for factor responses. Defaults
#'   to the second level.
#' @param auc_ci Add a bootstrap 95\% CI to the AUC legend label (requires the
#'   \pkg{pROC} package; slower for large data).
#' @param linewidth Line width of the ROC curves (default 0.9).
#' @param diagonal Draw the chance diagonal (default TRUE).
#' @param add_points Add threshold points when the sample size is small
#'   (default TRUE for n <= 200).
#' @param point_size Size of threshold points (default 1.6).
#' @param max_curves Maximum number of predictors to plot (default 12).
#' @param col Color for a single ROC curve (default from \code{clipar}).
#'
#' @return A \code{cliplot_type} object for use with \code{\link{cliplot}}.
#'
#' @details
#' Use the formula interface \code{cliplot(outcome ~ score, data, type = "roc")}
#' for a single classifier, or pass a data frame with \code{type_roc(predictors =
#' c("marker_A", "marker_B"))} to compare several biomarkers side by side.
#' AUC values are computed with a dependency-free rank-based estimator; the
#' optional 95\% CI uses \pkg{pROC}.
#'
#' @examples
#' \dontrun{
#' # Single classifier
#' cliplot(outcome ~ score, data = biomarker_df, type = "roc")
#'
#' # Compare three biomarkers
#' cliplot(biomarker_df, type = type_roc(predictors = c("marker_A", "marker_B", "marker_C")))
#' }
#'
#' @export
type_roc = function(
    predictors  = NULL,
    response    = NULL,
    positive    = NULL,
    auc_ci      = FALSE,
    linewidth   = 0.9,
    diagonal    = TRUE,
    add_points  = TRUE,
    point_size  = 1.6,
    max_curves  = 12,
    col         = NULL
) {
  cliplot_type(
    data = function(settings, ...) {
      df = settings$data
      resp  = NULL
      scores = list()

      if (!is.null(predictors) && !is.null(df) && is.data.frame(df)) {
        resp_col = .detect_response_col(df, response)
        if (is.null(resp_col)) {
          stop("Cannot find a binary response column in `data`. Provide `response`.",
               call. = FALSE)
        }
        resp = df[[resp_col]]
        for (nm in predictors) {
          if (!nm %in% names(df)) {
            stop("Predictor column not found in `data`: ", nm, call. = FALSE)
          }
          scores[[nm]] = df[[nm]]
        }
      } else if (!is.null(df) && is.data.frame(df) &&
                 (is.null(settings$y) || is.null(settings$x))) {
        # data.frame method: auto-detect response + all numeric predictors
        resp_col = .detect_response_col(df, response)
        if (is.null(resp_col)) {
          stop("Cannot find a binary response column in `data`. Provide `response`.",
               call. = FALSE)
        }
        resp = df[[resp_col]]
        num_cols = names(df)[vapply(df, is.numeric, logical(1))]
        num_cols = setdiff(num_cols, resp_col)
        if (is.null(predictors)) {
          predictors = num_cols
        }
        for (nm in predictors[seq_len(min(max_curves, length(predictors)))]) {
          scores[[nm]] = df[[nm]]
        }
      } else {
        # formula method: y = response, x = single predictor
        if (is.null(settings$y) || is.null(settings$x)) {
          stop("ROC needs a formula like `outcome ~ score` or a data frame ",
               "with predictors.", call. = FALSE)
        }
        resp = settings$y
        scores[[.predictor_label(settings$x)]] = settings$x
      }

      if (length(scores) == 0) {
        stop("No predictor(s) found for ROC analysis.", call. = FALSE)
      }
      settings$roc_response = resp
      settings$roc_scores   = scores
      settings$roc_auc_ci   = auc_ci
      settings$roc_positive = positive
      settings$roc_linewidth = linewidth
      settings$roc_diagonal = diagonal
      settings$roc_add_points = add_points
      settings$roc_point_size = point_size
      settings$roc_col      = col %||% settings$col.default %||% "#2B6CB0"
    },
    draw = function(data, mapping, settings, ...) {
      resp   = settings$roc_response
      scores = settings$roc_scores
      if (is.null(resp)) return(ggplot2::ggplot())

      curves = list()
      for (nm in names(scores)) {
        cur = .compute_roc_curve(resp, scores[[nm]], settings$roc_positive)
        if (!is.null(cur)) curves[[nm]] = cur
      }
      if (length(curves) == 0) return(ggplot2::ggplot())

      # Optional AUC CI (pROC)
      ci_map = NULL
      if (isTRUE(settings$roc_auc_ci) && requireNamespace("pROC", quietly = TRUE)) {
        ci_map = lapply(names(scores), function(nm) {
          .auc_ci(resp, scores[[nm]], settings$roc_positive)
        })
        names(ci_map) = names(scores)
      }

      curve_df = do.call(rbind, lapply(names(curves), function(nm) {
        c = curves[[nm]]
        data.frame(
          fpr   = c$fpr,
          tpr   = c$tpr,
          model = nm,
          stringsAsFactors = FALSE
        )
      }))
      curve_df$model = factor(curve_df$model, levels = names(curves))

      legend_labels = vapply(names(curves), function(nm) {
        lab = sprintf("%s (AUC = %.3f)", nm, curves[[nm]]$auc)
        ci = ci_map[[nm]]
        if (!is.null(ci)) {
          lab = sprintf("%s (AUC = %.3f, 95%% CI %.3f-%.3f)",
                        nm, curves[[nm]]$auc, ci[1], ci[3])
        }
        lab
      }, character(1))

      p = ggplot2::ggplot(
        curve_df,
        ggplot2::aes(x = .data[["fpr"]], y = .data[["tpr"]],
                     color = .data[["model"]])
      ) +
        ggplot2::geom_line(linewidth = settings$roc_linewidth)

      if (isTRUE(settings$roc_diagonal)) {
        p = p + ggplot2::geom_abline(
          slope = 1, intercept = 0,
          linetype = "dashed", color = "grey55", linewidth = 0.4
        )
      }

      n_total = curves[[1]]$n
      if (isTRUE(settings$roc_add_points) && n_total <= 200) {
        p = p + ggplot2::geom_point(size = settings$roc_point_size, stroke = 0.2)
      }

      n_curves = length(curves)
      if (n_curves == 1) {
        p = p + ggplot2::scale_color_manual(
          values = settings$roc_col,
          labels = legend_labels,
          name = ""
        )
      } else {
        p = p + ggplot2::scale_color_manual(
          values = get_cli_palette(settings$palette %||% "npg", n_curves),
          labels = legend_labels,
          name = ""
        ) +
          ggplot2::guides(color = ggplot2::guide_legend(
            override.aes = list(linewidth = 1.4))
          )
      }

      p = p +
        ggplot2::coord_equal() +
        ggplot2::labs(
          x = "False Positive Rate (1 - Specificity)",
          y = "True Positive Rate (Sensitivity)"
        ) +
        ggplot2::theme(legend.position = settings$legend %||% "right")

      p
    },
    name = "roc"
  )
}

# Label a bare predictor vector
.predictor_label = function(x) {
  if (!is.null(names(x))) return(names(x)[1])
  nm = deparse(substitute(x))
  if (nchar(nm) > 24) return("Score")
  nm
}
