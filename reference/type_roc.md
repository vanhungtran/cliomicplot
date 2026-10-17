# ROC Curve Type

Creates publication-ready ROC (Receiver Operating Characteristic) curves
for classifier and biomarker evaluation. Curves are drawn with the AUC
annotated directly in the legend, mirroring the style of cnsplots'
`rocplot()`. Multiple predictors can be compared in a single panel.

## Usage

``` r
type_roc(
  predictors = NULL,
  response = NULL,
  positive = NULL,
  auc_ci = FALSE,
  linewidth = 0.9,
  diagonal = TRUE,
  add_points = TRUE,
  point_size = 1.6,
  max_curves = 12,
  col = NULL
)
```

## Arguments

- predictors:

  Optional character vector of score-column names in `data` to compare.
  When supplied, one ROC curve is drawn per column. If `NULL`, a single
  curve is built from the formula variables
  (`cliplot(outcome ~ score, ...)`).

- response:

  Optional name of the binary outcome column in `data`. Auto-detected
  when `NULL` (looks for `outcome`, `response`, `class`, `label`,
  `status`, `disease`, `group`, then the first binary column).

- positive:

  Label of the positive class for factor responses. Defaults to the
  second level.

- auc_ci:

  Add a bootstrap 95% CI to the AUC legend label (requires the pROC
  package; slower for large data).

- linewidth:

  Line width of the ROC curves (default 0.9).

- diagonal:

  Draw the chance diagonal (default TRUE).

- add_points:

  Add threshold points when the sample size is small (default TRUE for n
  \<= 200).

- point_size:

  Size of threshold points (default 1.6).

- max_curves:

  Maximum number of predictors to plot (default 12).

- col:

  Color for a single ROC curve (default from `clipar`).

## Value

A `cliplot_type` object for use with
[`cliplot`](https://vanhungtran.github.io/cliomicplot/reference/cliplot.md).

## Details

Use the formula interface `cliplot(outcome ~ score, data, type = "roc")`
for a single classifier, or pass a data frame with
`type_roc(predictors = c("marker_A", "marker_B"))` to compare several
biomarkers side by side. AUC values are computed with a dependency-free
rank-based estimator; the optional 95% CI uses pROC.

## Examples

``` r
if (FALSE) { # \dontrun{
# Single classifier
cliplot(outcome ~ score, data = biomarker_df, type = "roc")

# Compare three biomarkers
cliplot(biomarker_df, type = type_roc(predictors = c("marker_A", "marker_B", "marker_C")))
} # }
```
