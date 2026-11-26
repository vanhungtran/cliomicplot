# Confusion Matrix Type

Creates publication-ready confusion matrix heatmaps for classification
results. Cell fill encodes the count, cell text shows the number of
observations, and the caption reports accuracy (and, for 2x2 tables, a
Fisher exact test p-value). Inspired by cnsplots' `confusionplot()`.

## Usage

``` r
type_confusion(
  add_pvalue = FALSE,
  show_accuracy = TRUE,
  color_low = "#F7F7F7",
  color_high = "#B2182B",
  text_color = "white",
  label_size = 4.2,
  show_totals = FALSE
)
```

## Arguments

- add_pvalue:

  Add a Fisher exact test p-value to the caption for 2x2 tables (default
  FALSE).

- show_accuracy:

  Show classification accuracy in the caption (default TRUE).

- color_low:

  Low end of the count color gradient (default "#F7F7F7").

- color_high:

  High end of the count color gradient (default "#B2182B").

- text_color:

  Color of the count labels (default "white").

- label_size:

  Size of the count labels (default 4.2).

- show_totals:

  Show row/column marginal totals (default FALSE).

## Value

A `cliplot_type` object for use with
[`cliplot`](https://vanhungtran.github.io/cliomicplot/reference/cliplot.md).

## Details

Use the formula interface
`cliplot(truth ~ predicted, data, type = "confusion")` where `y` is the
ground-truth variable and `x` the predicted variable. A data frame with
exactly two factor/character columns is also accepted directly.

## Examples

``` r
if (FALSE) { # \dontrun{
cliplot(truth ~ predicted, data = predictions, type = "confusion")
cliplot(truth ~ predicted, data = predictions,
        type = type_confusion(add_pvalue = TRUE))
} # }
```
