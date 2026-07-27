# Waterfall Plot Type

Creates publication-ready waterfall plots commonly used in oncology to
display best tumor response for each patient. Bars are sorted by
response and colored by response category.

## Usage

``` r
type_waterfall(
  bar_fill = "#0073C2",
  bar_width = 0.7,
  bar_alpha = 0.9,
  response_thresholds = c(PD = 20, PR = -30, CR = -100),
  show_labels = FALSE,
  label_size = 2.5,
  show_threshold_labels = TRUE
)
```

## Arguments

- bar_fill:

  Bar fill color for default (default "#0073C2")

- bar_width:

  Bar width (default 0.7)

- bar_alpha:

  Bar transparency (default 0.9)

- response_thresholds:

  Named vector of thresholds for coloring:
  `c("PD" = 20, "PR" = -30, "CR" = -100)`. PD = Progressive Disease (at
  least +20 percent), SD = Stable Disease (between thresholds), PR =
  Partial Response (at most -30 percent), CR = Complete Response.

- show_labels:

  Show patient IDs on bars (default FALSE)

- label_size:

  Size of patient labels (default 2.5)

- show_threshold_labels:

  Add PD/PR threshold labels.

## Value

A cliplot_type object for use with
[`cliplot`](https://vanhungtran.github.io/cliomicplot/reference/cliplot.md).

## Examples

``` r
if (FALSE) { # \dontrun{
# Generate example tumor response data
set.seed(123)
tumor_data = data.frame(
  Patient   = paste0("Pt", 1:30),
  Response  = sort(rnorm(30, mean = -20, sd = 30)),
  BestResp  = sample(c("CR", "PR", "SD", "PD"), 30, replace = TRUE)
)

cliplot(Response ~ Patient, data = tumor_data, type = "waterfall")
} # }
```
