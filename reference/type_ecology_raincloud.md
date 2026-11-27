# Ecology Half-Boxed Raincloud Plot Type

Creates publication-ready ecology and demographic viability raincloud
plots featuring a white-filled boxplot with colored outline, open square
outliers, a prominent circular mean marker, a solid half-violin density
slab, a dashed reference baseline, and bottom-positioned mean + 95
percent CI text annotations.

## Usage

``` r
type_ecology_raincloud(
  alpha = 0.98,
  box_width = 0.17,
  box_nudge = -0.15,
  box_fill = "#FFFFFF",
  box_lwd = 1.1,
  outlier_shape = 0,
  outlier_size = 2,
  outlier_stroke = 0.8,
  show_mean = TRUE,
  mean_nudge = -0.01,
  mean_shape = 16,
  mean_size = 3.8,
  violin_width = 0.52,
  adjust = 1.1,
  side = "right",
  ref_line = 1,
  ref_line_col = "grey70",
  ref_line_lty = "dashed",
  ref_line_lwd = 1.2,
  show_stats = TRUE,
  stat_param = "λ",
  stat_ci_level = 0.95,
  stat_digits = 3,
  stat_y_pos = NULL,
  stat_text_size = 3.4,
  stat_labels = NULL
)

type_half_boxed(
  alpha = 0.98,
  box_width = 0.17,
  box_nudge = -0.15,
  box_fill = "#FFFFFF",
  box_lwd = 1.1,
  outlier_shape = 0,
  outlier_size = 2,
  outlier_stroke = 0.8,
  show_mean = TRUE,
  mean_nudge = -0.01,
  mean_shape = 16,
  mean_size = 3.8,
  violin_width = 0.52,
  adjust = 1.1,
  side = "right",
  ref_line = 1,
  ref_line_col = "grey70",
  ref_line_lty = "dashed",
  ref_line_lwd = 1.2,
  show_stats = TRUE,
  stat_param = "λ",
  stat_ci_level = 0.95,
  stat_digits = 3,
  stat_y_pos = NULL,
  stat_text_size = 3.4,
  stat_labels = NULL
)
```

## Arguments

- alpha:

  Violin transparency (default 0.7 for default, 0.98 for ecology).

- box_width:

  Width of boxplot (default 0.2 for default, 0.17 for ecology).

- box_nudge:

  Horizontal nudge for boxplot in ecology style (default -0.15).

- box_fill:

  Fill color of boxplot (default white \`"#FFFFFF"\` in ecology style,
  semi-transparent in default).

- box_lwd:

  Line width for boxplot borders and whiskers (default 1.1 for ecology
  style).

- outlier_shape:

  Outlier point shape (default \`0\` \[open square\] for ecology style,
  \`NA\` for default).

- outlier_size:

  Outlier point size (default 2.0 for ecology style).

- outlier_stroke:

  Outlier border stroke thickness (default 0.8 for ecology style).

- show_mean:

  Logical. Draw a solid circular mean marker between boxplot and violin
  (default TRUE for ecology style).

- mean_nudge:

  Horizontal nudge for mean point (default -0.01).

- mean_shape:

  Mean marker shape (default 16, solid circle).

- mean_size:

  Mean marker size (default 3.8).

- violin_width:

  Width of half-violin slab (default 0.52).

- adjust:

  Bandwidth adjustment for density estimation (default 1.1).

- side:

  Which side to draw the half-violin: \`"right"\`, \`"left"\`, or
  \`"both"\`.

- ref_line:

  Numeric y-intercept for horizontal dashed reference line (e.g. \`1.0\`
  for population replacement rate). \`NULL\` to disable.

- ref_line_col:

  Color for reference line (default \`"grey70"\`).

- ref_line_lty:

  Linetype for reference line (default \`"dashed"\`).

- ref_line_lwd:

  Linewidth for reference line (default 1.2).

- show_stats:

  Logical. Compute and display sample mean and confidence intervals
  below each category (default TRUE for ecology style).

- stat_param:

  Parameter symbol or name for stats label (default \`"λ"\`, or
  \`"mean"\`).

- stat_ci_level:

  Confidence interval level (default 0.95 for 95 percent CI).

- stat_digits:

  Number of decimal digits to format in stats label (default 3).

- stat_y_pos:

  Custom y-coordinate for bottom stats labels. \`NULL\` automatically
  calculates position below minimum data range.

- stat_text_size:

  Font size for bottom stats labels (default 3.4).

- stat_labels:

  Optional custom character vector to override calculated stats labels.

## Value

A cliplot_type object.
