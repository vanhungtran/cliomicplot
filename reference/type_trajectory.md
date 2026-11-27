# Longitudinal Clinical Trajectory Plot Type

Creates publication-ready longitudinal clinical trajectories (Clinical
Archetype 4). Overlays semi-transparent individual patient spaghetti
trajectories onto shaded background clinical severity bands (e.g., Mild,
Moderate, Severe) or clean clinical frames, anchored by bold group mean
trajectories with standard error bars (or 95% CI ribbons), distinct
markers across recovery time points, and sample-size annotations.

## Usage

``` r
type_trajectory(
  patient_id = NULL,
  time = NULL,
  score = NULL,
  group = NULL,
  severity_bands = list(Mild = c(0, 25), Moderate = c(25, 50), Severe = c(50, 100)),
  severity_colors = c(Mild = "#D9EEF7", Moderate = "#FDE8D7", Severe = "#FADBD8"),
  spaghetti_alpha = 0.18,
  spaghetti_width = 0.35,
  spaghetti_color = "grey45",
  show_mean = TRUE,
  mean_color = "#005EB8",
  mean_width = 1.2,
  show_ci = FALSE,
  show_errorbar = TRUE,
  errorbar = c("se", "ci", "sd", "none"),
  errorbar_width = 0.14,
  errorbar_lwd = 0.85,
  point_shape = 21,
  point_size = 3.2,
  point_stroke = 1.1,
  point_fill = "white",
  dodge = 0.15,
  show_n = TRUE,
  xlab = "Study Visit / Time",
  ylab = "Clinical Severity Score"
)
```

## Arguments

- patient_id:

  Column name for individual patient/participant IDs.

- time:

  Column name for longitudinal visit/time values.

- score:

  Column name for clinical score/measurement.

- group:

  Optional column name for treatment arm or cohort stratification.

- severity_bands:

  Named list defining intervals for background severity shading (default
  `list(Mild = c(0, 25), Moderate = c(25, 50), Severe = c(50, 100))`).
  Set to `NULL` for clean background without bands.

- severity_colors:

  Named vector of fill colors for severity bands (default
  `c(Mild = "#D9EEF7", Moderate = "#FDE8D7", Severe = "#FADBD8")`).

- spaghetti_alpha:

  Opacity of individual patient trajectories (default 0.18).

- spaghetti_width:

  Line width of individual trajectories (default 0.35).

- spaghetti_color:

  Color of individual trajectories (default "grey45").

- show_mean:

  Overlay bold group mean trajectory (default TRUE).

- mean_color:

  Color of group summary line when unstratified (default "#005EB8").

- mean_width:

  Line width of group summary line (default 1.2).

- show_ci:

  Add standard error or 95% CI ribbon around mean (default FALSE when
  errorbar is shown).

- show_errorbar:

  Display vertical error bars with crossbar caps (default TRUE).

- errorbar:

  Type of error bar to compute: `"se"` (standard error of the mean),
  `"ci"` (Student's t / 95% CI), `"sd"` (standard deviation), or
  `"none"`.

- errorbar_width:

  Width of the horizontal error bar cap (default 0.14).

- errorbar_lwd:

  Line width of the error bar stroke (default 0.85).

- point_shape:

  Shape of the mean glyph (default 21, filled circle with colored
  border).

- point_size:

  Size of the mean glyph (default 3.2).

- point_stroke:

  Stroke width of the mean glyph border (default 1.1).

- point_fill:

  Interior fill of the mean glyph (`"white"` for crisp journal look, or
  `"color"`).

- dodge:

  Horizontal dodging distance for stratified comparison arms (default
  0.15).

- show_n:

  Display visit sample sizes (\$n = ...\$) above plot (default TRUE).

- xlab:

  Axis label for time/visit (default "Study Visit / Time").

- ylab:

  Axis label for clinical score (default "Clinical Severity Score").

## Value

A cliplot_type object for use with
[`cliplot`](https://vanhungtran.github.io/cliomicplot/reference/cliplot.md).

## Examples

``` r
if (FALSE) { # \dontrun{
set.seed(42)
n_pts = 30
visits = c("BL", "M3", "M6", "M12")
traj_df = expand.grid(Patient = paste0("Pt_", 1:n_pts), Visit = visits)
traj_df$Score = pmax(0, pmin(80, rnorm(nrow(traj_df), mean = 40, sd = 15)))

cliplot(Score ~ Visit | Patient, data = traj_df, type = "trajectory")
} # }
```
