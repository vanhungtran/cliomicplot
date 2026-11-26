# Longitudinal Clinical Trajectory Plot Type

Creates publication-ready longitudinal clinical trajectories (Clinical
Archetype 4). Overlays semi-transparent individual patient spaghetti
trajectories onto shaded background clinical severity bands (e.g., Mild,
Moderate, Severe), anchored by a bold group mean trajectory with
standard error or 95% CI ribbons and visit sample-size annotations.

## Usage

``` r
type_trajectory(
  patient_id = NULL,
  time = NULL,
  score = NULL,
  severity_bands = list(Mild = c(0, 25), Moderate = c(25, 50), Severe = c(50, 100)),
  severity_colors = c(Mild = "#D9EEF7", Moderate = "#FDE8D7", Severe = "#FADBD8"),
  spaghetti_alpha = 0.18,
  spaghetti_width = 0.35,
  spaghetti_color = "grey45",
  show_mean = TRUE,
  mean_color = "#005EB8",
  mean_width = 1.2,
  show_ci = TRUE,
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

- severity_bands:

  Named list defining intervals for background severity shading (default
  `list(Mild = c(0, 25), Moderate = c(25, 50), Severe = c(50, 100))`).

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

  Color of group summary line (default "#005EB8").

- mean_width:

  Line width of group summary line (default 1.2).

- show_ci:

  Add standard error or 95% CI ribbon around mean (default TRUE).

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
