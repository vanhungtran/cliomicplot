# ===========================================================================
# Clinical & omics type tests: boxplot, violin, volcano, PCA, MA, correlation
# forest, KM, waterfall, swimmer, heatmap
# ===========================================================================

# ---- boxplot ----

test_that("type_boxplot works without grouping", {
  p <- cliplot(Sepal.Length ~ 1, data = iris, type = "boxplot")
  expect_s3_class(p, "ggplot")
})

test_that("type_boxplot works with grouping", {
  p <- cliplot(Sepal.Length ~ Species, data = iris, type = "boxplot")
  expect_s3_class(p, "ggplot")
})

# ---- violin ----

test_that("type_violin works with grouping", {
  p <- cliplot(Sepal.Length ~ Species, data = iris, type = "violin")
  expect_s3_class(p, "ggplot")
})

# ---- PCA ----

test_that("type_pca works from data frame", {
  p <- cliplot(iris[, 1:4], type = "pca", by = iris$Species)
  expect_s3_class(p, "ggplot")
})

test_that("type_pca works with ellipses", {
  p <- cliplot(iris[, 1:4],
               type = type_pca(add_ellipse = TRUE, ellipse_level = 0.95),
               by = iris$Species)
  expect_s3_class(p, "ggplot")
})

test_that("type_pca works with sample labels", {
  p <- cliplot(iris[, 1:4],
               type = type_pca(add_ellipse = FALSE, label_samples = TRUE),
               by = iris$Species)
  expect_s3_class(p, "ggplot")
})

test_that("type_pca works from pre-computed scores", {
  pca_res <- prcomp(iris[, 1:4], center = TRUE, scale. = TRUE)
  scores  <- as.data.frame(pca_res$x)
  scores$Species <- iris$Species
  p <- cliplot(PC2 ~ PC1, data = scores, by = scores$Species, type = "points")
  expect_s3_class(p, "ggplot")
})

# ---- volcano ----

test_that("type_volcano works", {
  set.seed(42)
  de_data <- data.frame(
    logFC  = rnorm(1000, 0, 0.5),
    PValue = 10^-runif(1000, 0, 8)
  )
  p <- cliplot(-log10(PValue) ~ logFC, data = de_data, type = "volcano")
  expect_s3_class(p, "ggplot")
})

test_that("type_volcano works with custom cutoffs", {
  set.seed(42)
  de_data <- data.frame(
    logFC  = rnorm(500, 0, 0.5),
    PValue = 10^-runif(500, 0, 8)
  )
  p <- cliplot(-log10(PValue) ~ logFC, data = de_data,
               type = type_volcano(pval_cutoff = 0.001, fc_cutoff = 1.5))
  expect_s3_class(p, "ggplot")
})

# ---- MA plot ----

test_that("type_ma works", {
  set.seed(123)
  ma_data <- data.frame(
    baseMean       = 10^runif(500, 0, 5),
    log2FoldChange = rnorm(500, 0, 0.5),
    padj           = runif(500)
  )
  p <- cliplot(log2FoldChange ~ baseMean, data = ma_data, type = "ma")
  expect_s3_class(p, "ggplot")
})

# ---- correlation ----

test_that("type_correlation works", {
  p <- cliplot(mtcars[, 1:5], type = "correlation")
  expect_s3_class(p, "ggplot")
})

test_that("type_correlation works with lower triangle", {
  p <- cliplot(mtcars[, 1:5],
               type = type_correlation(type = "lower", add_coef = TRUE))
  expect_s3_class(p, "ggplot")
})

# ---- forest ----

test_that("type_forest works", {
  forest_data <- data.frame(
    Variable = c("Age", "Sex", "Stage"),
    HR = c(1.5, 2.0, 3.5),
    CI_low = c(1.1, 1.3, 2.0),
    CI_high = c(2.0, 3.0, 6.0),
    P = c(0.01, 0.003, 0.5)
  )
  p <- cliplot(HR ~ Variable, data = forest_data, type = "forest")
  expect_s3_class(p, "ggplot")
})

# ---- KM ----

test_that("type_km works with survival data", {
  skip_if_not_installed("survival")
  library(survival)
  lung <- survival::lung
  p <- cliplot(Surv(time, status) ~ sex, data = lung, type = "km")
  expect_true(is(p, "ggplot") || is(p, "patchwork") || is(p, "ggsurvplot") ||
              is(p, "gtable") || is.list(p))
})

# ---- waterfall ----

test_that("type_waterfall works", {
  set.seed(42)
  n_pts <- 20
  imtrial <- data.frame(
    Patient    = paste0("P-", sprintf("%03d", 1:n_pts)),
    BestChange = sort(rnorm(n_pts, mean = -15, sd = 25)),
    BestResp   = sample(c("CR", "PR", "SD", "PD"), n_pts, replace = TRUE),
    stringsAsFactors = FALSE
  )
  p <- cliplot(BestChange ~ Patient, data = imtrial, type = "waterfall")
  expect_s3_class(p, "ggplot")
})

# ---- swimmer ----

test_that("type_swimmer works", {
  set.seed(42)
  n_pts <- 15
  swim_data <- data.frame(
    Patient  = paste0("P-", sprintf("%03d", 1:n_pts)),
    Duration = pmax(1, round(rnorm(n_pts, mean = 12, sd = 5), 1)),
    BestResp = sample(c("CR", "PR", "SD", "PD"), n_pts, replace = TRUE),
    stringsAsFactors = FALSE
  )
  p <- cliplot(Duration ~ Patient, data = swim_data, type = "swimmer")
  expect_s3_class(p, "ggplot")
})

# ---- heatmap ----

test_that("type_heatmap works with matrix", {
  mat <- matrix(rnorm(100), nrow = 10)
  rownames(mat) <- paste0("Gene", 1:10)
  colnames(mat) <- paste0("Sample", 1:10)
  p <- cliplot(mat, type = "heatmap")
  expect_true(is(p, "ggplot") || is(p, "gtable") || is.list(p))
})

# ---- Clinical Archetype 1: MCID Dumbbell ----

test_that("type_mcid works (Archetype 1)", {
  mcid_df <- data.frame(
    Endpoint = c("EASI", "DLQI", "SCORAD", "POEM"),
    Estimate = c(9.2, 5.8, 4.1, 1.2),
    MCID     = c(6.6, 4.0, 8.7, 3.4),
    N        = c(420, 395, 210, 150),
    Category = c("SoC Exceeding", "SoC Exceeding", "SoC Below", "Concomitant Below")
  )
  p <- cliplot(Estimate ~ Endpoint, data = mcid_df, type = type_mcid(mcid = "MCID", n_treated = "N"))
  expect_s3_class(p, "ggplot")

  # Via string type
  p2 <- cliplot(Estimate ~ Endpoint, data = mcid_df, type = "mcid")
  expect_s3_class(p2, "ggplot")
})

# ---- Clinical Archetype 4: Longitudinal Trajectory ----

test_that("type_trajectory works with severity bands (Archetype 4)", {
  set.seed(42)
  n_pts <- 10
  visits <- c("BL", "M3", "M6", "M12")
  traj_df <- expand.grid(Patient = paste0("Pt_", 1:n_pts), Visit = visits)
  traj_df$Score <- pmax(0, pmin(80, rnorm(nrow(traj_df), mean = 40, sd = 15)))

  p <- cliplot(Score ~ Visit | Patient, data = traj_df, type = "trajectory")
  expect_s3_class(p, "ggplot")

  # With custom bands and string alias
  p2 <- cliplot(Score ~ Visit | Patient, data = traj_df,
                type = type_trajectory(show_mean = TRUE, show_ci = TRUE, show_n = TRUE))
  expect_s3_class(p2, "ggplot")
})

# ---- Clinical Archetype 5: KM Cumulative Incidence & Milestones ----

test_that("type_km works with cumulative incidence and landmark milestones", {
  skip_if_not_installed("survival")
  lung <- survival::lung

  # Cumulative incidence with landmark milestones
  p_cuminc <- cliplot(Surv(time, status) ~ sex, data = lung,
                      type = type_km(cumulative_incidence = TRUE, milestones = c(180, 365)))
  expect_true(is(p_cuminc, "ggplot") || is(p_cuminc, "patchwork"))

  # Survival with median drop lines and milestones
  p_surv <- cliplot(Surv(time, status) ~ sex, data = lung,
                    type = type_km(median_line = TRUE, conf_int = TRUE, milestones = c(365, 730)))
  expect_true(is(p_surv, "ggplot") || is(p_surv, "patchwork"))
})

# ---- Clinical Archetype 13: Swimmer with Ongoing Indicator ----

test_that("type_swimmer works with ongoing indicator", {
  set.seed(42)
  n_pts <- 12
  swim_df <- data.frame(
    Patient  = paste0("P-", sprintf("%03d", 1:n_pts)),
    Duration = pmax(2, round(rnorm(n_pts, mean = 14, sd = 4), 1)),
    BestResp = sample(c("CR", "PR", "SD", "PD"), n_pts, replace = TRUE),
    Ongoing  = sample(c(TRUE, FALSE), n_pts, replace = TRUE),
    stringsAsFactors = FALSE
  )
  events <- data.frame(
    ID    = sample(swim_df$Patient, 15, replace = TRUE),
    Time  = round(runif(15, 1, 12), 1),
    Event = sample(c("Progression", "AE", "Dose Reduction"), 15, replace = TRUE)
  )

  p <- cliplot(Duration ~ Patient, data = swim_df,
               type = type_swimmer(bar_fill = "BestResp", ongoing = "Ongoing",
                                   event_times = events, show_duration = TRUE))
  expect_s3_class(p, "ggplot")
})

# ---- Clinical Archetype 9: Forest Plot with Alternating Stripes & Diamonds ----

test_that("type_forest works with alternating stripes and summary diamond", {
  f_df <- data.frame(
    Variable = c("Trial A", "Trial B", "Trial C", "Pooled Meta-Analysis"),
    HR = c(0.72, 0.65, 0.81, 0.71),
    CI_low = c(0.55, 0.48, 0.62, 0.59),
    CI_high = c(0.94, 0.88, 1.05, 0.85),
    P = c(0.015, 0.006, 0.11, 0.0003),
    is_summary = c(FALSE, FALSE, FALSE, TRUE)
  )

  p <- cliplot(HR ~ Variable, data = f_df,
               type = type_forest(stripe = TRUE, diamond = TRUE, show_values = TRUE))
  expect_s3_class(p, "ggplot")
})

# ---- Journal Themes & Presets ----

test_that("theme_cli_scifig and clitheme('scifig') work", {
  p <- ggplot2::ggplot(mtcars, ggplot2::aes(wt, mpg)) +
    ggplot2::geom_point() +
    theme_cli_scifig()
  expect_s3_class(p, "ggplot")

  # Via clitheme
  clitheme("scifig")
  p2 <- cliplot(mpg ~ wt, data = mtcars)
  expect_s3_class(p2, "ggplot")
  clitheme() # Reset
})

test_that("cli_journal_size supports 9 journal publisher presets", {
  journals <- c("nature", "natcomms", "science", "cell", "jama", "nejm", "lancet", "plos", "ieee")
  for (j in journals) {
    sz <- cli_journal_size(j, "single")
    expect_true(sz$width_mm > 50 && sz$width_mm < 100)
    expect_true(sz$width_in > 0)
    expect_true(sz$height_in > 0)
    expect_equal(sz$journal, j)
  }
  # Aliases
  expect_equal(cli_journal_size("nature communications", "double")$journal, "natcomms")
  expect_equal(cli_journal_size("plos one", "single")$journal, "plos")
})

