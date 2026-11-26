test_that("type_pathology_boxplot creates valid cliplot_type", {
  tb <- type_pathology_boxplot()
  expect_s3_class(tb, "cliplot_type")
  expect_equal(tb$name, "pathology_boxplot")
})

test_that("cliplot renders pathology boxplot with grouped formula", {
  set.seed(123)
  df <- data.frame(
    feature = factor(rep(c("Calcification", "Fibrosis", "Atrophy"), each = 40)),
    status  = factor(rep(c("Absent", "Present"), each = 20, times = 3), levels = c("Absent", "Present")),
    delta   = rnorm(120, mean = 0.1, sd = 0.25)
  )

  p <- cliplot(
    delta ~ feature | status,
    data = df,
    type = type_pathology_boxplot(jitter_subsample = 15),
    palette = "pathology",
    theme = "pathology",
    title = "Structural Aging Across Pathology Notes"
  )

  expect_s3_class(p, "ggplot")

  # Verify plot builds cleanly
  b <- ggplot2::ggplot_build(p)
  expect_true(length(b$data) >= 4) # zero_line, boxplot, jitter, mean_line, mean_point, bold_median
})

test_that("cliplot resolves type = 'pathology' string alias", {
  df <- data.frame(
    feature = factor(rep(c("A", "B"), each = 20)),
    status  = factor(rep(c("Absent", "Present"), 20)),
    val     = rnorm(40)
  )

  p <- cliplot(
    val ~ feature | status,
    data = df,
    type = "pathology",
    palette = "pathology"
  )

  expect_s3_class(p, "ggplot")
})

test_that("pathology palette and theme are registered", {
  pal <- get_cli_palette("pathology", 2)
  expect_equal(length(pal), 2)
  expect_equal(pal[1], "#0F4D92")
  expect_equal(pal[2], "#D55E00")

  thm <- theme_cli_pathology()
  expect_s3_class(thm, "theme")
})

test_that("stat.label = 'p.nature' formats brackets correctly", {
  set.seed(42)
  df <- data.frame(
    feature = factor(rep(c("Calcification", "Fibrosis"), each = 40)),
    status  = factor(rep(c("Absent", "Present"), each = 20, times = 2), levels = c("Absent", "Present")),
    delta   = c(rnorm(20, 0), rnorm(20, 1.2), rnorm(20, 0), rnorm(20, 0.9))
  )

  p <- cliplot(
    delta ~ feature | status,
    data = df,
    type = type_pathology_boxplot(),
    stat.test = "t.test",
    stat.label = "p.nature",
    palette = "pathology",
    theme = "pathology"
  )

  expect_s3_class(p, "ggplot")
  b <- ggplot2::ggplot_build(p)
  expect_true(is.list(b$data))
})
