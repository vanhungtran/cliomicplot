test_that("type_ecology_raincloud creates valid cliplot_type", {
  te <- type_ecology_raincloud()
  expect_s3_class(te, "cliplot_type")
  expect_equal(te$name, "ecology_raincloud")

  thb <- type_half_boxed()
  expect_s3_class(thb, "cliplot_type")
})

test_that("cliplot renders ecology raincloud plot", {
  set.seed(42)
  df <- data.frame(
    years = factor(rep(c("2 yr", "5 yr", "10 yr"), each = 40), levels = c("2 yr", "5 yr", "10 yr")),
    growth_rate = c(rnorm(40, 0.869, 0.05), rnorm(40, 0.713, 0.04), rnorm(40, 0.727, 0.045))
  )

  p <- cliplot(
    growth_rate ~ years,
    data = df,
    type = type_ecology_raincloud(
      ref_line = 1.0,
      show_stats = TRUE,
      stat_param = "\u03bb"
    ),
    palette = "ecology",
    theme = "ecology",
    ylab = expression(paste("population growth rate (", lambda, ")")),
    xlab = "Reseeding years"
  )

  expect_s3_class(p, "ggplot")
  b <- ggplot2::ggplot_build(p)
  expect_true(length(b$data) >= 4) # ref_line, halfeye/poly, boxplot, mean_point, stats_text
})

test_that("cliplot resolves type = 'ecology' string alias", {
  df <- data.frame(
    grp = factor(rep(c("A", "B"), each = 25)),
    val = rnorm(50, 10, 2)
  )

  p <- cliplot(
    val ~ grp,
    data = df,
    type = "ecology",
    palette = "reseeding",
    theme = "reseeding"
  )

  expect_s3_class(p, "ggplot")
})

test_that("ecology and reseeding palettes and theme are registered", {
  pal <- get_cli_palette("ecology", 3)
  expect_equal(length(pal), 3)
  expect_equal(pal[1], "#006700")
  expect_equal(pal[2], "#9497D0")
  expect_equal(pal[3], "#CE6A39")

  thm <- theme_cli_ecology()
  expect_s3_class(thm, "theme")
})
