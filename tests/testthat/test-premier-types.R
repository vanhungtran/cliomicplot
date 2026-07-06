# ===========================================================================
# Premier type tests: raincloud, dumbbell, lollipop, beeswarm, radar, waffle
# ===========================================================================

# ---- raincloud ----

test_that("type_raincloud works", {
  p <- cliplot(Sepal.Length ~ Species, data = iris, type = "raincloud")
  expect_s3_class(p, "ggplot")
})

test_that("type_raincloud works with by grouping", {
  iris2 = iris; iris2$petal_cat = ifelse(iris$Petal.Width > 1.5, "wide", "narrow")
  p <- cliplot(Sepal.Length ~ Species, data = iris2,
               by = iris2$petal_cat, type = "raincloud")
  expect_s3_class(p, "ggplot")
})

# ---- dumbbell ----

test_that("type_dumbbell works", {
  db_df <- data.frame(
    country = c("USA", "China", "Japan", "Germany", "UK"),
    before  = c(80, 70, 78, 75, 72),
    after   = c(85, 75, 80, 78, 76)
  )
  p <- cliplot(after ~ country, data = db_df, type = "dumbbell")
  expect_s3_class(p, "ggplot")
})

# ---- lollipop ----

test_that("type_lollipop works", {
  ll_df <- data.frame(
    item = c("Alpha", "Beta", "Gamma", "Delta", "Epsilon"),
    value = c(85, 72, 60, 45, 30)
  )
  p <- cliplot(value ~ item, data = ll_df, type = "lollipop")
  expect_s3_class(p, "ggplot")
})

# ---- beeswarm ----

test_that("type_beeswarm works", {
  p <- cliplot(Sepal.Length ~ Species, data = iris, type = "beeswarm")
  expect_s3_class(p, "ggplot")
})

# ---- radar ----

test_that("type_radar works", {
  radar_df <- data.frame(
    Group = c("Drug A", "Drug B", "Placebo"),
    Efficacy = c(85, 72, 15),
    Safety = c(70, 90, 95),
    Tolerability = c(75, 80, 60),
    Cost = c(60, 45, 85)
  )
  p <- cliplot(radar_df, type = "radar")
  expect_s3_class(p, "ggplot")
})

# ---- waffle ----

test_that("type_waffle works", {
  wf_df <- data.frame(
    Category = c("CR", "PR", "SD", "PD"),
    Count = c(7, 30, 36, 27)
  )
  p <- cliplot(Count ~ Category, data = wf_df, type = "waffle")
  expect_s3_class(p, "ggplot")
})

# ---- interactive ----

test_that("cliplot_interactive works", {
  skip_if_not_installed("plotly")
  p <- cliplot(mpg ~ wt, data = mtcars, type = "points")
  ip <- cliplot_interactive(p)
  expect_s3_class(ip, "plotly")
})
