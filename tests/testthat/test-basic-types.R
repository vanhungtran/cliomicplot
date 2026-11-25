# ===========================================================================
# Basic type tests: points, jitter, barplot, lines, histogram, density
# ===========================================================================

# ---- points ----

test_that("type_points works without grouping", {
  p <- cliplot(mpg ~ wt, data = mtcars, type = "points")
  expect_s3_class(p, "ggplot")
})

test_that("type_points works with grouping", {
  p <- cliplot(Sepal.Length ~ Petal.Width, data = iris,
               by = iris$Species, type = "points")
  expect_s3_class(p, "ggplot")
})

test_that("type_points accepts custom parameters", {
  p <- cliplot(mpg ~ wt, data = mtcars,
               type = type_points(alpha = 0.5, size = 5, stroke = 0.5))
  expect_s3_class(p, "ggplot")
})

# ---- jitter ----

test_that("type_jitter works without grouping", {
  p <- cliplot(mpg ~ factor(cyl), data = mtcars, type = "jitter")
  expect_s3_class(p, "ggplot")
})

test_that("type_jitter works with grouping", {
  p <- cliplot(mpg ~ factor(cyl), data = mtcars,
               by = mtcars$am, type = "jitter")
  expect_s3_class(p, "ggplot")
})

test_that("type_jitter accepts custom width/height", {
  p <- cliplot(mpg ~ factor(cyl), data = mtcars,
               type = type_jitter(width = 0.3, height = 0.1))
  expect_s3_class(p, "ggplot")
})

# ---- barplot ----

test_that("type_barplot works without grouping", {
  p <- cliplot(~ factor(cyl), data = mtcars, type = "barplot")
  expect_s3_class(p, "ggplot")
})

test_that("type_barplot works with grouping", {
  p <- cliplot(~ factor(cyl), data = mtcars,
               by = factor(mtcars$am), type = "barplot")
  expect_s3_class(p, "ggplot")
})

test_that("type_barplot supports identity stat", {
  df <- data.frame(cat = letters[1:5], val = 1:5)
  p <- cliplot(val ~ cat, data = df, type = type_barplot(stat = "identity"))
  expect_s3_class(p, "ggplot")
})

# ---- lines ----

test_that("type_lines works without grouping", {
  p <- cliplot(1:10 ~ 1:10, type = "lines")
  expect_s3_class(p, "ggplot")
})

test_that("type_lines works with grouping", {
  p <- cliplot(Sepal.Length ~ Petal.Width, data = iris,
               by = iris$Species, type = "lines")
  expect_s3_class(p, "ggplot")
})

test_that("type_lines supports individual lines via group argument", {
  df <- data.frame(
    Time = rep(c(1, 2, 3), 4),
    Score = rnorm(12),
    Group = rep(c("A", "B"), each = 6),
    Patient = rep(paste0("PT", 1:4), each = 3)
  )
  p <- cliplot(Score ~ Time | Group, data = df, group = "Patient",
               type = type_lines(linewidth = 1.1, add_points = TRUE))
  expect_s3_class(p, "ggplot")
})

test_that("type_lines auto-aggregates duplicate x values by mean and supports show_se", {
  df <- data.frame(
    Day = rep(c(1, 3, 7, 14), each = 3),
    Value = c(10, 11, 12, 8, 9, 7, 25, 28, 26, 4, 5, 4),
    Trt = rep(c("Sham", "Drug"), each = 6)
  )
  p <- cliplot(Value ~ Day | Trt, data = df,
               type = type_lines(linewidth = 1.2, show_se = TRUE, add_points = TRUE))
  expect_s3_class(p, "ggplot")
})

# ---- histogram ----

test_that("type_histogram works", {
  p <- cliplot(~ mpg, data = mtcars, type = "histogram")
  expect_s3_class(p, "ggplot")
})

test_that("type_histogram accepts bin count", {
  p <- cliplot(~ mpg, data = mtcars, type = type_histogram(bins = 20))
  expect_s3_class(p, "ggplot")
})

# ---- density ----

test_that("type_density works without grouping", {
  p <- cliplot(~ mpg, data = mtcars, type = "density")
  expect_s3_class(p, "ggplot")
})

test_that("type_density works with grouping", {
  p <- cliplot(~ Sepal.Length, data = iris,
               by = iris$Species, type = "density")
  expect_s3_class(p, "ggplot")
})

# ---- text ----

test_that("type_text works", {
  df <- head(mtcars, 5)
  p <- cliplot(mpg ~ wt, data = df, type = "text")
  expect_s3_class(p, "ggplot")
})

test_that("type_text with style='label' works", {
  df <- head(mtcars, 5)
  p <- cliplot(mpg ~ wt, data = df, type = type_text(style = "label"))
  expect_s3_class(p, "ggplot")
})

# ---- bubble ----

test_that("type_bubble works", {
  p <- cliplot(mpg ~ wt, data = mtcars, type = type_bubble(z = mtcars$hp))
  expect_s3_class(p, "ggplot")
})

test_that("type_bubble works with grouping", {
  p <- cliplot(Sepal.Length ~ Sepal.Width, data = iris,
               by = iris$Species, type = type_bubble(z = iris$Petal.Length))
  expect_s3_class(p, "ggplot")
})

# ---- errorbar & ribbon ----

test_that("type_errorbar works", {
  df <- data.frame(x = 1:5, y = 1:5, ymin = 0:4, ymax = 2:6)
  p <- cliplot(y ~ x, data = df, type = "errorbar", ymin = df$ymin, ymax = df$ymax)
  expect_s3_class(p, "ggplot")
})
