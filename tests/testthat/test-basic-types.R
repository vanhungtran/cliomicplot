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
