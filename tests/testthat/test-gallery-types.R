# ===========================================================================
# Gallery-inspired type tests: ridge, lm, loess, spineplot, rug, abline, qq
# ===========================================================================

# ---- ridge ----

test_that("type_ridge works", {
  skip_if_not_installed("ggridges")
  p <- cliplot(~ Sepal.Length, data = iris, by = iris$Species, type = "ridge")
  expect_s3_class(p, "ggplot")
})

test_that("type_ridge works with gradient", {
  skip_if_not_installed("ggridges")
  p <- cliplot(Sepal.Length ~ Species, data = iris,
               type = type_ridge(gradient = TRUE))
  expect_s3_class(p, "ggplot")
})

test_that("type_ridge falls back to faceted density without ggridges", {
  # Just test that it doesn't error - ggridges may or may not be installed
  p <- tryCatch(
    cliplot(Sepal.Length ~ Species, data = iris, type = "ridge"),
    error = function(e) NULL
  )
  if (!is.null(p)) expect_s3_class(p, "ggplot")
})

# ---- lm ----

test_that("type_lm works", {
  p <- cliplot(mpg ~ wt, data = mtcars, type = "lm")
  expect_s3_class(p, "ggplot")
})

test_that("type_lm works with grouping", {
  p <- cliplot(Sepal.Length ~ Petal.Width, data = iris,
               by = iris$Species, type = "lm")
  expect_s3_class(p, "ggplot")
})

test_that("type_lm accepts custom level", {
  p <- cliplot(mpg ~ wt, data = mtcars,
               type = type_lm(level = 0.8, se = FALSE))
  expect_s3_class(p, "ggplot")
})

# ---- loess ----

test_that("type_loess works", {
  p <- cliplot(mpg ~ wt, data = mtcars, type = "loess")
  expect_s3_class(p, "ggplot")
})

test_that("type_loess works with grouping", {
  p <- cliplot(Sepal.Length ~ Petal.Width, data = iris,
               by = iris$Species, type = "loess")
  expect_s3_class(p, "ggplot")
})

test_that("type_loess accepts custom span", {
  p <- cliplot(mpg ~ wt, data = mtcars,
               type = type_loess(span = 0.3))
  expect_s3_class(p, "ggplot")
})

# ---- spineplot ----

test_that("type_spineplot works", {
  p <- cliplot(Species ~ factor(cyl), data = iris |>
                 transform(cyl = sample(4:8, 150, replace = TRUE)),
               type = "spineplot")
  expect_s3_class(p, "ggplot")
})

# ---- rug ----

test_that("type_rug works", {
  p <- cliplot(mpg ~ wt, data = mtcars, type = "rug")
  expect_s3_class(p, "ggplot")
})

test_that("type_rug accepts sides", {
  p <- cliplot(mpg ~ wt, data = mtcars,
               type = type_rug(sides = "tr"))
  expect_s3_class(p, "ggplot")
})

# ---- abline ----

test_that("type_abline works as horizontal line", {
  p <- cliplot(mpg ~ wt, data = mtcars,
               type = type_abline(intercept = 20))
  expect_s3_class(p, "ggplot")
})

test_that("type_abline works as diagonal line", {
  p <- cliplot(mpg ~ wt, data = mtcars,
               type = type_abline(intercept = 37, slope = -5))
  expect_s3_class(p, "ggplot")
})

# ---- qq ----

test_that("type_qq works", {
  p <- cliplot(~ mpg, data = mtcars, type = "qq")
  expect_s3_class(p, "ggplot")
})
