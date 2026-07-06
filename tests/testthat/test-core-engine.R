# ===========================================================================
# Core engine tests: cliplot S3 dispatch, formula parsing, data building
# ===========================================================================

# ---- S3 dispatch ----

test_that("cliplot generic dispatches correctly", {
  # Formula
  expect_s3_class(cliplot(Sepal.Length ~ Petal.Width, data = iris), "ggplot")

  # Data frame with explicit type
  p <- cliplot(mtcars[, 1:2], type = "points")
  expect_s3_class(p, "ggplot")

  # Matrix
  mat <- as.matrix(mtcars[, 1:4])
  p <- cliplot(mat, type = "heatmap")
  expect_true(is(p, "ggplot") || is(p, "gtable") ||
              inherits(p, "Heatmap") || inherits(p, "HeatmapList"))
})

# ---- Formula parsing ----

test_that("formula parsing handles one-sided formulas", {
  p <- cliplot(~ mpg, data = mtcars)
  expect_s3_class(p, "ggplot")
})

test_that("formula parsing handles two-sided formulas", {
  p <- cliplot(mpg ~ wt, data = mtcars)
  expect_s3_class(p, "ggplot")
})

test_that("formula parsing handles group-by pipe", {
  p <- cliplot(Sepal.Length ~ Petal.Width | Species, data = iris)
  expect_s3_class(p, "ggplot")
})

# ---- NULL coalescing ----

test_that("%||% operator works correctly", {
  expect_equal(NULL %||% 5, 5)
  expect_equal(3 %||% 5, 3)
  expect_equal(FALSE %||% TRUE, FALSE)
  expect_equal("" %||% "fallback", "")
})

# ---- build_plot_data ----

test_that("build_plot_data handles basic x/y", {
  s <- new.env(parent = emptyenv())
  s$x <- 1:10
  s$y <- 10:1
  s$by <- rep(c("A", "B"), 5)
  pd <- cliomicplot:::build_plot_data(s)
  expect_s3_class(pd, "data.frame")
  expect_equal(nrow(pd), 10)
  expect_true("x" %in% names(pd))
  expect_true("y" %in% names(pd))
  expect_true("..by.." %in% names(pd))
})

test_that("build_plot_data handles NULL inputs", {
  s <- new.env(parent = emptyenv())
  s$x <- NULL
  s$y <- NULL
  s$by <- NULL
  pd <- cliomicplot:::build_plot_data(s)
  expect_s3_class(pd, "data.frame")
  expect_equal(ncol(pd), 0)
})

# ---- build_mapping ----

test_that("build_mapping constructs correct aes", {
  df <- data.frame(x = 1:5, y = 6:10)
  m <- cliomicplot:::build_mapping(NULL, df)
  expect_s3_class(m, "uneval")
  expect_true("x" %in% names(m))
  expect_true("y" %in% names(m))
})

# ---- resolve_type ----

test_that("resolve_type maps strings to types", {
  t <- cliomicplot:::resolve_type("points")
  expect_s3_class(t, "cliplot_type")
  expect_equal(t$name, "points")

  t <- cliomicplot:::resolve_type("boxplot")
  expect_s3_class(t, "cliplot_type")
  expect_equal(t$name, "boxplot")
})

test_that("resolve_type passes through cliplot_type objects", {
  tp <- type_points(size = 10)
  t <- cliomicplot:::resolve_type(tp)
  expect_s3_class(t, "cliplot_type")
  expect_equal(t$name, "points")
})

test_that("resolve_type falls back to points for unknown types", {
  t <- cliomicplot:::resolve_type("nonexistent_type_xyz")
  expect_s3_class(t, "cliplot_type")
  expect_equal(t$name, "points")
})

# ---- auto-detection ----

test_that("default_type detects numeric-only as histogram", {
  expect_equal(cliomicplot:::default_type(1:100, NULL), "histogram")
})

test_that("default_type detects factor-only as barplot", {
  expect_equal(cliomicplot:::default_type(factor(letters[1:5]), NULL), "barplot")
})

test_that("default_type detects numeric pair as points", {
  expect_equal(cliomicplot:::default_type(1:100, 100:1), "points")
})

test_that("default_type detects factor-numeric as boxplot", {
  expect_equal(cliomicplot:::default_type(factor(rep(1:3, each=5)), rnorm(15)),
               "boxplot")
})

test_that("default_type detects many tied x-values as jitter", {
  x <- rep(1:5, each = 20)
  y <- rnorm(100)
  expect_equal(cliomicplot:::default_type(x, y), "jitter")
})

# ---- apply_palette ----

test_that("apply_palette adds color scales", {
  s <- new.env(parent = emptyenv())
  s$by <- factor(rep(c("A", "B"), each = 5))
  s$palette <- "jco"
  p <- ggplot2::ggplot(data.frame(x = 1:10, y = 1:10)) +
    ggplot2::geom_point(ggplot2::aes(x = x, y = y))
  p2 <- cliomicplot:::apply_palette(p, s)
  expect_s3_class(p2, "ggplot")
})
