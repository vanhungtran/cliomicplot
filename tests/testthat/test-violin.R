test_that("type_violin works with default inner boxplot and jitter", {
  p <- cliplot(len ~ dose, data = ToothGrowth, type = "violin")
  expect_s3_class(p, "ggplot")
  layers <- p$layers
  expect_true(any(vapply(layers, function(l) inherits(l$geom, "GeomViolin"), logical(1))))
  expect_true(any(vapply(layers, function(l) inherits(l$geom, "GeomBoxplot"), logical(1))))
  expect_true(any(vapply(layers, function(l) inherits(l$geom, "GeomPoint"), logical(1))))
})

test_that("type_violin supports inner = quartiles", {
  p <- cliplot(len ~ dose, data = ToothGrowth,
               type = type_violin(inner = "quartiles", add_jitter = FALSE))
  expect_s3_class(p, "ggplot")
  layers <- p$layers
  expect_true(any(vapply(layers, function(l) inherits(l$geom, "GeomViolin"), logical(1))))
})

test_that("type_violin supports inner = mean_se and inner = median_iqr", {
  p_mean <- cliplot(len ~ dose, data = ToothGrowth,
                    type = type_violin(inner = "mean_se", add_jitter = FALSE))
  expect_s3_class(p_mean, "ggplot")

  p_iqr <- cliplot(len ~ dose, data = ToothGrowth,
                   type = type_violin(inner = "median_iqr", add_jitter = FALSE))
  expect_s3_class(p_iqr, "ggplot")
})

test_that("type_split_violin works with 2-level grouping variable", {
  p <- cliplot(len ~ dose | supp, data = ToothGrowth,
               type = type_split_violin(inner = "quartiles"))
  expect_s3_class(p, "ggplot")
  layers <- p$layers
  expect_true(any(vapply(layers, function(l) inherits(l$geom, "GeomSplitViolin"), logical(1))))
})

test_that("type_split_violin supports inner = boxplot and inner = mean_se", {
  p_box <- cliplot(len ~ dose | supp, data = ToothGrowth,
                   type = type_split_violin(inner = "boxplot"))
  expect_s3_class(p_box, "ggplot")
  expect_true(any(vapply(p_box$layers, function(l) inherits(l$geom, "GeomBoxplot"), logical(1))))

  p_mean <- cliplot(len ~ dose | supp, data = ToothGrowth,
                    type = type_split_violin(inner = "mean_se"))
  expect_s3_class(p_mean, "ggplot")
})

test_that("type_violin(split = TRUE) delegates to split violin", {
  p <- cliplot(len ~ dose | supp, data = ToothGrowth,
               type = type_violin(split = TRUE, inner = "quartiles"))
  expect_s3_class(p, "ggplot")
  expect_true(any(vapply(p$layers, function(l) inherits(l$geom, "GeomSplitViolin"), logical(1))))
})

test_that("type_violin supports orientation = horizontal and show_n = TRUE", {
  p_horiz <- cliplot(len ~ dose, data = ToothGrowth,
                     type = type_violin(orientation = "horizontal"))
  expect_s3_class(p_horiz, "ggplot")
  expect_s3_class(p_horiz$coordinates, "CoordFlip")

  p_n <- cliplot(len ~ dose, data = ToothGrowth,
                 type = type_violin(show_n = TRUE))
  expect_s3_class(p_n, "ggplot")
  expect_true(any(vapply(p_n$layers, function(l) inherits(l$geom, "GeomText"), logical(1))))
})
