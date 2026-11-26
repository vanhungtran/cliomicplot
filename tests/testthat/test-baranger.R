test_that("type_scatter_box_violin creates a valid cliplot_type object", {
  sbv <- type_scatter_box_violin()
  expect_s3_class(sbv, "cliplot_type")
  expect_equal(sbv$name, "scatter_box_violin")

  # Aliases
  expect_equal(type_baranger()$name, "scatter_box_violin")
  expect_equal(type_sbv()$name, "scatter_box_violin")
})

test_that("cliplot renders Baranger scatter-box-violin on single factor", {
  p <- cliplot(
    Sepal.Length ~ Species,
    data = iris,
    type = "scatter_box_violin",
    palette = "nature",
    theme = "nature"
  )
  expect_s3_class(p, "ggplot")

  # Check that violin, boxplot, and point layers are present
  layer_classes <- vapply(p$layers, function(l) class(l$geom)[1], character(1))
  expect_true("GeomViolin" %in% layer_classes)
  expect_true("GeomBoxplot" %in% layer_classes)
  expect_true("GeomPoint" %in% layer_classes)
})

test_that("cliplot renders Baranger scatter-box-violin with dodged 2-way grouping", {
  tg <- ToothGrowth
  tg$dose <- factor(tg$dose)

  p_grouped <- cliplot(
    len ~ dose | supp,
    data = tg,
    type = type_scatter_box_violin(notch = FALSE),
    palette = "nature"
  )
  expect_s3_class(p_grouped, "ggplot")

  layer_classes <- vapply(p_grouped$layers, function(l) class(l$geom)[1], character(1))
  expect_true("GeomViolin" %in% layer_classes)
  expect_true("GeomBoxplot" %in% layer_classes)
  expect_true("GeomPoint" %in% layer_classes)
})

test_that("type_scatter_box and type_box_violin render correctly", {
  p_sb <- cliplot(
    Sepal.Length ~ Species,
    data = iris,
    type = "scatter_box"
  )
  expect_s3_class(p_sb, "ggplot")
  layer_sb <- vapply(p_sb$layers, function(l) class(l$geom)[1], character(1))
  expect_false("GeomViolin" %in% layer_sb)
  expect_true("GeomBoxplot" %in% layer_sb)
  expect_true("GeomPoint" %in% layer_sb)

  p_bv <- cliplot(
    Sepal.Length ~ Species,
    data = iris,
    type = "box_violin"
  )
  expect_s3_class(p_bv, "ggplot")
  layer_bv <- vapply(p_bv$layers, function(l) class(l$geom)[1], character(1))
  expect_true("GeomViolin" %in% layer_bv)
  expect_true("GeomBoxplot" %in% layer_bv)
  expect_false("GeomPoint" %in% layer_bv)
})
