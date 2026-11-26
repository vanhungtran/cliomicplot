# ===========================================================================
# Unit Tests: Benchmark & Attribution Plot Types
# ===========================================================================

test_that("theme_cli_benchmark and benchmark palettes work", {
  thm = theme_cli_benchmark()
  expect_s3_class(thm, "theme")

  # Registered palettes
  expect_true("figures4papers" %in% cli_palette_list())
  expect_true("tier" %in% cli_palette_list())
  expect_true("leakage" %in% cli_palette_list())
  expect_true("benchmark" %in% cli_palette_list())

  cols = get_cli_palette("figures4papers", 5)
  expect_equal(length(cols), 5)
  expect_equal(cols[1], "#0F4D92")
})

test_that("type_attribution with style = 'shapley' generates valid ggplot", {
  df = data.frame(
    mechanism = c("Label", "Prior Trials", "Attention", "Zero-fill", "Ungrouped CV"),
    shapley   = c(0.032, 0.020, 0.028, -0.008, 0.003),
    learner   = c("HGB", "HGB", "HGB", "HGB", "HGB")
  )

  p = cliplot(
    shapley ~ mechanism | learner,
    data = df,
    type = type_attribution(style = "shapley"),
    theme = "benchmark"
  )

  expect_s3_class(p, "ggplot")
  b = ggplot2::ggplot_build(p)
  expect_true(length(b$data) >= 1)
})

test_that("type_attribution with style = 'stepwise' generates valid ggplot", {
  df = data.frame(
    step = paste0("Step ", 0:4),
    auroc = c(0.99, 0.98, 0.95, 0.88, 0.86),
    auroc_lo = c(0.98, 0.97, 0.91, 0.80, 0.78),
    auroc_hi = c(0.998, 0.995, 0.98, 0.93, 0.92)
  )

  p = cliplot(
    auroc ~ step,
    data = df,
    type = type_attribution(style = "stepwise", reference_line = 0.5),
    theme = "benchmark"
  )

  expect_s3_class(p, "ggplot")
})

test_that("type_attribution with style = 'contrast' generates valid ggplot", {
  df = data.frame(
    mechanism = rep(c("L", "C", "A"), each = 2),
    delta = c(0.11, -0.001, 0.06, -0.003, 0.10, -0.006),
    design = rep(c("added to clean", "removed from leaky"), 3),
    learner = rep("hgb", 6)
  )

  p = cliplot(
    delta ~ mechanism | learner,
    data = df,
    type = type_attribution(style = "contrast"),
    theme = "benchmark"
  )

  expect_s3_class(p, "ggplot")
})

test_that("type_benchmark_bars works with error bars and permutation null band", {
  df = data.frame(
    model    = c("Biology", "Druggability", "B+D", "Attention", "Label"),
    auroc    = c(0.77, 0.77, 0.86, 0.91, 0.98),
    auroc_lo = c(0.69, 0.68, 0.78, 0.84, 0.94),
    auroc_hi = c(0.85, 0.84, 0.92, 0.97, 1.00)
  )

  p = cliplot(
    auroc ~ model,
    data = df,
    type = type_benchmark_bars(null_band = c(0.505, 0.549)),
    palette = "figures4papers",
    theme = "benchmark"
  )

  expect_s3_class(p, "ggplot")
  b = ggplot2::ggplot_build(p)
  expect_true(length(b$data) >= 2)
})

test_that("type_identifiability generates valid dual-axis deconvolution plot", {
  df = data.frame(
    cell_type = c("T_cell", "B_cell", "Macrophage", "Keratinocyte"),
    n_markers = c(13, 3, 42, 32),
    max_corr  = c(0.57, 0.90, 0.77, 0.46),
    status    = c("predicted", "predicted", "predicted", "zeroed"),
    note      = c("identifiable", "few markers + collinear", "collinear", "identifiable"),
    signature = c("AD", "AD", "AD", "AD")
  )

  p = cliplot(
    n_markers ~ cell_type | signature,
    data = df,
    type = type_identifiability()
  )

  expect_s3_class(p, "ggplot")
  b = ggplot2::ggplot_build(p)
  expect_true(length(b$data) >= 2)
})
