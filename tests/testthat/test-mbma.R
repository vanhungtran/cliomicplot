test_that("type_mbma_trajectory creates valid ggplot", {
  tc_data <- data.frame(
    week = rep(seq(0, 16, by = 4), 2),
    rate = c(0, 5, 10, 13, 14, 0, 8, 18, 24, 26),
    ymin = c(0, 4, 8, 11, 12, 0, 6, 15, 20, 22),
    ymax = c(0, 6, 12, 15, 16, 0, 10, 21, 28, 30),
    tertile = rep(c("Mid", "High"), each = 5),
    n = rep(c(150, 200), each = 5)
  )

  p <- cliplot(
    rate ~ week,
    data = tc_data,
    by = tc_data$tertile,
    type = type_mbma_trajectory(
      plateau_week = 12,
      point_size_var = "n"
    )
  )
  expect_s3_class(p, "ggplot")
})

test_that("type_mbma_grid creates valid simulation heatmap", {
  grid_df <- expand.grid(
    week = c("Wk 8", "Wk 12", "Wk 16"),
    easi = c("EASI 20", "EASI 28", "EASI 36"),
    design = c("Monotherapy", "+TCS"),
    stringsAsFactors = FALSE
  )
  grid_df$rate <- runif(nrow(grid_df), 10, 40)
  grid_df$ci_margin <- runif(nrow(grid_df), 1.0, 3.5)

  p <- cliplot(
    easi ~ week,
    data = grid_df,
    by = grid_df$design,
    type = type_mbma_grid(
      ci_margin = "ci_margin",
      palette = "blues"
    )
  )
  expect_s3_class(p, "ggplot")
})

test_that("type_mbma_benchmark creates valid benchmark plot", {
  bench_df <- data.frame(
    phase = factor(rep(c("Phase 2", "Phase 3"), each = 8)),
    response = c(rnorm(8, 12, 3), rnorm(8, 14, 3)),
    sample_size = runif(16, 50, 400)
  )

  p <- cliplot(
    response ~ phase,
    data = bench_df,
    type = type_mbma_benchmark(
      model_mean = 13.5,
      model_ci_low = 11.8,
      model_ci_high = 15.3,
      point_size_var = "sample_size"
    )
  )
  expect_s3_class(p, "ggplot")
})
