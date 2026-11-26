test_that("type_scorecard creates valid ggplot", {
  sc_df <- data.frame(
    group   = c("Lipidomics", "Lipidomics", "snRNA-seq", "snRNA-seq"),
    what    = c("LPE species", "TAG unsaturation", "Hepatocyte DEGs", "Kupffer subclusters"),
    pub     = c("up", "up", "recovered", "5 clusters"),
    rep     = c("P = 0.041", "P = 0.022", "98% match", "one liver artifact"),
    verdict = c("reproduced", "reproduced", "reproduced", "partly")
  )

  p <- cliplot(
    verdict ~ what,
    data = sc_df,
    type = type_scorecard()
  )
  expect_s3_class(p, "ggplot")
})

test_that("type_binned_shift creates valid shift profile", {
  shift_df <- expand.grid(
    bin = c("3.5-5", "5-10", "10-20", "20-40", ">40"),
    group = c("Ctrl", "NP", "MP"),
    stringsAsFactors = FALSE
  )
  shift_df$fraction <- c(10, 25, 35, 20, 10,
                         12, 28, 32, 18, 10,
                         8, 20, 38, 22, 12)

  p_dist <- cliplot(
    fraction ~ bin,
    data = shift_df,
    by = shift_df$group,
    type = type_binned_shift(mode = "distribution")
  )
  expect_s3_class(p_dist, "ggplot")

  p_diff <- cliplot(
    fraction ~ bin,
    data = shift_df,
    by = shift_df$group,
    type = type_binned_shift(mode = "difference", ref_group = "Ctrl")
  )
  expect_s3_class(p_diff, "ggplot")
})
