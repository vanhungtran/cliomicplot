# ===========================================================================
# Unit Tests for Immune Ageing & pQTL Architectures
# type_mr_coloc, type_composition, and type_slope reference lines
# ===========================================================================

test_that("data(pqtl_mr_coloc) loads and has correct dimensions", {
  data("pqtl_mr_coloc", package = "cliomicplot")
  expect_equal(nrow(pqtl_mr_coloc), 323)
  expect_true(all(c("symbol", "label_short", "b_mr", "se_mr", "p_mr", "or_per_sd", "PP_H4", "PP_H3") %in% names(pqtl_mr_coloc)))
})

test_that("data(immune_cell_composition) loads and has correct dimensions", {
  data("immune_cell_composition", package = "cliomicplot")
  expect_equal(nrow(immune_cell_composition), 630)
  expect_true(all(c("gsm", "dataset", "group", "lineage", "share") %in% names(immune_cell_composition)))
})

test_that("type_mr_coloc produces valid ggplot with borders and colocalization", {
  data("pqtl_mr_coloc", package = "cliomicplot")
  p <- cliplot(
    symbol ~ label_short,
    data = pqtl_mr_coloc,
    type = type_mr_coloc(
      set_col = "set_label",
      family_col = "family_label",
      p_threshold = 0.05,
      bonferroni = TRUE,
      coloc_threshold = 0.80
    ),
    title = "Test MR Coloc"
  )
  expect_s3_class(p, "ggplot")
  b <- ggplot2::ggplot_build(p)
  expect_true(length(b$data) >= 1)
})

test_that("type_mr_coloc works with string type 'mr_coloc'", {
  data("pqtl_mr_coloc", package = "cliomicplot")
  p <- cliplot(
    symbol ~ label_short,
    data = pqtl_mr_coloc,
    type = "mr_coloc"
  )
  expect_s3_class(p, "ggplot")
})

test_that("type_composition produces valid stacked bars with clinical track", {
  data("immune_cell_composition", package = "cliomicplot")
  sub_df <- immune_cell_composition[immune_cell_composition$dataset == "GSE183953", ]
  p <- cliplot(
    share ~ gsm,
    data = sub_df,
    type = type_composition(
      lineage_col = "lineage",
      track_col = "group"
    ),
    title = "Test Composition"
  )
  expect_s3_class(p, "ggplot")
  b <- ggplot2::ggplot_build(p)
  expect_true(length(b$data) >= 2)
})

test_that("type_slope supports reference_line", {
  paired_df <- data.frame(
    id = rep(paste0("P", 1:4), each = 2),
    timepoint = rep(c("Before", "After"), 4),
    val = c(-1.2, -0.4, 0.8, -0.2, 1.5, 1.2, -0.5, -0.9)
  )
  p <- cliplot(
    val ~ timepoint,
    data = paired_df,
    type = type_slope(pair = "id", reference_line = 0)
  )
  expect_s3_class(p, "ggplot")
  b <- ggplot2::ggplot_build(p)
  expect_equal(b$data[[1]]$yintercept[1], 0)
})
