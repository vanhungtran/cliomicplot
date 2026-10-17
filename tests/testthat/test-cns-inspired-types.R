# ===========================================================================
# cnsplots-inspired type tests: roc, gsea, upset, venn, slope, confusion,
# donut, and publication helpers
# ===========================================================================

# ---- roc ----

test_that("type_roc works via formula", {
  set.seed(42)
  df <- data.frame(
    outcome = factor(sample(c("yes", "no"), 80, TRUE, prob = c(0.4, 0.6))),
    score   = rnorm(80)
  )
  p <- cliplot(outcome ~ score, data = df, type = "roc")
  expect_s3_class(p, "ggplot")
})

test_that("type_roc compares multiple predictors", {
  set.seed(1)
  n <- 100
  df <- data.frame(
    disease = factor(sample(c(0, 1), n, TRUE, prob = c(0.6, 0.4))),
    marker_A = rnorm(n),
    marker_B = rnorm(n)
  )
  p <- cliplot(df, type = type_roc(predictors = c("marker_A", "marker_B")))
  expect_s3_class(p, "ggplot")
})

test_that("type_roc handles binary 0/1 numeric response", {
  set.seed(3)
  y <- rbinom(60, 1, 0.4)
  x <- y * 0.7 + rnorm(60)
  p <- cliplot(y ~ x, data = data.frame(x = x, y = y), type = "roc")
  expect_s3_class(p, "ggplot")
})

# ---- gsea ----

test_that("type_gsea works with a results data frame", {
  gsea_res <- data.frame(
    Term = c("Interferon Response", "DNA Repair", "Apoptosis", "Cell Cycle"),
    NES  = c(2.7, 2.3, -2.1, -1.8),
    FDR  = c(0.001, 0.006, 0.011, 0.019)
  )
  p <- cliplot(gsea_res, type = "gsea", theme = "nature")
  expect_s3_class(p, "ggplot")
})

test_that("type_gsea works via formula", {
  gsea_res <- data.frame(
    Term = c("A", "B", "C"),
    NES  = c(2.5, -1.5, 1.2),
    FDR  = c(0.01, 0.02, 0.05)
  )
  p <- cliplot(NES ~ Term, data = gsea_res, type = "gsea")
  expect_s3_class(p, "ggplot")
})

test_that("type_gsea respects top_term", {
  gsea_res <- data.frame(
    Term = paste0("T", 1:10),
    NES  = seq(2.8, 0.2, length.out = 10),
    FDR  = runif(10, 0.001, 0.05)
  )
  p <- cliplot(gsea_res, type = type_gsea(top_term = 4))
  expect_s3_class(p, "ggplot")
})

# ---- upset ----

test_that("type_upset works with a named list of sets", {
  sets <- list(
    A = c("a", "b", "c", "d"),
    B = c("c", "d", "e", "f"),
    C = c("b", "d", "f", "g")
  )
  p <- cliplot(sets, type = "upset")
  expect_s3_class(p, "ggplot")
})

test_that("type_upset works with a binary data frame", {
  df <- data.frame(
    row.names = paste0("g", 1:6),
    A = c(1, 1, 1, 0, 0, 0),
    B = c(0, 1, 1, 1, 0, 0),
    C = c(0, 0, 1, 1, 1, 1)
  )
  p <- cliplot(df, type = "upset")
  expect_s3_class(p, "ggplot")
})

test_that("type_upset supports max_rank and min_size", {
  sets <- list(
    A = paste0("g", 1:10),
    B = paste0("g", 4:14),
    C = paste0("g", 6:20)
  )
  p <- cliplot(sets, type = type_upset(max_rank = 3, min_size = 1))
  expect_s3_class(p, "ggplot")
})

# ---- venn ----

test_that("type_venn works for 2 and 3 sets", {
  s1 <- c("a", "b", "c", "d")
  s2 <- c("c", "d", "e", "f")
  s3 <- c("d", "e", "f", "g")
  p2 <- cliplot(list(A = s1, B = s2), type = "venn")
  expect_s3_class(p2, "ggplot")
  p3 <- cliplot(list(A = s1, B = s2, C = s3), type = "venn")
  expect_s3_class(p3, "ggplot")
})

test_that("type_venn rejects more than 3 sets", {
  sets <- list(A = 1:3, B = 2:4, C = 3:5, D = 4:6)
  expect_error(cliplot(sets, type = "venn"), "at most 3")
})

# ---- slope ----

test_that("type_slope works with paired data", {
  paired <- data.frame(
    subject   = rep(1:12, 2),
    timepoint = rep(c("Before", "After"), each = 12),
    value     = rnorm(24),
    group     = rep(c("A", "B"), 12)
  )
  p <- cliplot(value ~ timepoint, data = paired,
               type = type_slope(pair = "subject", color_by = "group"))
  expect_s3_class(p, "ggplot")
})

test_that("type_slope works without color grouping", {
  paired <- data.frame(
    subject   = rep(1:10, 2),
    timepoint = rep(c("T0", "T1"), each = 10),
    value     = rnorm(20)
  )
  p <- cliplot(value ~ timepoint, data = paired,
               type = type_slope(pair = "subject"))
  expect_s3_class(p, "ggplot")
})

# ---- confusion ----

test_that("type_confusion works via formula", {
  set.seed(5)
  df <- data.frame(
    truth = factor(sample(c("pos", "neg"), 60, TRUE)),
    pred  = factor(sample(c("pos", "neg"), 60, TRUE))
  )
  p <- cliplot(truth ~ pred, data = df, type = "confusion")
  expect_s3_class(p, "ggplot")
})

test_that("type_confusion adds Fisher p-value for 2x2 tables", {
  set.seed(6)
  df <- data.frame(
    truth = factor(sample(c("pos", "neg"), 80, TRUE)),
    pred  = factor(sample(c("pos", "neg"), 80, TRUE))
  )
  p <- cliplot(truth ~ pred, data = df, type = type_confusion(add_pvalue = TRUE))
  expect_s3_class(p, "ggplot")
})

# ---- donut ----

test_that("type_donut works with a categorical variable", {
  p <- cliplot(~ Species, data = iris, type = "donut")
  expect_s3_class(p, "ggplot")
})

test_that("type_donut works with values and center label", {
  df <- data.frame(cat = c("A", "B", "C"), val = c(10, 20, 30))
  p <- cliplot(val ~ cat, data = df,
               type = type_donut(center_label = "Total", center_value = "60"))
  expect_s3_class(p, "ggplot")
})

# ---- helpers ----

test_that("cli_panel_label adds an annotation layer", {
  p <- cliplot(mpg ~ wt, data = mtcars, theme = "nature") + cli_panel_label("A")
  expect_s3_class(p, "ggplot")
  layers <- ggplot2::ggplot_build(p)$plot$layers
  expect_true(any(vapply(layers, function(l) "GeomText" %in% class(l$geom), logical(1))))
})

test_that("cli_legend_out moves the legend", {
  p <- cliplot(mpg ~ wt, data = mtcars) + cli_legend_out("right")
  expect_s3_class(p, "ggplot")
  built <- ggplot2::ggplot_build(p)
  expect_equal(built$plot$theme$legend.position, "right")
})

test_that("cli_journal_size returns reference dimensions", {
  sz <- cli_journal_size("nature", "single")
  expect_equal(sz$width_mm, 89)
  expect_gt(sz$width_in, 3.4)
  sz2 <- cli_journal_size("cell", "double")
  expect_equal(sz2$width_mm, 180)
})

test_that("cli_save writes a file at journal width", {
  p <- cliplot(mpg ~ wt, data = mtcars)
  f <- tempfile(fileext = ".png")
  cli_save(p, f, journal = "science", column = "single", dpi = 96)
  expect_true(file.exists(f))
  unlink(f)
})
