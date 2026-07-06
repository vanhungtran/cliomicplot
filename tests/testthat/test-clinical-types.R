# ===========================================================================
# Clinical & omics type tests: boxplot, violin, volcano, PCA, MA, correlation
# forest, KM, waterfall, swimmer, heatmap
# ===========================================================================

# ---- boxplot ----

test_that("type_boxplot works without grouping", {
  p <- cliplot(Sepal.Length ~ 1, data = iris, type = "boxplot")
  expect_s3_class(p, "ggplot")
})

test_that("type_boxplot works with grouping", {
  p <- cliplot(Sepal.Length ~ Species, data = iris, type = "boxplot")
  expect_s3_class(p, "ggplot")
})

# ---- violin ----

test_that("type_violin works with grouping", {
  p <- cliplot(Sepal.Length ~ Species, data = iris, type = "violin")
  expect_s3_class(p, "ggplot")
})

# ---- PCA ----

test_that("type_pca works from data frame", {
  p <- cliplot(iris[, 1:4], type = "pca", by = iris$Species)
  expect_s3_class(p, "ggplot")
})

test_that("type_pca works with ellipses", {
  p <- cliplot(iris[, 1:4],
               type = type_pca(add_ellipse = TRUE, ellipse_level = 0.95),
               by = iris$Species)
  expect_s3_class(p, "ggplot")
})

test_that("type_pca works with sample labels", {
  p <- cliplot(iris[, 1:4],
               type = type_pca(add_ellipse = FALSE, label_samples = TRUE),
               by = iris$Species)
  expect_s3_class(p, "ggplot")
})

test_that("type_pca works from pre-computed scores", {
  pca_res <- prcomp(iris[, 1:4], center = TRUE, scale. = TRUE)
  scores  <- as.data.frame(pca_res$x)
  scores$Species <- iris$Species
  p <- cliplot(PC2 ~ PC1, data = scores, by = scores$Species, type = "points")
  expect_s3_class(p, "ggplot")
})

# ---- volcano ----

test_that("type_volcano works", {
  set.seed(42)
  de_data <- data.frame(
    logFC  = rnorm(1000, 0, 0.5),
    PValue = 10^-runif(1000, 0, 8)
  )
  p <- cliplot(-log10(PValue) ~ logFC, data = de_data, type = "volcano")
  expect_s3_class(p, "ggplot")
})

test_that("type_volcano works with custom cutoffs", {
  set.seed(42)
  de_data <- data.frame(
    logFC  = rnorm(500, 0, 0.5),
    PValue = 10^-runif(500, 0, 8)
  )
  p <- cliplot(-log10(PValue) ~ logFC, data = de_data,
               type = type_volcano(pval_cutoff = 0.001, fc_cutoff = 1.5))
  expect_s3_class(p, "ggplot")
})

# ---- MA plot ----

test_that("type_ma works", {
  set.seed(123)
  ma_data <- data.frame(
    baseMean       = 10^runif(500, 0, 5),
    log2FoldChange = rnorm(500, 0, 0.5),
    padj           = runif(500)
  )
  p <- cliplot(log2FoldChange ~ baseMean, data = ma_data, type = "ma")
  expect_s3_class(p, "ggplot")
})

# ---- correlation ----

test_that("type_correlation works", {
  p <- cliplot(mtcars[, 1:5], type = "correlation")
  expect_s3_class(p, "ggplot")
})

test_that("type_correlation works with lower triangle", {
  p <- cliplot(mtcars[, 1:5],
               type = type_correlation(type = "lower", add_coef = TRUE))
  expect_s3_class(p, "ggplot")
})

# ---- forest ----

test_that("type_forest works", {
  forest_data <- data.frame(
    Variable = c("Age", "Sex", "Stage"),
    HR = c(1.5, 2.0, 3.5),
    CI_low = c(1.1, 1.3, 2.0),
    CI_high = c(2.0, 3.0, 6.0),
    P = c(0.01, 0.003, 0.5)
  )
  p <- cliplot(HR ~ Variable, data = forest_data, type = "forest")
  expect_s3_class(p, "ggplot")
})

# ---- KM ----

test_that("type_km works with survival data", {
  skip_if_not_installed("survival")
  library(survival)
  data(lung, package = "survival")
  p <- cliplot(Surv(time, status) ~ sex, data = lung, type = "km")
  expect_true(is(p, "ggplot") || is(p, "patchwork") || is(p, "ggsurvplot") ||
              is(p, "gtable") || is.list(p))
})

# ---- waterfall ----

test_that("type_waterfall works", {
  set.seed(42)
  n_pts <- 20
  imtrial <- data.frame(
    Patient    = paste0("P-", sprintf("%03d", 1:n_pts)),
    BestChange = sort(rnorm(n_pts, mean = -15, sd = 25)),
    BestResp   = sample(c("CR", "PR", "SD", "PD"), n_pts, replace = TRUE),
    stringsAsFactors = FALSE
  )
  p <- cliplot(BestChange ~ Patient, data = imtrial, type = "waterfall")
  expect_s3_class(p, "ggplot")
})

# ---- swimmer ----

test_that("type_swimmer works", {
  set.seed(42)
  n_pts <- 15
  swim_data <- data.frame(
    Patient  = paste0("P-", sprintf("%03d", 1:n_pts)),
    Duration = pmax(1, round(rnorm(n_pts, mean = 12, sd = 5), 1)),
    BestResp = sample(c("CR", "PR", "SD", "PD"), n_pts, replace = TRUE),
    stringsAsFactors = FALSE
  )
  p <- cliplot(Duration ~ Patient, data = swim_data, type = "swimmer")
  expect_s3_class(p, "ggplot")
})

# ---- heatmap ----

test_that("type_heatmap works with matrix", {
  mat <- matrix(rnorm(100), nrow = 10)
  rownames(mat) <- paste0("Gene", 1:10)
  colnames(mat) <- paste0("Sample", 1:10)
  p <- cliplot(mat, type = "heatmap")
  expect_true(is(p, "ggplot") || is(p, "gtable") || is.list(p))
})
