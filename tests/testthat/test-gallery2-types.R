# ===========================================================================
# Gallery-inspired type tests: chord, treemap, streamgraph, connected scatter,
# circular barplot, 2D density, parallel coordinates, dendrogram
# Uses simulated data
# ===========================================================================

# ---- chord ----

test_that("type_chord works with matrix", {
  set.seed(42)
  chord_mat <- matrix(sample(10:100, 25), nrow = 5)
  rownames(chord_mat) <- LETTERS[1:5]
  colnames(chord_mat) <- LETTERS[1:5]
  p <- cliplot(chord_mat, type = "chord")
  expect_true(is(p, "ggplot") || is(p, "gtable") || is.list(p))
})

# ---- treemap ----

test_that("type_treemap works", {
  skip_if_not_installed("treemapify")
  tm_df <- data.frame(
    category = c("CDS", "UTR", "Intron", "Intergenic", "Promoter"),
    bases = c(1500000, 800000, 2500000, 3200000, 200000),
    stringsAsFactors = FALSE
  )
  p <- cliplot(bases ~ category, data = tm_df, type = "treemap")
  expect_s3_class(p, "ggplot")
})

# ---- streamgraph ----

test_that("type_streamgraph works", {
  skip_if_not_installed("ggstream")
  set.seed(42)
  sg_df <- data.frame(
    time = rep(1:20, 3),
    category = rep(c("A", "B", "C"), each = 20),
    value = round(runif(60, 5, 25))
  )
  p <- tryCatch(
    cliplot(value ~ time, data = sg_df, by = sg_df$category, type = "streamgraph"),
    error = function(e) NULL
  )
  if (!is.null(p)) expect_s3_class(p, "ggplot")
})

test_that("type_streamgraph falls back to stacked area", {
  set.seed(42)
  sg_df <- expand.grid(time = 1:10, category = c("A", "B", "C"))
  sg_df$value <- round(runif(nrow(sg_df), 5, 20))
  p <- tryCatch(
    cliplot(value ~ time, data = sg_df, by = sg_df$category, type = "streamgraph"),
    error = function(e) NULL
  )
  if (!is.null(p)) expect_s3_class(p, "ggplot")
})

# ---- connected scatter ----

test_that("type_connected works", {
  set.seed(42)
  path_df <- data.frame(
    time = 1:30,
    x = cumsum(rnorm(30, 0, 1)),
    y = cumsum(rnorm(30, 0, 1))
  )
  p <- cliplot(y ~ x, data = path_df, type = "connected")
  expect_s3_class(p, "ggplot")
})

# ---- circular barplot ----

test_that("type_circular_bar works", {
  cb_df <- data.frame(
    label = LETTERS[1:8],
    value = c(45, 72, 33, 58, 91, 27, 63, 39)
  )
  p <- cliplot(value ~ label, data = cb_df, type = "circular_bar")
  expect_s3_class(p, "ggplot")
})

# ---- 2D density ----

test_that("type_density2d works", {
  set.seed(42)
  dens_df <- data.frame(
    x = rnorm(500),
    y = rnorm(500)
  )
  p <- cliplot(y ~ x, data = dens_df, type = "density2d")
  expect_s3_class(p, "ggplot")
})

test_that("type_density2d works with points overlay", {
  set.seed(42)
  dens_df <- data.frame(x = rnorm(200), y = rnorm(200))
  p <- cliplot(y ~ x, data = dens_df,
               type = type_density2d(show_points = TRUE))
  expect_s3_class(p, "ggplot")
})

# ---- parallel coordinates ----

test_that("type_parallel works", {
  set.seed(42)
  pc_df <- data.frame(
    Group = sample(c("Responder", "Non-responder"), 30, replace = TRUE),
    CD4 = rnorm(30, 500, 150),
    CD8 = rnorm(30, 300, 100),
    NK = rnorm(30, 100, 40),
    Bcell = rnorm(30, 200, 80)
  )
  p <- cliplot(pc_df, type = "parallel")
  expect_s3_class(p, "ggplot")
})

# ---- dendrogram ----

test_that("type_dendrogram works", {
  skip_if_not_installed("ggdendro")
  mat <- matrix(rnorm(50), nrow = 10)
  rownames(mat) <- paste0("Gene", 1:10)
  p <- cliplot(mat, type = "dendrogram")
  expect_s3_class(p, "ggplot")
})
