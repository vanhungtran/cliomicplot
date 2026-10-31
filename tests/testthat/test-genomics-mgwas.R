# ===========================================================================
# Clinical & Genomics Archetype Tests: Manhattan (mGWAS), Bubble Matrix, Sharing Heatmap
# ===========================================================================

test_that("type_manhattan works with single chromosome mGWAS", {
  set.seed(42)
  df <- data.frame(
    pos = seq(1000, 200000, by = 2000),
    qval = c(runif(95, 0.05, 1.0), runif(5, 1e-5, 0.01)),
    gene = c(rep("", 95), paste0("mut_", 1:5))
  )

  p <- cliplot(qval ~ pos, data = df, type = "manhattan")
  expect_s3_class(p, "ggplot")

  p_custom <- cliplot(
    qval ~ pos,
    data = df,
    type = type_manhattan(threshold = 0.01, label_col = "gene", point_size = 1.5)
  )
  expect_s3_class(p_custom, "ggplot")
})

test_that("type_manhattan works with multi-chromosome GWAS", {
  set.seed(42)
  df <- data.frame(
    chr = rep(c("1", "2", "3"), each = 50),
    bp = rep(seq(1000, 50000, length.out = 50), 3),
    p = c(runif(145, 0.01, 1.0), runif(5, 1e-9, 1e-6)),
    snp = paste0("rs", 1:150)
  )

  p <- cliplot(p ~ bp | chr, data = df, type = "manhattan")
  expect_s3_class(p, "ggplot")

  # Direct call to type_manhattan
  p2 <- cliplot(p ~ bp | chr, data = df, type = type_manhattan(threshold = 5e-8, label_hits = TRUE))
  expect_s3_class(p2, "ggplot")
})

test_that("type_bubble_matrix works for differential abundance", {
  set.seed(42)
  da_df <- expand.grid(
    Species = c("S. epidermidis", "S. aureus", "C. acnes", "M. restricta"),
    Comparison = c("Forehead", "Cheek", "Antecubital")
  )
  da_df$log2FC <- rnorm(nrow(da_df), mean = 0, sd = 1.2)
  da_df$qval <- runif(nrow(da_df), 0.001, 0.2)

  p <- cliplot(Species ~ Comparison, data = da_df, type = "bubble_matrix")
  expect_s3_class(p, "ggplot")

  p_custom <- cliplot(
    Species ~ Comparison,
    data = da_df,
    type = type_bubble_matrix(size_col = "qval", fill_col = "log2FC", min_size = 2, max_size = 8)
  )
  expect_s3_class(p_custom, "ggplot")
})

test_that("type_sharing_heatmap works for dyad transmission matrices", {
  set.seed(42)
  dyad_df <- expand.grid(
    Species = c("S. epidermidis", "S. hominis", "C. acnes"),
    Dyad = paste0("Dyad_", 1:6)
  )
  dyad_df$Status <- ifelse(runif(nrow(dyad_df)) > 0.5, "Shared", "Not shared")

  p <- cliplot(Species ~ Dyad, data = dyad_df, type = "sharing_heatmap")
  expect_s3_class(p, "ggplot")

  p_custom <- cliplot(
    Species ~ Dyad,
    data = dyad_df,
    type = type_sharing_heatmap(status_col = "Status", shared_color = "#D73027")
  )
  expect_s3_class(p_custom, "ggplot")
})
