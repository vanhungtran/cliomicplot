test_that("cli_calc_gsea_running computes correct cumulative walk and peak ES", {
  set.seed(42)
  genes <- paste0("Gene_", 1:500)
  stats <- sort(rnorm(500, mean = 0, sd = 2), decreasing = TRUE)
  names(stats) <- genes
  gene_set <- genes[1:25] # Strongly enriched at top

  walk <- cli_calc_gsea_running(stats, gene_set)
  expect_s3_class(walk, "data.frame")
  expect_equal(nrow(walk), 500)
  expect_true("running_es" %in% names(walk))
  expect_true("is_peak" %in% names(walk))
  expect_true(attr(walk, "es") > 0.5)
  expect_equal(sum(walk$is_peak), 1)
})

test_that("type_gsea_running produces valid composite plot", {
  set.seed(42)
  genes <- paste0("Gene_", 1:300)
  stats <- sort(rnorm(300, mean = 0, sd = 1.5), decreasing = TRUE)
  names(stats) <- genes
  gene_set <- genes[c(2, 5, 8, 12, 19, 25, 40, 60)]

  walk <- cli_calc_gsea_running(stats, gene_set)
  p <- cliplot(
    walk,
    type = type_gsea_running(
      line_col = "#1B7837",
      pathway_name = "Test Pathway",
      nes = 2.15,
      fdr = 0.001
    )
  )
  expect_true(inherits(p, "ggplot") || inherits(p, "patchwork"))
})

test_that("cli_calc_ora correctly computes hypergeometric test statistics", {
  pathways <- list(
    "Pathway_A" = paste0("Gene_", 1:20),
    "Pathway_B" = paste0("Gene_", 50:70),
    "Pathway_C" = paste0("Gene_", 100:150)
  )
  bg_genes <- paste0("Gene_", 1:500)
  # Heavily enriched in Pathway_A
  de_genes <- c(paste0("Gene_", 1:15), paste0("Gene_", 200:210))

  res <- cli_calc_ora(de_genes, pathways, background = bg_genes)
  expect_s3_class(res, "data.frame")
  expect_equal(res$Pathway[1], "Pathway_A")
  expect_true(res$padj[1] < 1e-10)
  expect_equal(res$Count[1], 15)
  expect_true(res$FoldEnrichment[1] > 10)
})

