# ===========================================================================
# Unit Tests: Cross-Network Module Comparison & Module-Trait Correlation
# ===========================================================================

test_that("type_module_compare works with node-level data", {
  set.seed(42)
  genes <- paste0("Gene_", 1:90)
  net1 <- sample(c("M1_Inflam", "M2_Barrier", "M3_Lipid"), 90, replace = TRUE)
  net2 <- net1
  idx <- sample(1:90, 30)
  net2[idx] <- sample(c("ModA_Th2", "ModB_Stratum", "ModC_Sebum"), 30, replace = TRUE)

  df_nodes <- data.frame(
    Gene = genes,
    Discovery = net1,
    Replication = net2,
    stringsAsFactors = FALSE
  )

  # Formula call with character type
  p1 <- cliplot(Replication ~ Discovery, data = df_nodes, type = "module_compare")
  expect_s3_class(p1, "ggplot")

  # Alias cross_network
  p2 <- cliplot(Replication ~ Discovery, data = df_nodes, type = "cross_network")
  expect_s3_class(p2, "ggplot")

  # Direct type_module_compare with jaccard
  p3 <- cliplot(
    Replication ~ Discovery,
    data = df_nodes,
    type = type_module_compare(fill_by = "jaccard", show_stars = TRUE, show_totals = TRUE)
  )
  expect_s3_class(p3, "ggplot")

  # Direct type_module_compare with representation factor (rf)
  p4 <- cliplot(
    Replication ~ Discovery,
    data = df_nodes,
    type = type_module_compare(fill_by = "rf", show_p = TRUE, total_universe = 200)
  )
  expect_s3_class(p4, "ggplot")

  # Direct type_module_compare with overlap count
  p5 <- cliplot(
    Replication ~ Discovery,
    data = df_nodes,
    type = type_module_compare(fill_by = "overlap", text_color = "black")
  )
  expect_s3_class(p5, "ggplot")
})

test_that("type_module_compare works with precomputed summary data", {
  summary_df <- expand.grid(
    Net1 = c("Discovery_M1", "Discovery_M2"),
    Net2 = c("Replication_A", "Replication_B"),
    stringsAsFactors = FALSE
  )
  summary_df$overlap <- c(25, 4, 2, 30)
  summary_df$pvalue <- c(1e-8, 0.45, 0.82, 1e-10)
  summary_df$jaccard <- c(0.65, 0.08, 0.04, 0.72)

  p <- cliplot(Net2 ~ Net1, data = summary_df, type = "module_compare")
  expect_s3_class(p, "ggplot")
})

test_that("type_module_trait works with precomputed correlation tables", {
  cor_df <- expand.grid(
    Module = c("M1_Inflammation", "M2_SkinBarrier", "M3_Microbiome"),
    Factor = c("Skin_pH", "Humidity", "PM25", "SCORAD"),
    stringsAsFactors = FALSE
  )
  cor_df$cor <- c(0.42, -0.15, 0.58, -0.31, 0.48, -0.22, 0.25, -0.40, 0.65, 0.71, -0.28, 0.82)
  cor_df$pvalue <- c(0.002, 0.25, 1e-5, 0.03, 0.0004, 0.12, 0.08, 0.005, 1e-6, 1e-7, 0.04, 1e-12)

  # Formula call with default settings
  p1 <- cliplot(Module ~ Factor, data = cor_df, type = "module_trait")
  expect_s3_class(p1, "ggplot")

  # Aliases
  p2 <- cliplot(Module ~ Factor, data = cor_df, type = "module_env")
  expect_s3_class(p2, "ggplot")
  p3 <- cliplot(Module ~ Factor, data = cor_df, type = "module_cor")
  expect_s3_class(p3, "ggplot")

  # Clustering and stars formatting
  p4 <- cliplot(
    Module ~ Factor,
    data = cor_df,
    type = type_module_trait(
      show_labels = "coef_stars",
      cluster_modules = TRUE,
      cluster_env = TRUE,
      sig_digits = 3
    )
  )
  expect_s3_class(p4, "ggplot")

  # P-value cutoff filtering
  p5 <- cliplot(
    Module ~ Factor,
    data = cor_df,
    type = type_module_trait(show_labels = "both", p_cutoff = 0.01)
  )
  expect_s3_class(p5, "ggplot")
})

test_that("type_module_trait works with sample-level multi-variable data", {
  set.seed(42)
  n <- 40
  sample_df <- data.frame(
    ME_Inflamm = rnorm(n),
    ME_Barrier = rnorm(n),
    ME_Lipid = rnorm(n),
    Skin_pH = rnorm(n),
    Humidity = rnorm(n),
    SCORAD = rnorm(n)
  )

  p <- cliplot(sample_df, type = type_module_trait(method = "spearman"))
  expect_s3_class(p, "ggplot")

  # Direct matrix input
  mat <- stats::cor(sample_df)
  p_mat <- cliplot(mat, type = "module_trait")
  expect_s3_class(p_mat, "ggplot")
})

test_that("type_cross_network_graph and cliplot_cross_network_graph work", {
  demo <- generate_demo_cross_network()
  
  # Node-level graph
  p_node <- cliplot_cross_network_graph(
    nodes = demo$nodes,
    edges = demo$edges,
    cross_edges = demo$cross_edges,
    level = "node",
    title = "Cross Network Topology"
  )
  expect_s3_class(p_node, "ggplot")
  
  # Module-level graph
  p_mod <- cliplot_cross_network_graph(
    nodes = demo$nodes,
    edges = demo$edges,
    cross_edges = demo$cross_edges,
    level = "module"
  )
  expect_s3_class(p_mod, "ggplot")
  
  # Via cliplot dispatch
  p_cli <- cliplot(demo$nodes, type = "cross_network_graph")
  expect_s3_class(p_cli, "ggplot")
})

test_that("type_env_network and cliplot_env_network work", {
  # Direct function call
  p_env <- cliplot_env_network(title = "Central Network Coupled with Environmental Factors")
  expect_s3_class(p_env, "ggplot")
  
  # With correlation values enabled
  p_vals <- cliplot_env_network(show_values = TRUE, value_digits = 2, compact = TRUE)
  expect_s3_class(p_vals, "ggplot")

  # With coef label_type
  p_coef <- cliplot_env_network(label_type = "coef", value_size = 1.5)
  expect_s3_class(p_coef, "ggplot")
  
  # Via cliplot dispatch
  p_cli_env <- cliplot(data.frame(), type = "env_network")
  expect_s3_class(p_cli_env, "ggplot")
})
