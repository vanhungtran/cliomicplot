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
  
  # Module-level graph with custom sizes and styling
  p_mod <- cliplot_cross_network_graph(
    nodes              = demo$nodes,
    edges              = demo$edges,
    cross_edges        = demo$cross_edges,
    level              = "module",
    show_module_labels = TRUE,
    module_label_size  = 3.0,
    meta_label_size    = 3.5,
    module_size_range  = c(4.0, 9.0),
    circle_radius      = 1.15
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

  # With module labels enabled and custom names
  custom_lbls <- c("Mod_1" = "M1: Core", "Mod_2" = "M2: Th2", "Mod_3" = "M3: Lipid")
  p_lbls <- cliplot_env_network(show_module_labels = TRUE, module_labels = custom_lbls, module_label_pos = "radial")
  expect_s3_class(p_lbls, "ggplot")

  # With module labels disabled
  p_nolbl <- cliplot_env_network(show_module_labels = FALSE)
  expect_s3_class(p_nolbl, "ggplot")

  # With customizable edge styling, colors, and link correlation digits
  p_custom <- cliplot_env_network(
    show_values        = TRUE,
    value_digits       = 3,
    show_link_values   = TRUE,
    link_value_size    = 2.2,
    link_width         = 0.9,
    circle_color       = "#1F77B4",
    circle_width       = 0.6,
    env_label_size     = 2.2,
    env_label_color    = "darkblue",
    diag_point_size    = 2.2,
    diag_point_fill    = "gold"
  )
  expect_s3_class(p_custom, "ggplot")

  # With coef label_type
  p_coef <- cliplot_env_network(label_type = "coef", value_size = 1.5)
  expect_s3_class(p_coef, "ggplot")
  
  # Via cliplot dispatch
  p_cli_env <- cliplot(data.frame(), type = "env_network")
  expect_s3_class(p_cli_env, "ggplot")
})

test_that("cli_module_preservation and type_module_preservation work", {
  demo <- cli_demo_vitality_multiomics(n_samples = 25, n_features = 30, seed = 123)
  
  pres <- cli_module_preservation(
    ref_data    = demo$ref_expr,
    test_data   = demo$test_expr,
    ref_modules = demo$modules,
    n_perm      = 20,
    seed        = 123
  )
  expect_true(is.data.frame(pres))
  expect_true(all(c("module", "size", "z_summary", "preservation") %in% names(pres)))
  expect_true(nrow(pres) >= 3)
  
  # Plotting via type_module_preservation
  p_pres <- cliplot(z_summary ~ size, data = pres, type = type_module_preservation())
  expect_s3_class(p_pres, "ggplot")

  # Standalone helper
  p_stand <- cliplot_module_preservation(pres)
  expect_s3_class(p_stand, "ggplot")
})

test_that("cli_network_robustness and type_network_robustness work", {
  demo <- cli_demo_vitality_multiomics(n_samples = 20, n_features = 25, seed = 42)

  rob <- cli_network_robustness(
    networks     = demo$networks,
    attack       = c("random", "degree"),
    rm_fractions = c(0.1, 0.3, 0.5),
    n_perm       = 5,
    seed         = 42
  )
  expect_true(is.data.frame(rob))
  expect_true(all(c("group", "attack", "fraction_removed", "value", "se") %in% names(rob)))

  # Plotting
  p_rob <- cliplot(value ~ fraction_removed, data = rob, type = type_network_robustness())
  expect_s3_class(p_rob, "ggplot")

  # Standalone helper
  p_stand_rob <- cliplot_network_robustness(rob)
  expect_s3_class(p_stand_rob, "ggplot")
})

test_that("cli_mantel and cli_partial_cor work", {
  demo <- cli_demo_vitality_multiomics(n_samples = 20, n_features = 20, seed = 99)

  mantel_res <- cli_mantel(
    comm_data   = demo$ref_expr,
    env_data    = demo$env_data[, 1:4],
    dist_comm   = "bray",
    dist_env    = "euclidean",
    n_perm      = 49,
    by_variable = TRUE
  )
  expect_true(is.data.frame(mantel_res))
  expect_true(all(c("variable", "statistic", "pvalue", "padj") %in% names(mantel_res)))
  expect_true(nrow(mantel_res) >= 4)

  # Partial mantel test with covariate
  cov_df <- data.frame(Age = rnorm(20), Batch = rnorm(20))
  pmantel_res <- cli_mantel(
    comm_data    = demo$ref_expr,
    env_data     = demo$env_data[, 1:2],
    partial_data = cov_df,
    n_perm       = 49
  )
  expect_true(is.data.frame(pmantel_res))
  expect_true(all(pmantel_res$method == "Partial_Mantel"))

  # Partial correlation
  pcor <- cli_partial_cor(x = rnorm(20), y = rnorm(20), z = cov_df)
  expect_true(is.data.frame(pcor))
  expect_true(all(c("estimate", "pvalue", "padj") %in% names(pcor)))
})

test_that("cli_module_summary and cli_network_rewiring work", {
  demo <- cli_demo_vitality_multiomics(n_samples = 20, n_features = 24, seed = 88)

  # Eigengene summary
  me_df <- cli_module_summary(demo$ref_expr, demo$modules, method = "eigengene")
  expect_true(is.data.frame(me_df))
  expect_equal(nrow(me_df), 20)
  expect_true(ncol(me_df) >= 3)

  # Mean score summary
  ms_df <- cli_module_summary(demo$ref_expr, demo$modules, method = "mean_score")
  expect_equal(nrow(ms_df), 20)

  # Rewiring test
  rewire <- cli_network_rewiring(demo$networks$KO, demo$networks$WT, r_threshold = 0.3)
  expect_true(is.list(rewire))
  expect_true("jaccard_similarity" %in% names(rewire))
  expect_true(is.data.frame(rewire$edges))
  expect_true(is.data.frame(rewire$node_rewiring))
})

test_that("cli_scifig_qa and AI4SciFig publication gates work", {
  # Build a nature-preset coupled network
  cors <- list(
    "Env A" = matrix(c(1.0, -0.001, -0.001, 1.0), 2, 2, dimnames = list(c("V1", "V2"), c("V1", "V2"))),
    "Env B" = matrix(c(1.0, 0.45, 0.45, 1.0), 2, 2, dimnames = list(c("V3", "V4"), c("V3", "V4")))
  )

  p <- cliplot_env_network(
    env_cors     = cors,
    show_values  = TRUE,
    compact      = TRUE,
    preset       = "nature",
    scifig       = TRUE
  )

  expect_s3_class(p, "ggplot")

  # Run SciFig QA audit
  qa <- cli_scifig_qa(p, journal = "nature", column = "double")
  expect_s3_class(qa, "cli_scifig_qa")
  expect_true(is.data.frame(qa))
  expect_true(all(c("gate", "status", "measurement", "rule_source") %in% names(qa)))

  # Verify Zero Sign Suppression gate passes
  zero_row <- qa[qa$gate == "Zero Sign Suppression", ]
  expect_equal(zero_row$status, "PASS")
  expect_false(grepl("Negative zero detected", zero_row$measurement))

  # Test print method
  out_txt <- capture.output(print(qa))
  expect_true(any(grepl("SciFig QA Journal Compliance Report", out_txt)))
})

test_that("cliplot_env_network legend options and row formatting work", {
  cors <- list(
    "Env A" = matrix(c(1.0, 0.5, 0.5, 1.0), 2, 2, dimnames = list(c("V1", "V2"), c("V1", "V2")))
  )

  # With module legend
  p1 <- cliplot_env_network(env_cors = cors, show_module_legend = TRUE, legend_position = "top")
  expect_s3_class(p1, "ggplot")
  g1 <- ggplot2::ggplot_build(p1)
  expect_true(!is.null(g1))

  # Without module legend
  p2 <- cliplot_env_network(env_cors = cors, show_module_legend = FALSE, legend_position = "bottom")
  expect_s3_class(p2, "ggplot")
  g2 <- ggplot2::ggplot_build(p2)
  expect_true(!is.null(g2))
})

test_that("cliplot_cross_network_graph style presets and zero-arg fallback work", {
  # Zero-argument invocation
  p0 <- cliplot_cross_network_graph()
  expect_s3_class(p0, "ggplot")

  # Nature preset
  p_nat <- cliplot_cross_network_graph(preset = "nature")
  expect_s3_class(p_nat, "ggplot")

  # Module-level ring preservation
  p_ring <- cliplot_cross_network_graph(level = "module", preset = "nature")
  expect_s3_class(p_ring, "ggplot")

  # Horizontal flow progression
  p_flow <- cliplot_cross_network_graph(preset = "horizontal_flow")
  expect_s3_class(p_flow, "ggplot")

  # Dark mode
  p_dark <- cliplot_cross_network_graph(preset = "dark")
  expect_s3_class(p_dark, "ggplot")

  # Lancet minimal
  p_lan <- cliplot_cross_network_graph(preset = "lancet")
  expect_s3_class(p_lan, "ggplot")
})

test_that("cliplot_env_network style presets work out of the box", {
  # Clinical / nature style
  p_clin <- cliplot_env_network(preset = "clinical", show_values = TRUE)
  expect_s3_class(p_clin, "ggplot")

  # Seasonal style
  p_seas <- cliplot_env_network(preset = "seasonal", show_values = TRUE)
  expect_s3_class(p_seas, "ggplot")

  # Dark style
  p_dark <- cliplot_env_network(preset = "dark", show_values = TRUE)
  expect_s3_class(p_dark, "ggplot")

  # Lancet style
  p_lan <- cliplot_env_network(preset = "lancet", show_values = TRUE)
  expect_s3_class(p_lan, "ggplot")
})
