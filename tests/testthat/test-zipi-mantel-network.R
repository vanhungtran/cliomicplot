test_that("calc_zipi computes correct Zi, Pi and roles", {
  set.seed(42)
  n <- 12
  adj <- matrix(0, n, n)
  rownames(adj) <- colnames(adj) <- paste0("Taxa_", seq_len(n))
  # Two clear modules: 1:6 and 7:12
  adj[1:6, 1:6] <- 1
  adj[7:12, 7:12] <- 1
  # Connectors between modules
  adj[1, 7] <- adj[7, 1] <- 1
  adj[2, 8] <- adj[8, 2] <- 1
  diag(adj) <- 0

  mods <- setNames(c(rep("M1", 6), rep("M2", 6)), rownames(adj))

  res <- calc_zipi(adj, mods, z_threshold = 1.0, p_threshold = 0.3)
  expect_s3_class(res, "data.frame")
  expect_equal(nrow(res), n)
  expect_true(all(c("node", "module", "degree", "within_degree", "Zi", "Pi", "role") %in% names(res)))
  expect_true(all(res$role %in% c("Peripheral", "Connector", "Module hub", "Network hub")))
  expect_true(is.numeric(res$Zi))
  expect_true(is.numeric(res$Pi))
  expect_true(all(res$Pi >= 0 & res$Pi <= 1))
})

test_that("type_zipi works via cliplot formula and adjacency matrix", {
  set.seed(42)
  n <- 20
  df <- data.frame(
    taxa   = paste0("OTU_", seq_len(n)),
    Zi     = rnorm(n, mean = 0.5, sd = 1.2),
    Pi     = runif(n, 0, 0.8),
    role   = sample(c("Peripheral", "Connector", "Module hub", "Network hub"), n, replace = TRUE),
    Phylum = sample(c("Firmicutes", "Bacteroidetes", "Proteobacteria"), n, replace = TRUE),
    stringsAsFactors = FALSE
  )

  # Formula call
  p1 <- cliplot(Zi ~ Pi | role, data = df, type = type_zipi())
  expect_s3_class(p1, "ggplot")

  # Character type alias call
  p2 <- cliplot(Zi ~ Pi, data = df, type = "zipi")
  expect_s3_class(p2, "ggplot")

  # Adjacency matrix call
  adj <- matrix(sample(c(0, 1), 64, replace = TRUE, prob = c(0.7, 0.3)), 8, 8)
  adj <- (adj + t(adj)) / 2
  diag(adj) <- 0
  rownames(adj) <- colnames(adj) <- paste0("Taxa_", 1:8)
  mods <- setNames(rep(c("M1", "M2"), each = 4), rownames(adj))

  p3 <- cliplot(type = type_zipi(adj_mat = adj, modules = mods))
  expect_s3_class(p3, "ggplot")
})

test_that("type_mantel_triangle works and renders ggplot", {
  # Default demo call
  p_demo <- cliplot(type = type_mantel_triangle())
  expect_s3_class(p_demo, "ggplot")

  # Pre-computed mantel dataframe
  env_mat <- matrix(runif(16, -0.5, 0.8), 4, 4)
  diag(env_mat) <- 1
  colnames(env_mat) <- rownames(env_mat) <- c("pH", "SOC", "TN", "Moisture")

  mantel_df <- data.frame(
    spec = rep(c("Bacteria", "Fungi"), each = 4),
    env  = rep(c("pH", "SOC", "TN", "Moisture"), 2),
    r    = c(0.45, 0.25, 0.10, 0.52, 0.12, 0.38, 0.05, 0.41),
    p    = c(0.001, 0.02, 0.15, 0.0005, 0.3, 0.008, 0.45, 0.015),
    stringsAsFactors = FALSE
  )

  p_custom <- cliplot(type = type_mantel_triangle(env_cor = env_mat, mantel_res = mantel_df))
  expect_s3_class(p_custom, "ggplot")

  # Character alias
  p_alias <- cliplot(type = "mantel_triangle")
  expect_s3_class(p_alias, "ggplot")
})

test_that("type_polygon_network renders adaptive and constant ring layouts", {
  # Adaptive radius (PolygonRrClusterG)
  p_rr <- cliplot(type = type_polygon_network(adaptive_radius = TRUE))
  expect_s3_class(p_rr, "ggplot")

  # Constant radius (PolygonClusterG)
  p_const <- cliplot(type = type_polygon_network(adaptive_radius = FALSE))
  expect_s3_class(p_const, "ggplot")

  # Character alias
  p_alias <- cliplot(type = "polygon_network")
  expect_s3_class(p_alias, "ggplot")
})

test_that("type_bipartite renders bipolar and concentric transkingdom graphs", {
  # Bipolar layout
  p_bipolar <- cliplot(type = type_bipartite(layout = "bipolar"))
  expect_s3_class(p_bipolar, "ggplot")

  # Concentric layout
  p_concentric <- cliplot(type = type_bipartite(layout = "concentric"))
  expect_s3_class(p_concentric, "ggplot")

  # Character alias
  p_alias <- cliplot(type = "bipartite")
  expect_s3_class(p_alias, "ggplot")
})

test_that("type_edge_dynamics renders stack, ratio, and counts modes", {
  # Stack mode
  p_stack <- cliplot(type = type_edge_dynamics(mode = "stack"))
  expect_s3_class(p_stack, "ggplot")

  # Ratio mode
  p_ratio <- cliplot(type = type_edge_dynamics(mode = "ratio"))
  expect_s3_class(p_ratio, "ggplot")

  # Counts mode
  p_counts <- cliplot(type = type_edge_dynamics(mode = "counts"))
  expect_s3_class(p_counts, "ggplot")

  # Character alias
  p_alias <- cliplot(type = "edge_dynamics")
  expect_s3_class(p_alias, "ggplot")
})
