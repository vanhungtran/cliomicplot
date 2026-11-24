test_that("cli_env_network_data produces valid data object", {
  d <- cli_env_network_data()
  expect_s3_class(d, "cli_env_network_data")
  expect_true(is.list(d))
  expect_true(!is.null(d$nodes))
  expect_true(!is.null(d$edges))
  expect_true(!is.null(d$hulls))
  expect_true(!is.null(d$tiles))
  expect_true(!is.null(d$diag))
  expect_true(!is.null(d$env_labels))
  expect_true(!is.null(d$links))
  expect_true(!is.null(d$center_ring))
  expect_true(!is.null(d$outer_ring_nodes))
  expect_true(nrow(d$nodes) > 0)
  expect_true(nrow(d$tiles) > 0)
  expect_true(nrow(d$links) > 0)
})

test_that("cli_env_network_data computes empirical links from comm_data", {
  set.seed(42)
  comm <- matrix(rpois(50, lambda = 10), nrow = 10, ncol = 5,
                 dimnames = list(paste0("Sample_", 1:10), paste0("Taxon_", 1:5)))
  env1 <- matrix(c(1.0, 0.45, 0.45, 1.0), nrow = 2, ncol = 2,
                 dimnames = list(c("pH", "Temp"), c("pH", "Temp")))
  env_list <- list("Soil" = env1)

  d <- cli_env_network_data(comm_data = comm, env_data = env_list)
  expect_s3_class(d, "cli_env_network_data")
  expect_true(nrow(d$links) > 0)
  expect_true("sign" %in% names(d$links))
})

test_that("individual grammar layers return ggplot layer lists", {
  d <- cli_env_network_data()

  l_bnd <- geom_env_boundary(data = d)
  expect_true(is.list(l_bnd))
  expect_true(length(l_bnd) >= 2)

  l_net <- geom_env_central_network(data = d)
  expect_true(is.list(l_net))
  expect_true(length(l_net) >= 3)

  l_net_alias <- geom_central_network(data = d)
  expect_true(is.list(l_net_alias))

  l_map <- geom_env_corner_heatmaps(data = d)
  expect_true(is.list(l_map))
  expect_true(length(l_map) >= 2)

  l_map_alias <- geom_env_heatmaps(data = d)
  expect_true(is.list(l_map_alias))

  l_lnk <- geom_env_links(data = d)
  expect_true(is.list(l_lnk))
  expect_true(length(l_lnk) >= 2)

  coord <- coord_env_network()
  expect_true(inherits(coord, "Coord"))

  thm <- theme_env_network(preset = "clinical")
  expect_true(is.list(thm))
  expect_true(length(thm) == 2)
})

test_that("layered grammar of graphics constructs full plot via + operator", {
  d <- cli_env_network_data(preset = "nature")

  p <- ggplot2::ggplot() +
    geom_env_boundary(data = d) +
    geom_env_central_network(data = d) +
    geom_env_corner_heatmaps(data = d) +
    geom_env_links(data = d) +
    coord_env_network() +
    theme_env_network(preset = "nature")

  expect_s3_class(p, "ggplot")
  g <- ggplot2::ggplotGrob(p)
  expect_true(!is.null(g))
})

test_that("S3 plot and print methods for cli_env_network_data work", {
  d <- cli_env_network_data()
  p <- plot(d)
  expect_s3_class(p, "ggplot")

  # print invisible check
  res <- utils::capture.output({
    out <- print(d)
  })
  expect_identical(out, d)
})

test_that("formula syntax cliplot(env ~ comm, type = 'env_network') works", {
  set.seed(123)
  comm <- matrix(rpois(40, lambda = 8), nrow = 10, ncol = 4,
                 dimnames = list(paste0("S", 1:10), paste0("OTU", 1:4)))
  env_mat <- list(
    "Domain_A" = matrix(c(1, 0.5, 0.5, 1), 2, 2, dimnames = list(c("Var1", "Var2"), c("Var1", "Var2"))),
    "Domain_B" = matrix(c(1, -0.3, -0.3, 1), 2, 2, dimnames = list(c("Var3", "Var4"), c("Var3", "Var4")))
  )

  p <- cliplot(env_mat ~ comm, type = "env_network", preset = "clinical")
  expect_s3_class(p, "ggplot")
})
