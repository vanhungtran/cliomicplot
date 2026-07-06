# ===========================================================================
# Theme & palette tests: clitheme, clipar, palettes, markdown
# ===========================================================================

# ---- Themes ----

test_that("clitheme sets and retrieves theme", {
  clitheme("nature")
  expect_equal(get_environment_variable(".active_theme"), "nature")
  clitheme()  # reset
})

test_that("clitheme_list returns all themes", {
  themes <- clitheme_list()
  expect_type(themes, "character")
  expect_true("cli_bw" %in% themes)
  expect_true("nature" %in% themes)
  expect_true("nejm" %in% themes)
  expect_true("science" %in% themes)
  expect_true("lancet" %in% themes)
  expect_true("cell" %in% themes)
  expect_true("cli_minimal" %in% themes)
  expect_true("cli_classic" %in% themes)
  expect_true("dark" %in% themes)
  expect_true("showcase" %in% themes)
  expect_true("broadsheet" %in% themes)
})

test_that("clitheme_register adds a custom theme", {
  # Register a variation of an existing theme
  clitheme_register("test_custom", "cli_bw", overwrite = TRUE)
  expect_true("test_custom" %in% clitheme_list())
  clitheme_unregister("test_custom")
})

test_that("unknown theme throws an error", {
  expect_error(clitheme("nonexistent_theme_xyz"), "Unknown theme")
})

# ---- Palettes ----

test_that("cli_palette_list returns palettes", {
  pals <- cli_palette_list()
  expect_type(pals, "character")
  expect_true("jco" %in% pals)
  expect_true("nejm" %in% pals)
  expect_true("nature" %in% pals)
  expect_true("okabe_ito" %in% pals)
})

test_that("get_cli_palette returns correct number of colors", {
  pal <- get_cli_palette("jco", 5)
  expect_length(pal, 5)
  expect_true(all(grepl("^#[0-9A-Fa-f]{6}$", pal)))
})

test_that("palette_scale works with theme", {
  p <- ggplot2::ggplot(mtcars, ggplot2::aes(wt, mpg, color = factor(cyl))) +
    ggplot2::geom_point() +
    palette_scale("npg", "color")
  expect_s3_class(p, "ggplot")
})

# ---- clipar ----

test_that("clipar sets and gets parameters", {
  old <- clipar("stat.test")
  clipar(stat.test = "t.test")
  expect_equal(clipar("stat.test"), "t.test")
  clipar(stat.test = old)  # restore
})

test_that("clipar returns all params when called without args", {
  params <- clipar()
  expect_type(params, "list")
  expect_true("stat.test" %in% names(params))
})

# ---- cli_markdown ----

test_that("cli_markdown returns a theme object", {
  md <- cli_markdown()
  expect_s3_class(md, "theme")
  expect_s3_class(md, "gg")
})

test_that("cli_markdown targets can be specified", {
  md <- cli_markdown("title")
  expect_s3_class(md, "theme")

  md2 <- cli_markdown("all")
  expect_s3_class(md2, "theme")
})

test_that("clitheme_md sets global markdown", {
  clitheme_md("title")
  expect_s3_class(ggplot2::theme_get(), "theme")
  clitheme_md_reset()
})
