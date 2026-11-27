test_that("type_bubble_heatmap supports diamond, circle, square, triangle, and diverging forms", {
  test_df <- data.frame(
    Gene = rep(c("IL4", "IL13", "IL22", "IFNG", "TNF"), each = 3),
    Condition = rep(c("Mild", "Moderate", "Severe"), 5),
    FC = c(1.2, 2.5, 3.8, 0.8, 1.9, 3.2, 0.4, 1.1, 2.4, -0.5, 0.8, 1.6, 0.2, 0.9, 2.1),
    PVal = c(3.2, 6.8, 12.1, 2.1, 5.4, 9.8, 1.2, 3.5, 7.2, 0.8, 2.4, 4.6, 0.5, 2.1, 5.9)
  )

  # 1. Classic Diamond form
  p_diamond <- cliplot(Gene ~ Condition, data = test_df, by = test_df$FC,
                       type = type_bubble_diamond(max_size = 8),
                       extra_data = list(fill = test_df$PVal))
  expect_s3_class(p_diamond, "ggplot")

  # 2. Circle form with minimal grid
  p_circle <- cliplot(Gene ~ Condition, data = test_df, by = test_df$PVal,
                      type = type_bubble_circle(max_size = 7),
                      extra_data = list(fill = test_df$FC))
  expect_s3_class(p_circle, "ggplot")

  # 3. Square tile form with grid
  p_square <- cliplot(Gene ~ Condition, data = test_df, by = abs(test_df$FC),
                      type = type_bubble_square(max_size = 8),
                      extra_data = list(fill = test_df$PVal))
  expect_s3_class(p_square, "ggplot")

  # 4. Diverging NES / log2FC form
  p_div <- cliplot(Gene ~ Condition, data = test_df, by = test_df$PVal,
                   type = type_bubble_diverging(palette = "nature", midpoint = 0),
                   extra_data = list(fill = test_df$FC))
  expect_s3_class(p_div, "ggplot")

  # 5. Significance asterisks overlay form
  p_signif <- cliplot(Gene ~ Condition, data = test_df, by = abs(test_df$FC),
                      type = type_bubble_heatmap(
                        shape = "circle",
                        label_metric = "signif",
                        signif_cutoffs = c(0.05, 0.01, 0.001)
                      ),
                      extra_data = list(fill = test_df$PVal))
  expect_s3_class(p_signif, "ggplot")

  # 6. Triangle up form
  p_tri <- cliplot(Gene ~ Condition, data = test_df, by = abs(test_df$FC),
                   type = type_bubble_heatmap(shape = "triangle_up"),
                   extra_data = list(fill = test_df$PVal))
  expect_s3_class(p_tri, "ggplot")
})
