test_that("generate_plot_code includes core labelling and theme", {
  controls <- list(
    title = "A",
    subtitle = "B",
    caption = "C",
    x_label = "X",
    y_label = "Y",
    theme = "minimal",
    palette = "hue",
    point_size = 2,
    line_size = 1
  )

  code <- ggplotStudioR:::generate_plot_code("p", controls)

  expect_match(code, "ggplot2::labs\\(title = \"A\"")
  expect_match(code, "ggplot2::theme_minimal\\(\\)")
  expect_false(grepl("scale_color_brewer", code, fixed = TRUE))
})

test_that("generate_plot_code includes requested palette and sizes", {
  controls <- list(
    title = "",
    subtitle = "",
    caption = "",
    x_label = "",
    y_label = "",
    theme = "gray",
    palette = "Set1",
    point_size = 3.5,
    line_size = 1.2
  )

  code <- ggplotStudioR:::generate_plot_code("base_plot", controls)

  expect_match(code, "scale_color_brewer")
  expect_match(code, "scale_fill_brewer")
  expect_match(code, "point_size = 3.5")
  expect_match(code, "line_size = 1.2")
})

test_that("build_plot_from_controls applies labels and size params", {
  p <- ggplot2::ggplot(mtcars, ggplot2::aes(wt, mpg, color = factor(cyl))) +
    ggplot2::geom_point() +
    ggplot2::geom_line()

  controls <- list(
    title = "Title",
    subtitle = "Sub",
    caption = "Cap",
    x_label = "Weight",
    y_label = "MPG",
    theme = "bw",
    palette = "Dark2",
    point_size = 4,
    line_size = 2
  )

  edited <- ggplotStudioR:::build_plot_from_controls(p, controls)

  expect_equal(edited$labels$title, "Title")
  expect_equal(edited$labels$x, "Weight")
  expect_equal(edited$layers[[1]]$aes_params$size, 4)
  expect_equal(edited$layers[[2]]$aes_params$linewidth, 2)
})
