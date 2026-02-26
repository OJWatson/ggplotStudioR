test_that("spec model initializes from plot and validates", {
  p <- ggplot2::ggplot(mtcars, ggplot2::aes(wt, mpg, colour = factor(cyl))) +
    ggplot2::geom_point() +
    ggplot2::geom_smooth(se = FALSE)

  spec <- ggplotStudioR:::studio_spec_init(p)

  expect_true(ggplotStudioR:::studio_spec_validate(spec, p))
  expect_equal(spec$labels$title, "")
  expect_equal(length(spec$layers), 2)
  expect_true(all(c("plot.title", "panel.background", "legend.text") %in% names(spec$theme)))
})

test_that("apply_studio_spec mutates labels/theme and layer visibility", {
  p <- ggplot2::ggplot(mtcars, ggplot2::aes(wt, mpg, colour = factor(cyl))) +
    ggplot2::geom_point() +
    ggplot2::geom_smooth(se = FALSE)

  spec <- ggplotStudioR:::studio_spec_init(p)
  spec <- ggplotStudioR:::studio_spec_update_label(spec, "title", "Edited title")
  spec <- ggplotStudioR:::studio_spec_update_theme(spec, "panel.background", "fill", "#f0f0f0")
  spec <- ggplotStudioR:::studio_spec_set_theme_blank(spec, "panel.grid.minor", TRUE)
  spec <- ggplotStudioR:::studio_spec_update_layer_aes(spec, 1, "alpha", 0.6)
  spec <- ggplotStudioR:::studio_spec_set_layer_visible(spec, 2, FALSE)

  edited <- ggplotStudioR:::apply_studio_spec(p, spec)

  expect_equal(edited$labels$title, "Edited title")
  expect_equal(length(edited$layers), 1)
  expect_equal(edited$layers[[1]]$aes_params$alpha, 0.6)
  expect_true(inherits(edited$theme$panel.grid.minor, "element_blank"))
})

test_that("generate_plot_code supports additive and patch modes", {
  p <- ggplot2::ggplot(mtcars, ggplot2::aes(wt, mpg, colour = factor(cyl))) +
    ggplot2::geom_point() +
    ggplot2::geom_smooth(se = FALSE)

  spec <- ggplotStudioR:::studio_spec_init(p)
  spec <- ggplotStudioR:::studio_spec_update_label(spec, "caption", "hello")
  spec <- ggplotStudioR:::studio_spec_update_theme(spec, "legend.title", "face", "bold")
  spec <- ggplotStudioR:::studio_spec_update_layer_aes(spec, 2, "linewidth", 1.7)

  additive <- ggplotStudioR:::generate_plot_code("p", spec, mode = "additive")
  patch <- ggplotStudioR:::generate_plot_code("p", spec, mode = "patch")

  expect_match(additive, "edited_plot <- p")
  expect_match(additive, "ggplot2::labs")
  expect_match(additive, "legend.title = ggplot2::element_text")
  expect_match(additive, "layers\\[\\[2\\]\\]\\$aes_params\\$linewidth <- 1.7")

  expect_match(patch, "p <- p \\+ ggplot2::labs")
  expect_match(patch, "p\\$layers\\[\\[2\\]\\]\\$aes_params\\$linewidth <- 1.7", perl = TRUE)
})

test_that("legacy controls generation remains available", {
  controls <- list(
    title = "A",
    subtitle = "B",
    caption = "C",
    x_label = "X",
    y_label = "Y",
    theme = "minimal",
    palette = "Set1",
    point_size = 2,
    line_size = 1
  )

  code <- ggplotStudioR:::generate_plot_code("p", controls)
  expect_match(code, "theme_minimal")
  expect_match(code, "Palette requested: Set1")
})
