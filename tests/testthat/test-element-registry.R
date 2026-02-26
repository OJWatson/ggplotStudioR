test_that("element registry includes key selectable targets", {
  p <- ggplot2::ggplot(mtcars, ggplot2::aes(wt, mpg, colour = factor(cyl))) +
    ggplot2::geom_point() +
    ggplot2::geom_smooth(se = FALSE) +
    ggplot2::labs(
      title = "T",
      subtitle = "S",
      caption = "C",
      x = "Weight",
      y = "MPG",
      colour = "Cylinder"
    )

  reg <- ggplotStudioR:::build_element_registry(p, width_px = 900, height_px = 600)

  expect_true(all(c(
    "plot.title", "plot.subtitle", "plot.caption",
    "axis.title.x", "axis.title.y",
    "panel.background", "plot.background",
    "panel.grid.major", "panel.grid.minor",
    "legend.container", "legend.title", "legend.text",
    "layer:1"
  ) %in% reg$key))
})

test_that("layer registry preserves original indices after hidden layers", {
  p <- ggplot2::ggplot(mtcars, ggplot2::aes(wt, mpg, colour = factor(cyl))) +
    ggplot2::geom_point() +
    ggplot2::geom_smooth(se = FALSE)

  spec <- ggplotStudioR:::studio_spec_init(p)
  spec <- ggplotStudioR:::studio_spec_set_layer_visible(spec, 1, FALSE)
  edited <- ggplotStudioR:::apply_studio_spec(p, spec)

  reg <- ggplotStudioR:::build_element_registry(edited, width_px = 900, height_px = 600)
  expect_true("layer:2" %in% reg$key)
  expect_false("layer:1" %in% reg$key)
})

test_that("resolve_registry_selection cycles overlapping candidates", {
  reg <- data.frame(
    key = c("legend.container", "legend.title", "legend.text"),
    label = c("Legend container", "Legend title", "Legend text"),
    type = c("rect", "text", "text"),
    x_min = c(0.1, 0.1, 0.1),
    x_max = c(0.3, 0.3, 0.3),
    y_min = c(0.1, 0.1, 0.1),
    y_max = c(0.3, 0.3, 0.3),
    stringsAsFactors = FALSE
  )

  first <- ggplotStudioR:::resolve_registry_selection(reg, 0.2, 0.2)
  second <- ggplotStudioR:::resolve_registry_selection(reg, 0.2, 0.2, current_key = first)

  expect_equal(first, "legend.container")
  expect_equal(second, "legend.title")
})
