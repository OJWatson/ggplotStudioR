test_that("demo journey returns deterministic ordered steps", {
  p <- ggplotStudioR:::demo_reference_plot()
  journey <- ggplotStudioR:::build_demo_code_journey("base_plot", plot = p, mode = "additive")

  expect_length(journey, 5)
  expect_equal(journey[[1]]$step_name, "Initial state")
  expect_equal(journey[[5]]$step_name, "Layer tuning")
  expect_match(journey[[2]]$code, "MPG vs Weight")
  expect_match(journey[[3]]$code, "panel.background = ggplot2::element_rect")
})

test_that("demo plot journey returns ggplot objects", {
  p <- ggplot2::ggplot(mtcars, ggplot2::aes(wt, mpg, color = factor(cyl))) +
    ggplot2::geom_point() +
    ggplot2::geom_smooth(se = FALSE)

  plots <- ggplotStudioR:::build_demo_plot_journey(p)

  expect_length(plots, 5)
  expect_true(all(vapply(plots, inherits, logical(1), what = "ggplot")))
})
