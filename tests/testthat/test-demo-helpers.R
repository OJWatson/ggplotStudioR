test_that("demo journey returns deterministic ordered steps", {
  journey <- ggplotStudioR:::build_demo_code_journey("base_plot")

  expect_length(journey, 5)
  expect_equal(journey[[1]]$step_name, "Initial state")
  expect_equal(journey[[5]]$controls$caption, "Edited in ggplotStudioR")
  expect_match(journey[[2]]$code, "title = \"MPG vs Weight\"")
  expect_match(journey[[4]]$code, "scale_color_brewer")
})

test_that("demo plot journey returns ggplot objects", {
  p <- ggplot2::ggplot(mtcars, ggplot2::aes(wt, mpg, color = factor(cyl))) +
    ggplot2::geom_point() +
    ggplot2::geom_smooth(se = FALSE)

  plots <- ggplotStudioR:::build_demo_plot_journey(p)

  expect_length(plots, 5)
  expect_true(all(vapply(plots, inherits, logical(1), what = "ggplot")))
})
