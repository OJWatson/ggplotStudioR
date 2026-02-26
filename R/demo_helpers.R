# Deterministic helper utilities used by docs/examples to show stepwise editing.

demo_default_controls <- function() {
  list(
    title = "",
    subtitle = "",
    caption = "",
    x_label = "",
    y_label = "",
    theme = "gray",
    palette = "hue",
    point_size = 2,
    line_size = 1
  )
}

demo_control_changes <- function() {
  list(
    list(step = "Initial state", updates = list()),
    list(
      step = "Update labels",
      updates = list(
        title = "MPG vs Weight",
        subtitle = "Exploring mtcars by cylinder",
        x_label = "Weight (1000 lbs)",
        y_label = "Miles per gallon"
      )
    ),
    list(step = "Switch to minimal theme", updates = list(theme = "minimal")),
    list(step = "Apply Set1 palette", updates = list(palette = "Set1")),
    list(
      step = "Increase geometry sizes and add caption",
      updates = list(
        point_size = 3.2,
        line_size = 1.4,
        caption = "Edited in ggplotStudioR"
      )
    )
  )
}

build_demo_code_journey <- function(base_plot_expr = "p") {
  steps <- demo_control_changes()
  current <- demo_default_controls()

  lapply(seq_along(steps), function(i) {
    updates <- steps[[i]]$updates
    if (length(updates) > 0) {
      current[names(updates)] <- updates
    }

    list(
      step_number = i,
      step_name = steps[[i]]$step,
      controls = current,
      code = generate_plot_code(base_plot_expr = base_plot_expr, controls = current)
    )
  })
}

build_demo_plot_journey <- function(plot) {
  validate_ggplot(plot)
  journey <- build_demo_code_journey(base_plot_expr = "p")

  lapply(journey, function(step) {
    build_plot_from_controls(plot, step$controls)
  })
}
