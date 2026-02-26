demo_reference_plot <- function() {
  ggplot2::ggplot(datasets::mtcars, ggplot2::aes(wt, mpg, colour = factor(cyl))) +
    ggplot2::geom_point() +
    ggplot2::geom_smooth(se = FALSE)
}

demo_spec_changes <- function() {
  list(
    list(step = "Initial state", apply = function(spec) spec),
    list(
      step = "Edit labels",
      apply = function(spec) {
        spec <- studio_spec_update_label(spec, "title", "MPG vs Weight")
        spec <- studio_spec_update_label(spec, "subtitle", "Interactive selection + inspector")
        spec <- studio_spec_update_label(spec, "x", "Weight (1000 lbs)")
        spec <- studio_spec_update_label(spec, "y", "Miles per gallon")
        studio_spec_update_label(spec, "caption", "Edited in ggplotStudioR")
      }
    ),
    list(
      step = "Style panel + title",
      apply = function(spec) {
        spec <- studio_spec_update_theme(spec, "panel.background", "fill", "#f7f7f7")
        spec <- studio_spec_update_theme(spec, "panel.background", "colour", "#d9d9d9")
        spec <- studio_spec_update_theme(spec, "plot.title", "colour", "#2c3e50")
        studio_spec_update_theme(spec, "plot.title", "face", "bold")
      }
    ),
    list(
      step = "Grid + legend cleanup",
      apply = function(spec) {
        spec <- studio_spec_set_theme_blank(spec, "panel.grid.minor", TRUE)
        spec <- studio_spec_update_theme(spec, "panel.grid.major", "colour", "#dddddd")
        studio_spec_update_theme(spec, "legend.title", "face", "bold")
      }
    ),
    list(
      step = "Layer tuning",
      apply = function(spec) {
        if (length(spec$layers) >= 1) {
          spec <- studio_spec_update_layer_aes(spec, 1, "alpha", 0.8)
          spec <- studio_spec_update_layer_aes(spec, 1, "size", 2.8)
        }
        if (length(spec$layers) >= 2) {
          spec <- studio_spec_update_layer_aes(spec, 2, "linewidth", 1.1)
          spec <- studio_spec_update_layer_aes(spec, 2, "linetype", "dashed")
        }
        spec
      }
    )
  )
}

build_demo_code_journey <- function(base_plot_expr = "p", plot = demo_reference_plot(), mode = "additive") {
  validate_ggplot(plot)

  steps <- demo_spec_changes()
  current <- studio_spec_init(plot)

  lapply(seq_along(steps), function(i) {
    current <<- steps[[i]]$apply(current)
    list(
      step_number = i,
      step_name = steps[[i]]$step,
      spec = current,
      code = generate_plot_code(base_plot_expr = base_plot_expr, spec = current, mode = mode)
    )
  })
}

build_demo_plot_journey <- function(plot) {
  validate_ggplot(plot)

  steps <- demo_spec_changes()
  current <- studio_spec_init(plot)

  lapply(steps, function(step) {
    current <<- step$apply(current)
    apply_studio_spec(plot, current)
  })
}
