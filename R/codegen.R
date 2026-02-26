studio_theme_entry_code <- function(element, entry) {
  type <- studio_theme_element_type(element)

  if (isTRUE(entry$blank)) {
    return(sprintf("%s = ggplot2::element_blank()", element))
  }

  if (!studio_theme_entry_modified(entry)) {
    return(NULL)
  }

  args <- list()
  if (type == "text") {
    if (!is.null(entry$colour)) args$colour <- entry$colour
    if (!is.null(entry$size)) args$size <- entry$size
    if (!is.null(entry$face)) args$face <- entry$face
    fn <- "ggplot2::element_text"
  } else if (type == "rect") {
    if (!is.null(entry$fill)) args$fill <- entry$fill
    if (!is.null(entry$colour)) args$colour <- entry$colour
    if (!is.null(entry$linewidth)) args$linewidth <- entry$linewidth
    if (!is.null(entry$linetype)) args$linetype <- entry$linetype
    fn <- "ggplot2::element_rect"
  } else {
    if (!is.null(entry$colour)) args$colour <- entry$colour
    if (!is.null(entry$linewidth)) args$linewidth <- entry$linewidth
    if (!is.null(entry$linetype)) args$linetype <- entry$linetype
    fn <- "ggplot2::element_line"
  }

  if (length(args) == 0) {
    return(NULL)
  }

  arg_code <- paste(
    sprintf("%s = %s", names(args), vapply(args, studio_code, character(1))),
    collapse = ", "
  )
  sprintf("%s = %s(%s)", element, fn, arg_code)
}

studio_theme_code <- function(spec) {
  entries <- lapply(names(spec$theme), function(el) {
    studio_theme_entry_code(el, spec$theme[[el]])
  })
  entries <- Filter(Negate(is.null), entries)

  if (length(entries) == 0) {
    return(NULL)
  }

  paste0("ggplot2::theme(\n  ", paste(entries, collapse = ",\n  "), "\n)")
}

studio_labs_code <- function(spec) {
  sprintf(
    "ggplot2::labs(title = %s, subtitle = %s, caption = %s, x = %s, y = %s)",
    studio_code(spec$labels$title),
    studio_code(spec$labels$subtitle),
    studio_code(spec$labels$caption),
    studio_code(spec$labels$x),
    studio_code(spec$labels$y)
  )
}

studio_layer_assignment_code <- function(object_name, spec) {
  lines <- c()

  for (i in seq_along(spec$layers)) {
    layer_spec <- spec$layers[[i]]
    for (aes_name in c("alpha", "size", "linewidth", "linetype")) {
      value <- layer_spec$aes_params[[aes_name]]
      if (!is.null(value)) {
        lines <- c(
          lines,
          sprintf("%s$layers[[%d]]$aes_params$%s <- %s", object_name, i, aes_name, studio_code(value))
        )
      }
    }
  }

  keep <- which(vapply(spec$layers, function(layer) isTRUE(layer$visible), logical(1)))
  if (length(keep) < length(spec$layers)) {
    keep_expr <- if (length(keep) == 0) {
      "integer(0)"
    } else {
      sprintf("c(%s)", paste(keep, collapse = ", "))
    }

    lines <- c(lines, sprintf("%s$layers <- %s$layers[%s]", object_name, object_name, keep_expr))
  }

  lines
}

codegen_additive <- function(base_plot_expr, spec) {
  studio_spec_validate(spec)

  lines <- c(
    sprintf("edited_plot <- %s", base_plot_expr),
    sprintf("edited_plot <- edited_plot + %s", studio_labs_code(spec))
  )

  theme_code <- studio_theme_code(spec)
  if (!is.null(theme_code)) {
    lines <- c(lines, sprintf("edited_plot <- edited_plot + %s", theme_code))
  }

  lines <- c(lines, studio_layer_assignment_code("edited_plot", spec), "edited_plot")

  paste(lines, collapse = "\n")
}

codegen_patch <- function(base_plot_expr, spec) {
  studio_spec_validate(spec)

  object_name <- base_plot_expr
  lines <- c(
    sprintf("%s <- %s + %s", object_name, object_name, studio_labs_code(spec))
  )

  theme_code <- studio_theme_code(spec)
  if (!is.null(theme_code)) {
    lines <- c(lines, sprintf("%s <- %s + %s", object_name, object_name, theme_code))
  }

  lines <- c(lines, studio_layer_assignment_code(object_name, spec), object_name)
  paste(lines, collapse = "\n")
}

is_legacy_controls <- function(x) {
  is.list(x) && all(c("title", "subtitle", "caption", "x_label", "y_label") %in% names(x))
}

generate_plot_code_legacy <- function(base_plot_expr = "p", controls) {
  layers <- c(
    sprintf(
      "ggplot2::labs(title = %s, subtitle = %s, caption = %s, x = %s, y = %s)",
      studio_code(controls$title),
      studio_code(controls$subtitle),
      studio_code(controls$caption),
      studio_code(controls$x_label),
      studio_code(controls$y_label)
    )
  )

  if (!is.null(controls$theme) && !identical(controls$theme, "")) {
    layers <- c(layers, sprintf("ggplot2::theme_%s()", controls$theme))
  }

  code <- paste(c(base_plot_expr, paste0("  ", layers)), collapse = " +\n")
  notes <- c(
    if (!is.null(controls$palette) && !controls$palette %in% c("none", "hue")) {
      sprintf("# Palette requested: %s", controls$palette)
    },
    sprintf("# Geom size intent: point_size = %s, line_size = %s", controls$point_size %||% "NA", controls$line_size %||% "NA")
  )

  paste(c(code, notes), collapse = "\n")
}

#' Generate reproducible edit code
#'
#' @param base_plot_expr Character scalar naming the source plot object/expression.
#' @param spec Edit spec created by [studio_spec_init()] (or legacy controls list).
#' @param mode Export mode: `"additive"` creates a new `edited_plot`; `"patch"`
#'   mutates the supplied plot symbol.
#' @return Character scalar containing executable R code.
#' @keywords internal
generate_plot_code <- function(base_plot_expr = "p", spec, mode = c("additive", "patch")) {
  if (is_legacy_controls(spec)) {
    return(generate_plot_code_legacy(base_plot_expr = base_plot_expr, controls = spec))
  }

  mode <- match.arg(mode)

  if (identical(mode, "additive")) {
    return(codegen_additive(base_plot_expr = base_plot_expr, spec = spec))
  }

  codegen_patch(base_plot_expr = base_plot_expr, spec = spec)
}
