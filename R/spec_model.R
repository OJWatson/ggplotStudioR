validate_ggplot <- function(plot) {
  if (!inherits(plot, "ggplot")) {
    stop("`plot` must be a ggplot object.", call. = FALSE)
  }
}

studio_theme_element_type <- function(element) {
  if (element %in% c(
    "plot.title", "plot.subtitle", "plot.caption",
    "axis.title.x", "axis.title.y",
    "legend.title", "legend.text"
  )) {
    return("text")
  }

  if (element %in% c("panel.background", "plot.background", "legend.background")) {
    return("rect")
  }

  if (element %in% c("panel.grid.major", "panel.grid.minor")) {
    return("line")
  }

  stop(sprintf("Unsupported theme element: %s", element), call. = FALSE)
}

studio_supported_theme_elements <- function() {
  c(
    "plot.title", "plot.subtitle", "plot.caption",
    "axis.title.x", "axis.title.y",
    "panel.background", "plot.background",
    "panel.grid.major", "panel.grid.minor",
    "legend.background", "legend.title", "legend.text"
  )
}

studio_default_theme_entry <- function(element) {
  type <- studio_theme_element_type(element)

  if (type == "text") {
    return(list(blank = FALSE, colour = NULL, size = NULL, face = NULL))
  }

  if (type == "rect") {
    return(list(blank = FALSE, fill = NULL, colour = NULL, linewidth = NULL, linetype = NULL))
  }

  list(blank = FALSE, colour = NULL, linewidth = NULL, linetype = NULL)
}

studio_layer_default <- function(layer, index) {
  list(
    index = index,
    visible = TRUE,
    aes_params = list(
      alpha = layer$aes_params$alpha %||% NULL,
      size = layer$aes_params$size %||% NULL,
      linewidth = layer$aes_params$linewidth %||% NULL,
      linetype = layer$aes_params$linetype %||% NULL
    )
  )
}

studio_spec_init <- function(plot) {
  validate_ggplot(plot)

  theme_entries <- studio_supported_theme_elements()
  theme <- stats::setNames(
    lapply(theme_entries, studio_default_theme_entry),
    theme_entries
  )

  list(
    version = 1L,
    labels = list(
      title = plot$labels$title %||% "",
      subtitle = plot$labels$subtitle %||% "",
      caption = plot$labels$caption %||% "",
      x = plot$labels$x %||% "",
      y = plot$labels$y %||% ""
    ),
    theme = theme,
    layers = lapply(seq_along(plot$layers), function(i) {
      studio_layer_default(plot$layers[[i]], i)
    })
  )
}

studio_spec_validate <- function(spec, plot = NULL) {
  required_names <- c("version", "labels", "theme", "layers")
  if (!all(required_names %in% names(spec))) {
    stop("Spec is missing required top-level fields.", call. = FALSE)
  }

  if (!all(c("title", "subtitle", "caption", "x", "y") %in% names(spec$labels))) {
    stop("Spec labels must include title/subtitle/caption/x/y.", call. = FALSE)
  }

  valid_theme <- studio_supported_theme_elements()
  if (!all(valid_theme %in% names(spec$theme))) {
    stop("Spec theme must include all supported theme element entries.", call. = FALSE)
  }

  if (!is.null(plot)) {
    validate_ggplot(plot)
    if (length(spec$layers) != length(plot$layers)) {
      stop("Spec layer entries must align with base plot layers.", call. = FALSE)
    }
  }

  invisible(TRUE)
}

studio_spec_update_label <- function(spec, field, value) {
  if (!field %in% c("title", "subtitle", "caption", "x", "y")) {
    stop("Unsupported label field.", call. = FALSE)
  }

  spec$labels[[field]] <- value %||% ""
  spec
}

studio_spec_set_theme_blank <- function(spec, element, blank) {
  studio_theme_element_type(element)
  spec$theme[[element]]$blank <- isTRUE(blank)
  spec
}

studio_spec_update_theme <- function(spec, element, property, value) {
  type <- studio_theme_element_type(element)

  valid_props <- switch(
    type,
    text = c("colour", "size", "face"),
    rect = c("fill", "colour", "linewidth", "linetype"),
    line = c("colour", "linewidth", "linetype")
  )

  if (!property %in% valid_props) {
    stop(sprintf("Property '%s' is not valid for %s.", property, element), call. = FALSE)
  }

  spec$theme[[element]][[property]] <- studio_null_if_empty(value)
  spec
}

studio_spec_set_layer_visible <- function(spec, layer_index, visible) {
  if (layer_index < 1L || layer_index > length(spec$layers)) {
    stop("Invalid layer index.", call. = FALSE)
  }

  spec$layers[[layer_index]]$visible <- isTRUE(visible)
  spec
}

studio_spec_update_layer_aes <- function(spec, layer_index, aes_name, value) {
  if (layer_index < 1L || layer_index > length(spec$layers)) {
    stop("Invalid layer index.", call. = FALSE)
  }

  if (!aes_name %in% c("alpha", "size", "linewidth", "linetype")) {
    stop("Unsupported layer aes parameter.", call. = FALSE)
  }

  spec$layers[[layer_index]]$aes_params[[aes_name]] <- studio_null_if_empty(value)
  spec
}

selection_descriptor <- function(key) {
  if (is.null(key) || !nzchar(key)) {
    return(NULL)
  }

  map <- list(
    "plot.title" = list(label = "Plot title", label_field = "title", theme_element = "plot.title", style = "text"),
    "plot.subtitle" = list(label = "Plot subtitle", label_field = "subtitle", theme_element = "plot.subtitle", style = "text"),
    "plot.caption" = list(label = "Plot caption", label_field = "caption", theme_element = "plot.caption", style = "text"),
    "axis.title.x" = list(label = "X axis title", label_field = "x", theme_element = "axis.title.x", style = "text"),
    "axis.title.y" = list(label = "Y axis title", label_field = "y", theme_element = "axis.title.y", style = "text"),
    "panel.background" = list(label = "Panel background", theme_element = "panel.background", style = "rect"),
    "plot.background" = list(label = "Plot background", theme_element = "plot.background", style = "rect"),
    "panel.grid.major" = list(label = "Major grid lines", theme_element = "panel.grid.major", style = "line"),
    "panel.grid.minor" = list(label = "Minor grid lines", theme_element = "panel.grid.minor", style = "line"),
    "legend.container" = list(label = "Legend container", theme_element = "legend.background", style = "rect"),
    "legend.title" = list(label = "Legend title", theme_element = "legend.title", style = "text"),
    "legend.text" = list(label = "Legend text", theme_element = "legend.text", style = "text")
  )

  if (startsWith(key, "layer:")) {
    idx <- suppressWarnings(as.integer(sub("^layer:", "", key)))
    if (!is.na(idx) && idx > 0L) {
      return(list(
        key = key,
        kind = "layer",
        layer_index = idx,
        label = sprintf("Layer %d", idx)
      ))
    }
  }

  if (!key %in% names(map)) {
    return(NULL)
  }

  desc <- map[[key]]
  desc$key <- key
  desc$kind <- "theme"
  desc
}
