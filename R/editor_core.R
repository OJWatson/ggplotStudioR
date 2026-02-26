# Core helpers for applying UI state to a ggplot and producing reproducible code.

.palette_choices <- c("none", "hue", "Set1", "Dark2", "viridis")
.theme_choices <- c("gray", "bw", "minimal", "classic", "light", "dark", "void")

validate_ggplot <- function(plot) {
  if (!inherits(plot, "ggplot")) {
    stop("`plot` must be a ggplot object.", call. = FALSE)
  }
}

is_mapped_aes <- function(plot, aes_name) {
  global_map <- names(plot$mapping)
  if (aes_name %in% global_map) {
    return(TRUE)
  }

  any(vapply(plot$layers, function(layer) {
    aes_name %in% names(layer$mapping)
  }, logical(1)))
}

apply_theme_preset <- function(plot, preset) {
  theme_layer <- switch(
    preset,
    gray = ggplot2::theme_gray(),
    bw = ggplot2::theme_bw(),
    minimal = ggplot2::theme_minimal(),
    classic = ggplot2::theme_classic(),
    light = ggplot2::theme_light(),
    dark = ggplot2::theme_dark(),
    void = ggplot2::theme_void(),
    ggplot2::theme_gray()
  )

  plot + theme_layer
}

apply_palette <- function(plot, palette_choice) {
  if (palette_choice %in% c("none", "hue")) {
    return(plot)
  }

  has_color <- is_mapped_aes(plot, "colour") || is_mapped_aes(plot, "color")
  has_fill <- is_mapped_aes(plot, "fill")

  if (palette_choice %in% c("Set1", "Dark2")) {
    if (has_color) {
      plot <- plot + ggplot2::scale_color_brewer(palette = palette_choice)
    }
    if (has_fill) {
      plot <- plot + ggplot2::scale_fill_brewer(palette = palette_choice)
    }
    return(plot)
  }

  if (palette_choice == "viridis") {
    if (has_color) {
      plot <- plot + ggplot2::scale_color_viridis_d()
    }
    if (has_fill) {
      plot <- plot + ggplot2::scale_fill_viridis_d()
    }
  }

  plot
}

apply_geom_sizes <- function(plot, point_size, line_size) {
  plot$layers <- lapply(plot$layers, function(layer) {
    geom_cls <- class(layer$geom)[1]

    if (identical(geom_cls, "GeomPoint")) {
      layer$aes_params$size <- point_size
    }

    if (geom_cls %in% c("GeomLine", "GeomPath", "GeomSmooth", "GeomSegment", "GeomCurve")) {
      layer$aes_params$linewidth <- line_size
    }

    layer
  })

  plot
}

build_plot_from_controls <- function(plot, controls) {
  validate_ggplot(plot)

  edited <- plot + ggplot2::labs(
    title = controls$title,
    subtitle = controls$subtitle,
    caption = controls$caption,
    x = controls$x_label,
    y = controls$y_label
  )

  edited <- apply_theme_preset(edited, controls$theme)

  if (!isTRUE(controls$palette == "hue")) {
    edited <- apply_palette(edited, controls$palette)
  }

  apply_geom_sizes(edited, controls$point_size, controls$line_size)
}

generate_plot_code <- function(base_plot_expr = "p", controls) {
  layers <- c(
    sprintf(
      "ggplot2::labs(title = %s, subtitle = %s, caption = %s, x = %s, y = %s)",
      deparse(controls$title),
      deparse(controls$subtitle),
      deparse(controls$caption),
      deparse(controls$x_label),
      deparse(controls$y_label)
    ),
    sprintf("ggplot2::theme_%s()", controls$theme)
  )

  palette_comment <- NULL
  if (controls$palette %in% c("Set1", "Dark2")) {
    palette_comment <- "# Add discrete Brewer palette when color/fill is mapped"
    layers <- c(
      layers,
      sprintf("ggplot2::scale_color_brewer(palette = %s)", deparse(controls$palette)),
      sprintf("ggplot2::scale_fill_brewer(palette = %s)", deparse(controls$palette))
    )
  } else if (controls$palette == "viridis") {
    palette_comment <- "# Add viridis discrete palette when color/fill is mapped"
    layers <- c(
      layers,
      "ggplot2::scale_color_viridis_d()",
      "ggplot2::scale_fill_viridis_d()"
    )
  }

  code <- paste(c(base_plot_expr, paste0("  ", layers)), collapse = " +\n")
  notes <- c(
    sprintf("# Geom size intent: point_size = %s, line_size = %s", controls$point_size, controls$line_size),
    "# Use explicit geom arguments in your source plot for persistence"
  )

  paste(c(code, palette_comment, notes), collapse = "\n")
}

editor_extensions <- function() {
  list(
    drag_handlers = list(),
    layer_transformers = list()
  )
}
