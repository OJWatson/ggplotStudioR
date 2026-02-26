studio_theme_entry_modified <- function(entry) {
  if (isTRUE(entry$blank)) {
    return(TRUE)
  }

  any(!vapply(entry[names(entry) != "blank"], is.null, logical(1)))
}

studio_build_theme_element <- function(element, entry) {
  type <- studio_theme_element_type(element)

  if (isTRUE(entry$blank)) {
    return(ggplot2::element_blank())
  }

  if (!studio_theme_entry_modified(entry)) {
    return(NULL)
  }

  if (type == "text") {
    args <- list()
    if (!is.null(entry$colour)) args$colour <- entry$colour
    if (!is.null(entry$size)) args$size <- entry$size
    if (!is.null(entry$face)) args$face <- entry$face
    return(do.call(ggplot2::element_text, args))
  }

  if (type == "rect") {
    args <- list()
    if (!is.null(entry$fill)) args$fill <- entry$fill
    if (!is.null(entry$colour)) args$colour <- entry$colour
    if (!is.null(entry$linewidth)) args$linewidth <- entry$linewidth
    if (!is.null(entry$linetype)) args$linetype <- entry$linetype
    return(do.call(ggplot2::element_rect, args))
  }

  args <- list()
  if (!is.null(entry$colour)) args$colour <- entry$colour
  if (!is.null(entry$linewidth)) args$linewidth <- entry$linewidth
  if (!is.null(entry$linetype)) args$linetype <- entry$linetype
  do.call(ggplot2::element_line, args)
}

apply_studio_spec <- function(plot, spec) {
  validate_ggplot(plot)
  studio_spec_validate(spec, plot)

  edited <- plot + do.call(ggplot2::labs, spec$labels)

  theme_args <- list()
  for (element in names(spec$theme)) {
    built <- studio_build_theme_element(element, spec$theme[[element]])
    if (!is.null(built)) {
      theme_args[[element]] <- built
    }
  }

  if (length(theme_args) > 0) {
    edited <- edited + do.call(ggplot2::theme, theme_args)
  }

  updated_layers <- edited$layers
  keep <- rep(TRUE, length(updated_layers))

  for (i in seq_along(spec$layers)) {
    layer_spec <- spec$layers[[i]]
    keep[i] <- isTRUE(layer_spec$visible)

    if (i > length(updated_layers)) {
      next
    }

    layer <- updated_layers[[i]]
    for (aes_name in c("alpha", "size", "linewidth", "linetype")) {
      value <- layer_spec$aes_params[[aes_name]]
      if (!is.null(value)) {
        layer$aes_params[[aes_name]] <- value
      }
    }

    updated_layers[[i]] <- layer
  }

  visible_idx <- which(keep)
  edited$layers <- updated_layers[visible_idx]
  attr(edited, "studio_layer_index_map") <- visible_idx
  edited
}

preview_highlight_selection <- function(plot, selected_key) {
  if (is.null(selected_key)) {
    return(plot)
  }

  desc <- selection_descriptor(selected_key)
  if (is.null(desc)) {
    return(plot)
  }

  if (identical(desc$kind, "layer")) {
    idx <- desc$layer_index
    if (idx < 1L || idx > length(plot$layers)) {
      return(plot)
    }

    for (i in seq_along(plot$layers)) {
      if (i == idx) {
        if (is.null(plot$layers[[i]]$aes_params$alpha)) {
          plot$layers[[i]]$aes_params$alpha <- 1
        }
        if (!is.null(plot$layers[[i]]$aes_params$size)) {
          plot$layers[[i]]$aes_params$size <- plot$layers[[i]]$aes_params$size * 1.2
        }
        if (!is.null(plot$layers[[i]]$aes_params$linewidth)) {
          plot$layers[[i]]$aes_params$linewidth <- plot$layers[[i]]$aes_params$linewidth * 1.2
        }
      } else {
        if (is.null(plot$layers[[i]]$aes_params$alpha)) {
          plot$layers[[i]]$aes_params$alpha <- 0.25
        } else {
          plot$layers[[i]]$aes_params$alpha <- max(0.05, 0.25 * plot$layers[[i]]$aes_params$alpha)
        }
      }
    }

    return(plot)
  }

  if (is.null(desc$theme_element)) {
    return(plot)
  }

  accent <- "#d7301f"
  el <- desc$theme_element
  style <- desc$style %||% "text"

  if (style == "text") {
    return(plot + do.call(ggplot2::theme, stats::setNames(list(ggplot2::element_text(colour = accent)), el)))
  }

  if (style == "rect") {
    return(plot + do.call(ggplot2::theme, stats::setNames(list(ggplot2::element_rect(colour = accent, linewidth = 1.3)), el)))
  }

  plot + do.call(ggplot2::theme, stats::setNames(list(ggplot2::element_line(colour = accent, linewidth = 1.1)), el))
}
