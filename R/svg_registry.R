render_plot_svg <- function(plot, width_px = 900, height_px = 600) {
  validate_ggplot(plot)

  svg_path <- tempfile(fileext = ".svg")
  grDevices::svg(
    filename = svg_path,
    width = width_px / 96,
    height = height_px / 96,
    bg = "white"
  )
  on.exit(grDevices::dev.off(), add = TRUE)
  print(plot)

  paste(readLines(svg_path, warn = FALSE), collapse = "\n")
}

studio_box_row <- function(key, label, type, box) {
  data.frame(
    key = key,
    label = label,
    type = type,
    x_min = box$x_min,
    x_max = box$x_max,
    y_min = box$y_min,
    y_max = box$y_max,
    stringsAsFactors = FALSE
  )
}

studio_layout_box <- function(grob, widths, heights, layout_names) {
  idx <- which(grob$layout$name %in% layout_names)
  if (length(idx) == 0) {
    return(NULL)
  }

  x_min <- min(vapply(idx, function(i) {
    l <- grob$layout$l[[i]]
    if (l <= 1) 0 else sum(widths[seq_len(l - 1)])
  }, numeric(1)))

  x_max <- max(vapply(idx, function(i) {
    r <- grob$layout$r[[i]]
    sum(widths[seq_len(r)])
  }, numeric(1)))

  y_min <- min(vapply(idx, function(i) {
    t <- grob$layout$t[[i]]
    if (t <= 1) 0 else sum(heights[seq_len(t - 1)])
  }, numeric(1)))

  y_max <- max(vapply(idx, function(i) {
    b <- grob$layout$b[[i]]
    sum(heights[seq_len(b)])
  }, numeric(1)))

  list(x_min = x_min, x_max = x_max, y_min = y_min, y_max = y_max)
}

studio_panel_range <- function(panel_params, axis = c("x", "y")) {
  axis <- match.arg(axis)

  direct <- panel_params[[paste0(axis, ".range")]]
  if (is.numeric(direct) && length(direct) == 2 && all(is.finite(direct))) {
    return(direct)
  }

  scale_obj <- panel_params[[axis]]
  if (!is.null(scale_obj$continuous_range)) {
    return(scale_obj$continuous_range)
  }

  ranged <- scale_obj$range$range
  if (is.numeric(ranged) && length(ranged) == 2) {
    return(ranged)
  }

  c(0, 1)
}

studio_rescale_clamped <- function(x, from) {
  if (length(from) != 2 || !all(is.finite(from)) || diff(from) == 0) {
    return(rep(0.5, length(x)))
  }

  y <- (x - from[1]) / diff(from)
  y[!is.finite(y)] <- NA_real_
  pmin(pmax(y, 0), 1)
}

build_layer_registry <- function(plot, panel_box) {
  built <- ggplot2::ggplot_build(plot)
  if (length(built$data) == 0) {
    return(data.frame())
  }

  panel_params <- built$layout$panel_params[[1]]
  x_range <- studio_panel_range(panel_params, "x")
  y_range <- studio_panel_range(panel_params, "y")

  rows <- list()
  panel_width <- panel_box$x_max - panel_box$x_min
  panel_height <- panel_box$y_max - panel_box$y_min

  layer_index_map <- attr(plot, "studio_layer_index_map")
  if (is.null(layer_index_map) || length(layer_index_map) != length(built$data)) {
    layer_index_map <- seq_along(built$data)
  }

  for (i in seq_along(built$data)) {
    layer_data <- built$data[[i]]

    x_cols <- intersect(c("x", "xmin", "xmax", "xend"), names(layer_data))
    y_cols <- intersect(c("y", "ymin", "ymax", "yend"), names(layer_data))

    if (length(x_cols) == 0 || length(y_cols) == 0 || nrow(layer_data) == 0) {
      layer_box <- panel_box
    } else {
      x_vals <- unlist(layer_data[x_cols], use.names = FALSE)
      y_vals <- unlist(layer_data[y_cols], use.names = FALSE)
      x_vals <- x_vals[is.finite(x_vals)]
      y_vals <- y_vals[is.finite(y_vals)]

      if (length(x_vals) == 0 || length(y_vals) == 0) {
        layer_box <- panel_box
      } else {
        x_npc <- studio_rescale_clamped(x_vals, x_range)
        y_npc_bottom <- studio_rescale_clamped(y_vals, y_range)

        x0 <- min(x_npc, na.rm = TRUE)
        x1 <- max(x_npc, na.rm = TRUE)
        y0_top <- 1 - max(y_npc_bottom, na.rm = TRUE)
        y1_top <- 1 - min(y_npc_bottom, na.rm = TRUE)

        layer_box <- list(
          x_min = panel_box$x_min + panel_width * x0,
          x_max = panel_box$x_min + panel_width * x1,
          y_min = panel_box$y_min + panel_height * y0_top,
          y_max = panel_box$y_min + panel_height * y1_top
        )
      }
    }

    original_idx <- layer_index_map[[i]]

    rows[[length(rows) + 1L]] <- studio_box_row(
      key = sprintf("layer:%d", original_idx),
      label = sprintf("Layer %d", original_idx),
      type = "layer",
      box = layer_box
    )
  }

  do.call(rbind, rows)
}

build_element_registry <- function(plot, width_px = 900, height_px = 600) {
  validate_ggplot(plot)

  grob <- ggplot2::ggplotGrob(plot)

  # NOTE: gtable units can collapse to zero without a fully resolved viewport.
  # We therefore use normalized layout-cell geometry for deterministic hit boxes.
  widths <- rep(1, length(grob$widths))
  heights <- rep(1, length(grob$heights))
  widths <- widths / sum(widths)
  heights <- heights / sum(heights)

  rows <- list()

  add_entry <- function(key, label, type, layout_names) {
    box <- studio_layout_box(grob, widths, heights, layout_names)
    if (is.null(box)) {
      return(invisible(NULL))
    }

    rows[[length(rows) + 1L]] <<- studio_box_row(key, label, type, box)
    invisible(NULL)
  }

  add_entry("plot.title", "Plot title", "text", "title")
  add_entry("plot.subtitle", "Plot subtitle", "text", "subtitle")
  add_entry("plot.caption", "Plot caption", "text", "caption")
  add_entry("axis.title.x", "X axis title", "text", c("xlab-b", "xlab-t"))
  add_entry("axis.title.y", "Y axis title", "text", c("ylab-l", "ylab-r"))

  panel_box <- studio_layout_box(grob, widths, heights, "panel")
  if (!is.null(panel_box)) {
    rows[[length(rows) + 1L]] <- studio_box_row("panel.background", "Panel background", "rect", panel_box)
    rows[[length(rows) + 1L]] <- studio_box_row("panel.grid.major", "Major grid lines", "line", panel_box)
    rows[[length(rows) + 1L]] <- studio_box_row("panel.grid.minor", "Minor grid lines", "line", panel_box)

    layer_rows <- build_layer_registry(plot, panel_box)
    if (nrow(layer_rows) > 0) {
      rows[[length(rows) + 1L]] <- layer_rows
    }
  }

  add_entry("plot.background", "Plot background", "rect", "background")

  legend_candidates <- grep("^guide-box", grob$layout$name, value = TRUE)
  if (length(legend_candidates) > 0) {
    boxes <- lapply(legend_candidates, function(name) {
      studio_layout_box(grob, widths, heights, name)
    })

    areas <- vapply(boxes, function(box) {
      if (is.null(box)) {
        return(0)
      }

      max(0, box$x_max - box$x_min) * max(0, box$y_max - box$y_min)
    }, numeric(1))

    if (any(areas > 0)) {
      legend_box <- boxes[[which.max(areas)]]
      rows[[length(rows) + 1L]] <- studio_box_row("legend.container", "Legend container", "rect", legend_box)
      rows[[length(rows) + 1L]] <- studio_box_row("legend.title", "Legend title", "text", legend_box)
      rows[[length(rows) + 1L]] <- studio_box_row("legend.text", "Legend text", "text", legend_box)
    }
  }

  out <- do.call(rbind, rows)
  if (is.null(out) || nrow(out) == 0) {
    return(data.frame(
      key = character(0),
      label = character(0),
      type = character(0),
      x_min = numeric(0),
      x_max = numeric(0),
      y_min = numeric(0),
      y_max = numeric(0),
      stringsAsFactors = FALSE
    ))
  }

  rownames(out) <- NULL
  out
}

build_svg_scene <- function(plot, width_px = 900, height_px = 600) {
  list(
    svg = render_plot_svg(plot, width_px = width_px, height_px = height_px),
    registry = build_element_registry(plot, width_px = width_px, height_px = height_px)
  )
}

resolve_registry_selection <- function(registry, x_npc, y_npc, current_key = NULL) {
  if (is.null(registry) || nrow(registry) == 0) {
    return(NULL)
  }

  hits <- registry[
    registry$x_min <= x_npc & registry$x_max >= x_npc &
      registry$y_min <= y_npc & registry$y_max >= y_npc,
    , drop = FALSE
  ]

  if (nrow(hits) == 0) {
    return(NULL)
  }

  candidates <- unique(hits$key)
  layer_hits <- candidates[startsWith(candidates, "layer:")]
  if (length(layer_hits) > 0) {
    layer_idx <- suppressWarnings(as.integer(sub("^layer:", "", layer_hits)))
    layer_hits <- layer_hits[order(layer_idx)]
  }

  preferred <- intersect(
    c(
      "plot.title", "plot.subtitle", "plot.caption",
      "axis.title.x", "axis.title.y",
      "legend.container", "legend.title", "legend.text",
      "panel.background", "panel.grid.major", "panel.grid.minor",
      "plot.background"
    ),
    candidates
  )

  ordered <- c(
    layer_hits,
    preferred,
    setdiff(candidates, c(layer_hits, preferred))
  )

  if (length(ordered) == 0) {
    return(NULL)
  }

  if (!is.null(current_key) && current_key %in% ordered) {
    idx <- match(current_key, ordered)
    return(ordered[(idx %% length(ordered)) + 1L])
  }

  ordered[[1]]
}
