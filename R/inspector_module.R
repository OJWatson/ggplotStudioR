inspector_module_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::uiOutput(ns("inspector"))
}

build_inspector_ui <- function(desc, spec) {
  if (is.null(desc)) {
    return(shiny::div(
      shiny::h4("Inspector"),
      shiny::p("Click an editable plot element on the canvas to reveal controls.")
    ))
  }

  if (identical(desc$kind, "layer")) {
    layer <- spec$layers[[desc$layer_index]]
    return(shiny::tagList(
      shiny::h4(desc$label),
      shiny::checkboxInput("layer_visible", "Visible", value = isTRUE(layer$visible)),
      shiny::numericInput("layer_alpha", "Alpha", value = layer$aes_params$alpha %||% NA_real_, min = 0, max = 1, step = 0.05),
      shiny::numericInput("layer_size", "Size", value = layer$aes_params$size %||% NA_real_, min = 0, step = 0.1),
      shiny::numericInput("layer_linewidth", "Line width", value = layer$aes_params$linewidth %||% NA_real_, min = 0, step = 0.1),
      shiny::selectInput(
        "layer_linetype",
        "Line type",
        choices = c("(inherit)" = "", "solid", "dashed", "dotted", "dotdash", "longdash", "twodash"),
        selected = layer$aes_params$linetype %||% ""
      )
    ))
  }

  entry <- spec$theme[[desc$theme_element]]

  controls <- list(
    shiny::h4(desc$label)
  )

  if (!is.null(desc$label_field)) {
    controls <- c(
      controls,
      list(shiny::textInput("text_value", "Text", value = spec$labels[[desc$label_field]] %||% ""))
    )
  }

  controls <- c(controls, list(
    shiny::checkboxInput("theme_blank", "Hide (element_blank)", value = isTRUE(entry$blank))
  ))

  if (identical(desc$style, "text")) {
    controls <- c(controls, list(
      shiny::textInput("theme_colour", "Colour", value = entry$colour %||% ""),
      shiny::numericInput("theme_size", "Size", value = entry$size %||% NA_real_, min = 1, step = 0.5),
      shiny::selectInput(
        "theme_face",
        "Font face",
        choices = c("(inherit)" = "", "plain", "bold", "italic", "bold.italic"),
        selected = entry$face %||% ""
      )
    ))
  } else if (identical(desc$style, "rect")) {
    controls <- c(controls, list(
      shiny::textInput("theme_fill", "Fill", value = entry$fill %||% ""),
      shiny::textInput("theme_colour", "Border colour", value = entry$colour %||% ""),
      shiny::numericInput("theme_linewidth", "Border width", value = entry$linewidth %||% NA_real_, min = 0, step = 0.1),
      shiny::selectInput(
        "theme_linetype",
        "Border line type",
        choices = c("(inherit)" = "", "solid", "dashed", "dotted", "dotdash", "longdash", "twodash"),
        selected = entry$linetype %||% ""
      )
    ))
  } else if (identical(desc$style, "line")) {
    controls <- c(controls, list(
      shiny::textInput("theme_colour", "Line colour", value = entry$colour %||% ""),
      shiny::numericInput("theme_linewidth", "Line width", value = entry$linewidth %||% NA_real_, min = 0, step = 0.1),
      shiny::selectInput(
        "theme_linetype",
        "Line type",
        choices = c("(inherit)" = "", "solid", "dashed", "dotted", "dotdash", "longdash", "twodash"),
        selected = entry$linetype %||% ""
      )
    ))
  }

  controls <- c(controls, list(
    shiny::helpText("Only representable fields are exposed. Unsupported aesthetics remain untouched in generated code.")
  ))

  do.call(shiny::tagList, controls)
}

inspector_module_server <- function(id, selected, spec, update_spec) {
  shiny::moduleServer(id, function(input, output, session) {
    current_desc <- shiny::reactive(selection_descriptor(selected()))

    output$inspector <- shiny::renderUI({
      build_inspector_ui(current_desc(), spec())
    })

    theme_prop_update <- function(property, value) {
      desc <- current_desc()
      if (is.null(desc) || !identical(desc$kind, "theme") || is.null(desc$theme_element)) {
        return()
      }

      update_spec(function(s) {
        studio_spec_update_theme(s, desc$theme_element, property, value)
      })
    }

    shiny::observeEvent(input$text_value, {
      desc <- current_desc()
      if (is.null(desc) || is.null(desc$label_field)) {
        return()
      }

      update_spec(function(s) {
        studio_spec_update_label(s, desc$label_field, input$text_value)
      })
    }, ignoreInit = TRUE)

    shiny::observeEvent(input$theme_blank, {
      desc <- current_desc()
      if (is.null(desc) || is.null(desc$theme_element)) {
        return()
      }

      update_spec(function(s) {
        studio_spec_set_theme_blank(s, desc$theme_element, input$theme_blank)
      })
    }, ignoreInit = TRUE)

    shiny::observeEvent(input$theme_colour, {
      theme_prop_update("colour", input$theme_colour)
    }, ignoreInit = TRUE)

    shiny::observeEvent(input$theme_fill, {
      theme_prop_update("fill", input$theme_fill)
    }, ignoreInit = TRUE)

    shiny::observeEvent(input$theme_size, {
      theme_prop_update("size", studio_num_or_null(input$theme_size))
    }, ignoreInit = TRUE)

    shiny::observeEvent(input$theme_face, {
      theme_prop_update("face", input$theme_face)
    }, ignoreInit = TRUE)

    shiny::observeEvent(input$theme_linewidth, {
      theme_prop_update("linewidth", studio_num_or_null(input$theme_linewidth))
    }, ignoreInit = TRUE)

    shiny::observeEvent(input$theme_linetype, {
      theme_prop_update("linetype", input$theme_linetype)
    }, ignoreInit = TRUE)

    shiny::observeEvent(input$layer_visible, {
      desc <- current_desc()
      if (is.null(desc) || !identical(desc$kind, "layer")) {
        return()
      }

      update_spec(function(s) {
        studio_spec_set_layer_visible(s, desc$layer_index, input$layer_visible)
      })
    }, ignoreInit = TRUE)

    shiny::observeEvent(input$layer_alpha, {
      desc <- current_desc()
      if (is.null(desc) || !identical(desc$kind, "layer")) {
        return()
      }

      update_spec(function(s) {
        studio_spec_update_layer_aes(s, desc$layer_index, "alpha", studio_num_or_null(input$layer_alpha))
      })
    }, ignoreInit = TRUE)

    shiny::observeEvent(input$layer_size, {
      desc <- current_desc()
      if (is.null(desc) || !identical(desc$kind, "layer")) {
        return()
      }

      update_spec(function(s) {
        studio_spec_update_layer_aes(s, desc$layer_index, "size", studio_num_or_null(input$layer_size))
      })
    }, ignoreInit = TRUE)

    shiny::observeEvent(input$layer_linewidth, {
      desc <- current_desc()
      if (is.null(desc) || !identical(desc$kind, "layer")) {
        return()
      }

      update_spec(function(s) {
        studio_spec_update_layer_aes(s, desc$layer_index, "linewidth", studio_num_or_null(input$layer_linewidth))
      })
    }, ignoreInit = TRUE)

    shiny::observeEvent(input$layer_linetype, {
      desc <- current_desc()
      if (is.null(desc) || !identical(desc$kind, "layer")) {
        return()
      }

      update_spec(function(s) {
        studio_spec_update_layer_aes(s, desc$layer_index, "linetype", input$layer_linetype)
      })
    }, ignoreInit = TRUE)
  })
}
