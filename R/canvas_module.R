canvas_module_ui <- function(id, height = "620px") {
  ns <- shiny::NS(id)

  shiny::tagList(
    shiny::plotOutput(ns("plot"), click = ns("plot_click"), height = height),
    shiny::helpText("Click plot elements to inspect/edit. Repeated clicks on overlapping regions cycle candidates."),
    shiny::textOutput(ns("selected_label"))
  )
}

canvas_module_server <- function(id, plot) {
  shiny::moduleServer(id, function(input, output, session) {
    selected <- shiny::reactiveVal(NULL)

    output_dims <- shiny::reactive({
      width <- session$clientData[[paste0("output_", session$ns("plot"), "_width")]] %||% 900
      height <- session$clientData[[paste0("output_", session$ns("plot"), "_height")]] %||% 620
      list(width = as.numeric(width), height = as.numeric(height))
    })

    scene <- shiny::reactive({
      dims <- output_dims()
      build_svg_scene(plot(), width_px = dims$width, height_px = dims$height)
    })

    output$plot <- shiny::renderPlot({
      preview_highlight_selection(plot(), selected())
    }, res = 96)

    output$selected_label <- shiny::renderText({
      key <- selected()
      if (is.null(key)) {
        return("Selected: none")
      }

      desc <- selection_descriptor(key)
      if (is.null(desc)) {
        return(sprintf("Selected: %s", key))
      }

      sprintf("Selected: %s", desc$label)
    })

    shiny::observeEvent(input$plot_click, {
      click <- input$plot_click
      if (is.null(click$coords_css)) {
        return()
      }

      dims <- output_dims()
      x_npc <- click$coords_css$x / dims$width
      y_npc <- click$coords_css$y / dims$height

      key <- resolve_registry_selection(
        scene()$registry,
        x_npc = x_npc,
        y_npc = y_npc,
        current_key = selected()
      )

      if (!is.null(key)) {
        selected(key)
      }
    })

    list(
      selected = shiny::reactive(selected()),
      scene = scene,
      set_selected = function(key) selected(key)
    )
  })
}
