#' Launch ggplotStudioR Interactive Editor
#'
#' Open a Shiny-based MVP editor for a ggplot object.
#'
#' @param plot A ggplot object to edit.
#' @return Invisibly returns a [shiny::shiny.appobj()] after app closure.
#' @export
launch_ggplot_studio <- function(plot) {
  validate_ggplot(plot)

  ui <- shiny::fluidPage(
    shiny::sidebarLayout(
      shiny::sidebarPanel(
        shiny::textInput("title", "Title", value = plot$labels$title %||% ""),
        shiny::textInput("subtitle", "Subtitle", value = plot$labels$subtitle %||% ""),
        shiny::textInput("caption", "Caption", value = plot$labels$caption %||% ""),
        shiny::textInput("x_label", "X Label", value = plot$labels$x %||% ""),
        shiny::textInput("y_label", "Y Label", value = plot$labels$y %||% ""),
        shiny::selectInput("theme", "Theme Preset", choices = .theme_choices, selected = "gray"),
        shiny::selectInput("palette", "Palette", choices = .palette_choices, selected = "hue"),
        shiny::sliderInput("point_size", "Point Size", min = 0.5, max = 8, value = 2, step = 0.1),
        shiny::sliderInput("line_size", "Line Size", min = 0.2, max = 5, value = 1, step = 0.1),
        shiny::downloadButton("download_code", "Export Code File"),
        shiny::actionButton("copy_code", "Copy Code to Clipboard")
      ),
      shiny::mainPanel(
        shiny::plotOutput("plot_preview"),
        shiny::verbatimTextOutput("generated_code")
      )
    )
  )

  server <- function(input, output, session) {
    controls <- reactive({
      list(
        title = input$title,
        subtitle = input$subtitle,
        caption = input$caption,
        x_label = input$x_label,
        y_label = input$y_label,
        theme = input$theme,
        palette = input$palette,
        point_size = input$point_size,
        line_size = input$line_size
      )
    })

    edited_plot <- reactive({
      build_plot_from_controls(plot, controls())
    })

    output$plot_preview <- shiny::renderPlot({
      edited_plot()
    })

    output$generated_code <- shiny::renderText({
      generate_plot_code("p", controls())
    })

    output$download_code <- shiny::downloadHandler(
      filename = function() {
        "ggplotStudioR_plot_code.R"
      },
      content = function(file) {
        writeLines(generate_plot_code("p", controls()), con = file)
      }
    )

    shiny::observeEvent(input$copy_code, {
      code <- generate_plot_code("p", controls())
      ok <- FALSE

      try({
        clipr::write_clip(code)
        ok <- TRUE
      }, silent = TRUE)

      if (ok) {
        shiny::showNotification("Code copied to clipboard.", type = "message")
      } else {
        fallback <- file.path(tempdir(), "ggplotStudioR_plot_code.R")
        writeLines(code, con = fallback)
        shiny::showNotification(
          paste0("Clipboard unavailable. Wrote fallback file: ", fallback),
          type = "warning",
          duration = 8
        )
      }
    })
  }

  app <- shiny::shinyApp(ui = ui, server = server)
  shiny::runApp(app)
  invisible(app)
}

#' Launch ggplotStudioR
#'
#' Convenience wrapper around [launch_ggplot_studio()].
#'
#' @param plot A ggplot object to edit.
#' @return Invisibly returns a [shiny::shiny.appobj()] after app closure.
#' @export
launch <- function(plot) {
  launch_ggplot_studio(plot)
}

`%||%` <- function(lhs, rhs) {
  if (is.null(lhs) || identical(lhs, "")) rhs else lhs
}
