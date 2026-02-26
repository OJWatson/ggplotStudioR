#' Launch ggplotStudioR Interactive Editor
#'
#' Open the next-generation Shiny editor for an existing `ggplot2` object.
#'
#' @param plot A ggplot object to edit.
#' @return Invisibly returns a [shiny::shiny.appobj()] after app closure.
#' @export
launch_ggplot_studio <- function(plot) {
  validate_ggplot(plot)

  ui <- shiny::fluidPage(
    shiny::fluidRow(
      shiny::column(
        width = 4,
        shiny::wellPanel(
          shiny::h3("Inspector"),
          inspector_module_ui("inspector"),
          shiny::hr(),
          shiny::h4("Code export"),
          shiny::textInput("base_plot_expr", "Base plot symbol", value = "p"),
          shiny::radioButtons(
            "export_mode",
            "Export mode",
            choices = c("Additive ggplot" = "additive", "Patch script" = "patch"),
            selected = "additive",
            inline = TRUE
          ),
          shiny::downloadButton("download_code", "Export code file"),
          shiny::actionButton("copy_code", "Copy code to clipboard")
        ),
        shiny::h4("Generated code"),
        shiny::verbatimTextOutput("generated_code")
      ),
      shiny::column(
        width = 8,
        canvas_module_ui("canvas")
      )
    )
  )

  server <- function(input, output, session) {
    spec_state <- shiny::reactiveVal(studio_spec_init(plot))

    update_spec <- function(mutator) {
      next_spec <- mutator(spec_state())
      studio_spec_validate(next_spec, plot)
      spec_state(next_spec)
    }

    edited_plot <- shiny::reactive({
      apply_studio_spec(plot, spec_state())
    })

    canvas <- canvas_module_server("canvas", plot = edited_plot)

    inspector_module_server(
      "inspector",
      selected = canvas$selected,
      spec = shiny::reactive(spec_state()),
      update_spec = update_spec
    )

    generated_code <- shiny::reactive({
      base_expr <- studio_null_if_empty(input$base_plot_expr) %||% "p"
      generate_plot_code(
        base_plot_expr = base_expr,
        spec = spec_state(),
        mode = input$export_mode %||% "additive"
      )
    })

    output$generated_code <- shiny::renderText(generated_code())

    output$download_code <- shiny::downloadHandler(
      filename = function() {
        sprintf("ggplotStudioR_%s_code.R", input$export_mode %||% "additive")
      },
      content = function(file) {
        writeLines(generated_code(), con = file)
      }
    )

    shiny::observeEvent(input$copy_code, {
      code <- generated_code()
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
