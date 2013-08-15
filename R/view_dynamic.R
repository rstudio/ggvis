#' Generate a dynamic shiny app with the embedded gigvis graph
#'
#' @inheritParams view_static
#' @param port the port on which to start the shiny app
#' @keywords internal
#' @export
#' @importFrom RJSONIO toJSON
#' @importFrom whisker whisker.render
#' @importFrom shiny pageWithSidebar headerPanel sidebarPanel uiOutput
#'   mainPanel tags observe runApp stopApp renderUI
#' @keywords internal
view_dynamic <- function(gv, renderer = "canvas", launch = TRUE, port = 8228) {

  if (!(renderer %in% c("canvas", "svg")))
    stop("renderer must be 'canvas' or 'svg'")

  plot_id <- "plot1"

  # Make our resources available
  ui <- pageWithSidebar(
    headerPanel("Gigvis plot"),
    sidebarPanel(
      uiOutput("gigvis_ui"),

      # Add an actionButton that quits the app and closes the browser window
      tags$button(id="quit", type="button", class="btn action-button",
        onclick = "window.close()", "Quit")
    ),
    mainPanel(
      gigvisOutput(plot_id)
    )
  )

  server <- function(input, output, session) {
    r_gv <- reactive(gv)
    # Set up observers for the spec and the data
    observeGigvis(r_gv, plot_id, session, renderer)

    # User interface elements (in the sidebar)
    output$gigvis_ui <- renderControls(r_gv, session)

    # Stop the app when the quit button is clicked
    observe({
      if (is.null(input$quit)) return()
      if (input$quit > 0) stopApp()
    })
  }

  runApp(list(ui = ui, server = server))
}
