# This is used similarly to view_static, but view_dynamic can take functions
# or (reactive expressions) as data instead of names in an environment.

#' @importFrom shiny pageWithSidebar headerPanel sidebarPanel uiOutput
#'   mainPanel tags observe runApp stopApp renderUI
view_dynamic <- function(gv, envir = parent.frame(), controls = NULL,
                         renderer = "canvas", launch = TRUE, port = 8228) {

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
    # Set up observers for the spec and the data
    observeGigvis(gv, plot_id, session)

    # User interface elements (in the sidebar)
    output$gigvis_ui <- renderControls(gv)

    # Stop the app when the quit button is clicked
    observe({
      if (is.null(input$quit)) return()
      if (input$quit > 0) stopApp()
    })
  }

  runApp(list(ui = ui, server = server))
}
