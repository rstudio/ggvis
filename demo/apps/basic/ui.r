shinyUI(pageWithSidebar(
  headerPanel("Ggvis plot"),
  sidebarPanel(
    uiOutput("gigvis_ui")
  ),
  mainPanel(
    # Placeholder for the plots
    gigvisOutput("plot1"),
    gigvisOutput("plot2"),

    # Add an actionButton that quits the app and closes the browser window
    tags$button(id="quit", type="button", class="btn action-button",
      onclick = "window.close()", "Quit")
  )
))
