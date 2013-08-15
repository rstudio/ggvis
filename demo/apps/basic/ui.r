shinyUI(pageWithSidebar(
  headerPanel("Ggvis plot"),
  sidebarPanel(
    uiOutput("ggvis_ui")
  ),
  mainPanel(
    # Placeholder for the plots
    ggvisOutput("plot1"),
    ggvisOutput("plot2"),

    # Add an actionButton that quits the app and closes the browser window
    tags$button(id="quit", type="button", class="btn action-button",
      onclick = "window.close()", "Quit")
  )
))
