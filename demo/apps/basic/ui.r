shinyUI(pageWithSidebar(
  headerPanel("Gigvis plot"),
  sidebarPanel(
    sliderInput("obs", "Number of points",
      value = 5, min = 1, max = nrow(mtcars), step = 1),
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
