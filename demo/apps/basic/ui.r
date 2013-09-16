shinyUI(pageWithSidebar(
  headerPanel("ggvis plot"),
  sidebarPanel(
    sliderInput("n", "Number of points", min = 1, max = nrow(mtcars),
                value = 10, step = 1),
    uiOutput("ggvis_ui"),
    ggvisControlGroup("plot1")
  ),
  mainPanel(
    ggvis_output("plot1"),
    tableOutput("mtc_table")
  )
))
