shinyUI(pageWithSidebar(
  headerPanel("ggvis plot"),
  sidebarPanel(
    uiOutput("ggvis_ui"),
    ggvisControlGroup("plot1")
  ),
  mainPanel(
    ggvis_output("plot1"),
    verbatimTextOutput("info"),
    div(id = "ggvis_tooltip", class = "shiny-ggvis-tooltip-output", "")
  )
))
