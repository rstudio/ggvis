shinyUI(pageWithSidebar(
  headerPanel("ggvis plot"),
  sidebarPanel(
    uiOutput("ggvis_ui"),
    ggvisControlGroup("plot1")
  ),
  mainPanel(
    ggvis_output("plot1"),
    ggvis_output("plot2"),
    h3("Hover data (sent from client to server)"),
    verbatimTextOutput("hover_data")
  )
))
