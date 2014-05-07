shinyUI(bootstrapPage(
  ggvisOutput("plot1"),
  ggvisOutput("plot2"),
  h3("Hover data (sent from client to server)"),
  verbatimTextOutput("hover_data")
))
