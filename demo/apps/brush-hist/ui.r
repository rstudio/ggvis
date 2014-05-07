shinyUI(bootstrapPage(
  ggvisOutput("plot1"),
  ggvisOutput("plot2"),
  h3("Brush data (sent from client to server)"),
  verbatimTextOutput("brush_data")
))
