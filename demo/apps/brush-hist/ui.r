shinyUI(bootstrapPage(
  ggvis_output("plot1"),
  ggvis_output("plot2"),
  h3("Brush data (sent from client to server)"),
  verbatimTextOutput("brush_data")
))
