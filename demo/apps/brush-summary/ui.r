shinyUI(bootstrapPage(
  ggvis_output("plot1"),
  h3("Summary of brushed data (sent from client to server)"),
  verbatimTextOutput("brush_data")
))
