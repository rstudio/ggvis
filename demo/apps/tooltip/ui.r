shinyUI(bootstrapPage(
  ggvis_output("plot1"),
  h3("Hover data (sent from client to server)"),
  verbatimTextOutput("hover_data")
))
