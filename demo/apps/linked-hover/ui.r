library(ggvis)

shinyUI(bootstrapPage(
  ggvisOutput("plot1"),
  ggvisOutput("plot2")
))
