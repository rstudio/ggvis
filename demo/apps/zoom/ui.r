shinyUI(pageWithSidebar(
  headerPanel("Zooming demo"),
  sidebarPanel(
    numericInput("xmin", "X min:", min(mtcars$disp)),
    numericInput("xmax", "X max:", max(mtcars$disp)),
    numericInput("ymin", "Y min:", min(mtcars$mpg)),
    numericInput("ymax", "Y max:", max(mtcars$mpg))
  ),
  mainPanel(
    gigvisOutput("zoom")
  )
))
