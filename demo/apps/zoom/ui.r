shinyUI(pageWithSidebar(
  headerPanel("Zooming demo"),
  sidebarPanel(
    sliderInput("x_domain", "X", min(mtcars$disp), max(mtcars$disp), range(mtcars$disp)),
    sliderInput("y_domain", "Y", min(mtcars$mpg), max(mtcars$mpg), range(mtcars$mpg))
  ),
  mainPanel(
    gigvisOutput("zoom")
  )
))
