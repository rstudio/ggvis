library(ggvis)

var_range <- function(id, label, variable) {
  rng <- range(variable, na.rm = TRUE)
  sliderInput(id, label, rng[1], rng[2], rng)
}

shinyUI(pageWithSidebar(
  headerPanel("Zooming demo"),
  sidebarPanel(
    var_range("x_domain", "X", mtcars$disp),
    var_range("y_domain", "Y", mtcars$mpg)
  ),
  mainPanel(
    ggvisOutput("zoom")
  )
))
