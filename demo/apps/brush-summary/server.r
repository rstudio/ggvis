library(ggvis)

data(diamonds, package = "ggplot2")
diamonds <- diamonds[sample(1:nrow(diamonds), 1000), ]

shinyServer(function(input, output, session) {
  under_brush <- function(items, ...) {
    output$brush_data <- renderPrint({
      cat("Number of points selected: ", nrow(items), "\n\n")
      print(summary(items))
    })
  }

  gv <- reactive({
    mtcars %>% ggvis(~wt, ~mpg, fill.brush := "blue") %>%
      layer_points() %>%
      handle_brush(under_brush)
  })

  # Set up observers for the spec and the data
  observe_ggvis(gv, "plot1", session)

})
