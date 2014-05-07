library(ggvis)

data(diamonds, package = "ggplot2")
diamonds <- diamonds[sample(1:nrow(diamonds), 1000), ]

shinyServer(function(input, output, session) {
  # Create plot and set up observers
  gv <- mtcars %>%
    ggvis(~wt, ~mpg) %>%
    layer_points(fill.brush := "blue") %>%
    handle_brush(function(items, ...) {
      output$brush_data <- renderPrint({
        cat("Number of points selected: ", nrow(items), "\n\n")
        print(summary(items))
      })
    })

  # Set up observers for the spec and the data
  observe_ggvis(gv, "plot1", session)
})
