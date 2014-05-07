library(ggvis)

data(diamonds, package = "ggplot2")
diamonds <- diamonds[sample(1:nrow(diamonds), 1000), ]

shinyServer(function(input, output, session) {
  mtcars %>%
    ggvis(~wt, ~mpg) %>%
    layer_points(fill.brush := "blue") %>%
    handle_brush(function(items, ...) {
      output$brush_data <- renderPrint({
        cat("Number of points selected: ", nrow(items), "\n\n")
        print(summary(items))
      })
    }) %>%
    bind_shiny("plot1")
})
