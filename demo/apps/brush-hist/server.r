library(ggvis)

data(diamonds, package = "ggplot2")
diamonds <- diamonds[sample(1:nrow(diamonds), 1000), ]

shinyServer(function(input, output, session) {

  hist_gv <- reactive({
    ggvis(diamonds, props(x = ~carat)) +
      branch_histogram(props(fill.brush := "red"), binwidth = 0.1) +
      branch_brush() +
      opts(height = 200)
  })

  # Set up observers for the spec and the data
  observe_ggvis(hist_gv, "plot1", session)


  diamonds_brushed <- reactive({
    ranges <- input$ggvis_plot1_brush$items

    if (is.null(ranges) || length(ranges) == 0) {
      return(cbind(diamonds, colors = "#333"))
    }

    in_ranges <- lapply(ranges, function(range) {
      diamonds$carat > range$xmin__ & diamonds$carat < range$xmax__
    })
    
    selected <- Reduce(`|`, in_ranges)

    # Return a character vector with colors for each TRUE and FALSE value
    colors <- rep("#333", length(selected))
    colors[selected == TRUE] <- "red"

    cbind(diamonds, colors = colors)
  })

  scatter_gv <- reactive({
    ggvis(diamonds_brushed, props(x = ~depth, y = ~price)) +
      mark_symbol(props(fill := ~colors, fillOpacity := 0.8))
  })

  # Set up observers for the spec and the data
  observe_ggvis(scatter_gv, "plot2", session)

  # Print the object that was sent over
  output$brush_data <- renderPrint({
    str(input$ggvis_plot1_brush)
  })

})
