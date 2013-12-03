library(ggvis)

data(diamonds, package = "ggplot2")
diamonds <- diamonds[sample(1:nrow(diamonds), 1000), ]

shinyServer(function(input, output, session) {

  gv <- reactive({
    ggvis(mtcars, props(x = ~wt, y = ~mpg, fill.brush := "blue")) +
      mark_symbol() +
      branch_brush() +
      opts(brush_delay = 100)
  })

  # Set up observers for the spec and the data
  observe_ggvis(gv, "plot1", session)

  # Return a data frame of the brushed points
  brushed_items <- reactive({
    items <- input$ggvis_plot1_brush$items

    if (is.null(items) || length(items) == 0) {
      return(data.frame(wt = numeric(0), mpg = numeric(0)))
    }

    data.frame(
      wt  = vapply(items, `[[`, "wt",  FUN.VALUE = numeric(1)),
      mpg = vapply(items, `[[`, "mpg", FUN.VALUE = numeric(1))
    )
  })

  # Print the object that was sent over
  output$brush_data <- renderPrint({
    cat("Number of points selected: ", nrow(brushed_items()), "\n\n")
    print(summary(brushed_items()))
  })

})
