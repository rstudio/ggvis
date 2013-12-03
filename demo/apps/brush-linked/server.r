library(ggvis)

data(diamonds, package = "ggplot2")
diamonds <- diamonds[sample(1:nrow(diamonds), 1000), ]
diamonds$id <- sample(1:1000)

shinyServer(function(input, output, session) {

  gv1 <- reactive({
    ggvis(diamonds, props(x = ~carat, y = ~price)) +
      mark_symbol(props(fill.brush := "red", fillOpacity := 0.8, key := ~id)) +
      branch_brush() +
      opts(width = 300, height = 300, brush_delay = 100)
  })

  # Set up observers for the spec and the data
  observe_ggvis(gv1, "plot1", session)

  # Return the colors for the brushed items
  selected <- reactive({
    colors <- rep("black", nrow(diamonds))

    if (is.null(input$ggvis_plot1_brush)) return(colors)

    items <- input$ggvis_plot1_brush$items

    keys <- vapply(items, `[[`, "key__", FUN.VALUE = character(1))
    keys <- as.integer(keys)

    colors[diamonds$id %in% keys] <- "red"
    colors
  })

  gv2 <- reactive({
    ggvis(diamonds, props(x = ~table, y = ~depth)) +
      mark_symbol(props(fill := ~selected(), fillOpacity := 0.8)) +
      opts(width = 300, height = 300)
  })

  # Set up observers for the spec and the data
  observe_ggvis(gv2, "plot2", session)

  # Print the object that was sent over
  output$brush_data <- renderPrint({
     str(input$ggvis_plot1_brush)
  })

})
