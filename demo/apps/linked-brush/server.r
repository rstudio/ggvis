library(ggvis)

data(diamonds, package = "ggplot2")
diamonds <- diamonds[sample(1:nrow(diamonds), 1000), ]

shinyServer(function(input, output, session) {

  hist_gv <- reactive({
    ggvis(
      diamonds,
      props(x = ~carat),
      branch_histogram(props(fill.brush := "red"), binwidth = 0.1),
      branch_brush()
    )
  })

  # Set up observers for the spec and the data
  observe_ggvis(hist_gv, "plot1", session, "svg")

  # Return a character vector that tells whether each point is within a brushed
  # bar
  brushed_idx <- reactive({
    ranges <- input$ggvis_plot1_brush$items

    if (is.null(ranges) || length(ranges) == 0)
      return(rep("FALSE", nrow(diamonds)))

    in_ranges <- lapply(ranges, function(range) {
      diamonds$carat > range$xmin__ & diamonds$carat < range$xmax__
    })
    
    selected <- Reduce(`|`, in_ranges)

    as.character(selected)
  })

  scatter_gv <- reactive({
    ggvis(
      diamonds,
      props(x = ~depth, y = ~price),
      mark_symbol(props(fill = ~brushed_idx(), fillOpacity := 0.8)),
      dscale("fill", "nominal", range = c("#333", "red"))
    )
  })

  # Set up observers for the spec and the data
  observe_ggvis(scatter_gv, "plot2", session, "svg")

  # Print the object that was sent over
  output$brush_data <- renderPrint({
    str(input$ggvis_plot1_brush)
  })

})
