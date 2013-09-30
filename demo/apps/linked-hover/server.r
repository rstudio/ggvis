data(diamonds, package = "ggplot2")

shinyServer(function(input, output, session) {

  hist_gv <- reactive({
    ggvis(
      diamonds,
      props(x = ~carat),
      branch_histogram(props(fill.hover := "red"), binwidth = 0.1)
    )
  })

  # Set up observers for the spec and the data
  observe_ggvis(hist_gv, "plot1", session, "svg")


  # Get subset of diamonds from first plot
  diamonds_subset <- reactive({
    hover <- input$ggvis_hover
    if (is.null(hover) || is.na(hover$data['xmin__'])) return(diamonds)

    diamonds[diamonds$carat >= hover$data$xmin__ &
             diamonds$carat <  hover$data$xmax__, ]
  })

  # Sub-histogram
  hist2_gv <- reactive({
    ggvis(
      diamonds_subset(),
      props(x = ~carat),
      branch_histogram(
        props(fill.hover := "red"),
        binwidth = 0.01, drop = TRUE, right = FALSE
      )
    )
  })

  # Set up observers for the spec and the data
  observe_ggvis(hist2_gv, "plot2", session, "svg")

  # User interface elements (in the sidebar)
  output$ggvis_ui <- renderControls(hist_gv, session)

  # Stop the app when the quit button is clicked
  observe({
    if (is.null(input$quit)) return()
    if (input$quit > 0) stopApp()
  })

  # Print the object that was sent over
  output$hover_data <- renderPrint({
    str(input$ggvis_hover)
  })

})
