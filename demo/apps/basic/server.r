shinyServer(function(input, output, session) {
  # Create a slider that's used for two different plots
  span_slider <- input_slider(min = 0.2, max = 1, value = 0.5, step = 0.05,
    label = "Loess span")

  # Create the ggvis objects
  gv <- reactive({
    ggvis(mtcars, props(x ~ wt, y ~ mpg),
      mark_symbol(),
      branch_smooth(span = span_slider)
    )
  })
  gv2 <- reactive({
    ggvis(mtcars, props(x ~ wt, y ~ hp),
      mark_symbol(),
      branch_smooth(span = span_slider)
    )
  })

  # Set up observers for the spec and the data
  observe_ggvis(gv, "plot1", session)
  observe_ggvis(gv2, "plot2", session, width = 250, height = 250)

  # User interface elements (in the sidebar)
  output$ggvis_ui <- renderControls(gv, session)

  # Stop the app when the quit button is clicked
  observe({
    if (is.null(input$quit)) return()
    if (input$quit > 0) stopApp()
  })

})
