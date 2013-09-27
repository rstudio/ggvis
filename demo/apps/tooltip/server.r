shinyServer(function(input, output, session) {

  r_gv <- reactive({
    ggvis(mtcars, props(x = ~wt, y = ~mpg, size.hover := 200), mark_symbol())
  })

  # Set up observers for the spec and the data
  observe_ggvis(r_gv, "plot1", session, "svg")

  # User interface elements (in the sidebar)
  output$ggvis_ui <- renderControls(r_gv, session)

  # Stop the app when the quit button is clicked
  observe({
    if (is.null(input$quit)) return()
    if (input$quit > 0) stopApp()
  })

  # Print the object that was sent over
  output$info <- renderPrint({
    str(input$ggvis_hover)
  })

  # Hover tooltip
  output$ggvis_tooltip <- reactive({
    hover <- input$ggvis_hover
    if (is.null(hover)) return(invisible())

    list(
      pagex = hover$pagex + 15,
      pagey = hover$pagey,
      text = format(div(
        paste("wt:", hover$data$wt),
        br(),
        paste("mpg", hover$data$mpg)
      ))
    )
  })
  outputOptions(output, "ggvis_tooltip", suspendWhenHidden = FALSE)

})
