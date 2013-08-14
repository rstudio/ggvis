shinyServer(function(input, output, session) {
  gv <- reactive({
    gigvis(mtcars, props(x ~ disp, y ~ mpg),
      mark_symbol(),
      dscale("x", "numeric", domain = c(input$xmin, input$xmax), nice = FALSE),
      dscale("y", "numeric", domain = c(input$ymin, input$ymax), nice = FALSE)
    )
  })
  
  observeGigvis(gv, "zoom", session)
})
