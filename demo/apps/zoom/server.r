shinyServer(function(input, output, session) {
  gv <- reactive({
    gigvis(mtcars, props(x ~ disp, y ~ mpg),
      mark_symbol(),
      dscale("x", "numeric", range = c(input$xmin, input$xmax)),
      dscale("y", "numeric", range = c(input$ymin, input$ymax))
    )
  })
  
  observeGigvis(gv, "zoom", session)
})
