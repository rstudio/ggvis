shinyServer(function(input, output, session) {
  gv <- reactive({
    gigvis(mtcars, props(x ~ disp, y ~ mpg),
      mark_symbol(),
      dscale("x", "numeric", domain = input$x_domain, nice = FALSE),
      dscale("y", "numeric", domain = input$y_domain, nice = FALSE)
    )
  })
  
  observeGigvis(gv, "zoom", session)
})
