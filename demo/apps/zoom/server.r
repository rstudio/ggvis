shinyServer(function(input, output, session) {
  gv <- reactive({
    ggvis(mtcars, props(x ~ disp, y ~ mpg),
      mark_symbol(),
      dscale("x", "numeric", domain = input$x_domain, nice = FALSE, clamp = TRUE),
      dscale("y", "numeric", domain = input$y_domain, nice = FALSE, clamp = TRUE)
    )
  })

  observe_ggvis(gv, "zoom", session)
})
