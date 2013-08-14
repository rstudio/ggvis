shinyServer(function(input, output, session) {
  gv <- reactive({
    gigvis(mtcars, props(x ~ disp, y ~ mpg),
      mark_symbol(),
      dscale("x", "numeric", range = c(input$xmin, input$xmax)),
      dscale("y", "numeric", range = c(input$ymin, input$ymax))
    )
  })
  
  # Set up observers for the spec
  spec <- as.vega(isolate(gv()), session = session, dynamic = TRUE)
  observe_spec(spec, "zoom", session, "canvas")
})
