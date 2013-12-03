shinyServer(function(input, output, session) {

  # A subset of mtcars
  mtc <- reactive({ mtcars[1:input$n, ] })

  r_gv <- reactive({
    ggvis(mtc, props(x = ~wt, y = ~mpg)) + mark_symbol()
  })

  # Set up observers for the spec and the data
  observe_ggvis(r_gv, "plot1", session)

  # User interface elements (in the sidebar)
  output$ggvis_ui <- renderControls(r_gv, session)

  output$mtc_table <- renderTable({
    mtc()[, c("wt", "mpg")]
  })
})
