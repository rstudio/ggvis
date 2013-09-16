shinyServer(function(input, output, session) {

  # A subset of mtcars
  mtc <- reactive({ mtcars[1:input$n, ] })

  r_gv <- reactive({
    ggvis(mtc, props(x = ~wt, y = ~mpg), mark_symbol())
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

  output$mtc_table <- renderTable({
    mtc()[, c("wt", "mpg")]
  })
})
