library(ggvis)

shinyServer(function(input, output, session) {
  # A reactive subset of mtcars
  mtc <- reactive({ mtcars[1:input$n, ] })

  # A simple visualisation. In shiny apps, need to register observers
  # and tell shiny where to put the controls
  r_gv <- mtc %>% ggvis(~wt, ~mpg) %>% layer_points()
  observe_ggvis(r_gv, "plot1", session)
  output$ggvis_ui <- renderControls(r_gv, session)

  output$mtc_table <- renderTable({
    mtc()[, c("wt", "mpg")]
  })
})
