library(ggvis)

function(input, output, session) {
  # A reactive subset of mtcars
  mtc <- reactive({ mtcars[1:input$n, ] })

  # A simple visualisation. In shiny apps, need to register observers
  # and tell shiny where to put the controls
  mtc %>%
    ggvis(~wt, ~mpg) %>%
    layer_points() %>%
    bind_shiny("plot", "plot_ui")

  output$mtc_table <- renderTable({
    mtc()[, c("wt", "mpg")]
  })
}
