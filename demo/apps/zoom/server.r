shinyServer(function(input, output, session) {
  gv <- reactive({
    mtcars %>% ggvis(~disp, ~mpg) %>%
      layer_points() %>%
      set_dscale("x", "numeric", domain = input$x_domain, nice = FALSE, clamp = TRUE) %>%
      set_dscale("y", "numeric", domain = input$y_domain, nice = FALSE, clamp = TRUE)
  })

  observe_ggvis(gv, "zoom", session)
})
