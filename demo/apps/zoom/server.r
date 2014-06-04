library(ggvis)

shinyServer(function(input, output, session) {
  reactive({
    mtcars %>% ggvis(~disp, ~mpg) %>%
      layer_points() %>%
      scale_numeric("x", domain = input$x_domain, nice = FALSE, clamp = TRUE) %>%
      scale_numeric("y", domain = input$y_domain, nice = FALSE, clamp = TRUE)
  }) %>%
  bind_shiny("zoom")
})
