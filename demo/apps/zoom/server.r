library(ggvis)

shinyServer(function(input, output, session) {
  reactive({
    mtcars %>% ggvis(~disp, ~mpg) %>%
      layer_points() %>%
      set_dscale("x", "numeric", domain = input$x_domain, nice = FALSE, clamp = TRUE) %>%
      set_dscale("y", "numeric", domain = input$y_domain, nice = FALSE, clamp = TRUE)
  }) %>%
  bind_shiny("zoom")
})
