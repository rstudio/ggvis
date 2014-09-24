library(ggvis)
data(diamonds, package = "ggplot2")

shinyServer(function(input, output, session) {

  values <- reactiveValues(selected = rep(TRUE, nrow(diamonds)))

  diamonds %>% ggvis(~carat) %>%
    layer_histograms(fill.hover := "red", width = 0.1) %>%
    handle_hover(function(data, ...) {
      values$selected <- diamonds$carat >= data$xmin_ &
        diamonds$carat < data$xmax_
    }) %>%
    set_options(width = 400, height = 200) %>%
    bind_shiny("plot1")

  # Sub-histogram
  reactive(diamonds[values$selected, , drop = FALSE]) %>%
    ggvis(~carat) %>%
    layer_histograms(width = 0.01) %>%
    set_options(width = 400, height = 200) %>%
    bind_shiny("plot2")

})
