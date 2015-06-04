library(ggvis)

set.seed(1233)
cocaine <- cocaine[sample(1:nrow(cocaine), 500), ]
cocaine$id <- seq_len(nrow(cocaine))

shinyServer(function(input, output, session) {

  lb <- linked_brush(keys = cocaine$id, "red")

  cocaine %>%
    ggvis(~weight, ~price, key := ~id) %>%
    layer_points(fill := lb$fill, fill.brush := "red", opacity := 0.3) %>%
    lb$input() %>%
    set_options(width = 300, height = 300) %>%
    bind_shiny("plot1") # Very important!


  # A subset of cocaine, of only the selected points
  selected <- lb$selected
  cocaine_selected <- reactive({
    cocaine[selected(), ]
  })

  cocaine %>%
    ggvis(~potency) %>%
    layer_histograms(width = 5, boundary = 0) %>%
    add_data(cocaine_selected) %>%
    layer_histograms(width = 5, boundary = 0, fill := "#dd3333") %>%
    set_options(width = 300, height = 300) %>%
    bind_shiny("plot2")
})
