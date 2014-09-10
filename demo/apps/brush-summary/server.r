library(ggvis)

shinyServer(function(input, output, session) {
  mtcars <- cbind(mtcars, id = seq_len(nrow(mtcars)))

  # Create a linked brush object
  lb <- linked_brush(keys = mtcars$id, "red")

  # Just the brushed points
  selected <- lb$selected
  mtcars_selected <- reactive({
    if (!any(selected())) return(mtcars)
    mtcars[selected(), ]
  })

  mtcars %>%
    ggvis(~wt, ~mpg) %>%
    layer_points(fill := lb$fill, fill.brush := "red") %>%
    lb$input() %>%
    add_data(mtcars_selected) %>%
    layer_model_predictions(model = "lm")%>%
    bind_shiny("plot1")

  output$brush_data <- renderPrint({
    cat("Number of points selected: ", nrow(mtcars_selected()), "\n\n")
    print(summary(mtcars_selected()))
  })
})
