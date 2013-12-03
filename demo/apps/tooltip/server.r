shinyServer(function(input, output, session) {

  r_gv <- reactive({
    ggvis(mtcars, props(x = ~wt, y = ~mpg, size.hover := 200)) + mark_symbol()
  })

  # Set up observers for the spec and the data
  observe_ggvis(r_gv, "plot1", session)

  # User interface elements (in the sidebar)
  output$ggvis_ui <- renderControls(r_gv, session)

  # Print the object that was sent over
  output$hover_data <- renderPrint({
    str(input$ggvis_plot1_hover)
  })

  observe({
    hover <- input$ggvis_plot1_hover

    # Initially, hover is null. Later, after mousing out of a mark, hover$data
    # exists, and is NULL.
    if (is.null(hover) || is.null(hover$data)) {
      message <- list(visible = FALSE)

    } else {
      # If hovering over a point, `data` is a list with `wt` and `mpg`, but if
      # hovering over an axis label or legend, `data` is an atomic vector, so
      # we need to do these checks. An alternative is to use a try() block.
      if (exists('wt', hover$data) && exists('mpg', hover$data)) {
        html <- format(div(
          paste("Weight:", 1000*hover$data$wt, "lbs"),
          br(),
          paste("MPG:", hover$data$mpg)
        ))
      } else {
        html <- format(div(hover$data))
      }

      message <- list(
        visible = TRUE,
        pagex = hover$pagex + 15,
        pagey = hover$pagey,
        html = html
      )
    }

    session$sendCustomMessage('ggvis_tooltip', message)
  })

})
