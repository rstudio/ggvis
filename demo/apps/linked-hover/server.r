data(diamonds, package = "ggplot2")

shinyServer(function(input, output, session) {

  hist_gv <- reactive({
    ggvis(diamonds, props(x = ~carat)) +
      branch_histogram(props(fill.hover := "red"), binwidth = 0.1)
  })

  # Set up observers for the spec and the data
  observe_ggvis(hist_gv, "plot1", session)

  # Store the subset of diamonds subset in a reactiveValues object
  values <- reactiveValues(diamonds = diamonds)

  # Get subset of diamonds from first plot
  observe({
    hover <- input$ggvis_plot1_hover

    # These conditions means that we're not ready yet, or that we're not
    # hovering over the right thing.
    if (is.null(hover) || is.null(hover$data) ||
        is.na(hover$data['xmin__'])) {
      return()
    }

    # If we got this far, that means we're hovering over a bar in plot1.
    # Push a subset of data into values$diamonds
    values$diamonds <- diamonds[diamonds$carat >= hover$data$xmin__ &
                                diamonds$carat <  hover$data$xmax__, ]
  })

  # Sub-histogram
  hist2_gv <- reactive({
    ggvis(values$diamonds, props(x = ~carat)) +
      branch_histogram(
        props(fill.hover := "red"),
        binwidth = 0.01, drop = TRUE, right = FALSE
      )
  })

  # Set up observers for the spec and the data
  observe_ggvis(hist2_gv, "plot2", session)

  # User interface elements (in the sidebar)
  output$ggvis_ui <- renderControls(hist_gv, session)

  # Print the object that was sent over
  output$hover_data <- renderPrint({
    str(input$ggvis_plot1_hover)
  })

})
