shinyServer(function(input, output, session) {

  # Create the gigvis object
  mtc <- reactive({
    if (is.null(input$obs)) return(mtcars)
    mtcars[sample(nrow(mtcars), input$obs), ]
  })

  gv <- reactive({
    gigvis(mtc, props(x ~ wt, y ~ mpg),
      mark_symbol(),
      branch_smooth(
        n = input_slider(min = 2, max = 80, value = 5, step = 1,
                         label = "Interpolation points"),
        method = input_select(c("Linear" = "lm", "LOESS" = "loess"),
                              label = "Smooth method")
      )
    )
  })

  # Set up observers for the spec and the data
  observeGigvis(gv, "plot1", session)

  # User interface elements (in the sidebar)
  output$gigvis_ui <- renderControls(gv, session)


  # A second plot, without controls
  gv2 <- reactive({
    gigvis(mtc, props(x ~ wt, y ~ mpg, fill ~ factor(cyl)),
      mark_symbol()
    )
  })
  observeGigvis(gv2, "plot2", session, width = 250, height = 250)


  # Stop the app when the quit button is clicked
  observe({
    if (is.null(input$quit)) return()
    if (input$quit > 0) stopApp()
  })

})
