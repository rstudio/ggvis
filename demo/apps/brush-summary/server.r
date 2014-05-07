library(ggvis)

data(diamonds, package = "ggplot2")
diamonds <- diamonds[sample(1:nrow(diamonds), 1000), ]

brush_summary <- function(items) {
  if (length(items) == 0) {
    return(data.frame(wt = numeric(0), mpg = numeric(0)))
  }

  data.frame(
    wt  = vapply(items, `[[`, "wt",  FUN.VALUE = numeric(1)),
    mpg = vapply(items, `[[`, "mpg", FUN.VALUE = numeric(1))
  )
}

shinyServer(function(input, output, session) {
  under_brush <- function(value, ...) {
    df <- brush_summary(value$items)

    output$brush_data <- renderPrint({
      cat("Number of points selected: ", nrow(df), "\n\n")
      print(summary(df))
    })
  }

  gv <- reactive({
    mtcars %>% ggvis(~wt, ~mpg, fill.brush := "blue") %>%
      layer_points() %>%
      handle_brush(under_brush)
  })

  # Set up observers for the spec and the data
  observe_ggvis(gv, "plot1", session)

})
