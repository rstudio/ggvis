\name{renderControls}
\alias{renderControls}
\title{Render the controls for a gigvis object in a Shiny app}
\usage{
  renderControls(r_gv, session = NULL)
}
\description{
  Render the controls for a gigvis object in a Shiny app
}
\examples{
\dontrun{
# In server.r
gv <- reactive({
  gigvis(mtcars, props(x ~ wt, y ~ mpg),
    mark_symbol(),
    branch_smooth(
      n = input_slider(2, 80, "Interpolation points", value = 5, step = 1),
      method = input_select(c("Linear" = "lm", "LOESS" = "loess"))
    )
  )
})

output$controls <- renderControls(gv)
}
}

