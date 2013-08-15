\name{renderControls}
\alias{renderControls}
\title{Render the controls for a ggvis object in a Shiny app}
\usage{
  renderControls(r_gv, session = NULL)
}
\arguments{
  \item{r_gv}{a ggvis object wrapped in a reactive}

  \item{session}{the session argument from the shiny server
  function}
}
\description{
  Render the controls for a ggvis object in a Shiny app
}
\examples{
\dontrun{
# In server.r
gv <- reactive({
  ggvis(mtcars, props(x ~ wt, y ~ mpg),
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

