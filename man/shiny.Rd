\name{shiny}
\alias{ggvis_output}
\alias{observe_ggvis}
\alias{renderControls}
\alias{shiny}
\title{Connect a ggvis graphic to a shiny app.}
\usage{
  ggvis_output(plot_id, shiny = TRUE)

  observe_ggvis(r_gv, id, session, ...)

  renderControls(r_gv, session = NULL)
}
\arguments{
  \item{plot_id}{unique identifier to use for the div
  containing the ggvis plot.}

  \item{shiny}{Should this include headers for Shiny? For
  dynamic and interactive plots, this should be TRUE;
  otherwise FALSE.}

  \item{r_gv}{A reactive expression which returns a ggvis
  object.}

  \item{session}{A Shiny session object.}

  \item{...}{Other arguments passed to \code{as.vega}.}
}
\description{
  It's easiest to learn by example: there are two shiny
  apps in \code{demo/apps/} that you can learn from.
}
\details{
  There are two required components and one optional
  component:

  \itemize{ \item Use \code{ggvis_output} in \code{ui.r} to
  insert a placeholder (a div with id) for a ggvis graphic.

  \item Use \code{observe_ggvis} in \code{server.r} to
  insert a ggvis object into a shiny app and set up the
  observers to notify the client side whenever the plot
  data or spec changes.

  \item If the plot uses interactive inputs, use
  \code{renderControls} to insert those controls into the
  ui. }
}
\examples{
\dontrun{
## In server.r
gv <- reactive({
  ggvis(mtcars, props(x = ~wt, y = ~mpg),
    mark_symbol(),
    branch_smooth(
      n = input_slider(2, 80, "Interpolation points", value = 5, step = 1),
      method = input_select(c("Linear" = "lm", "LOESS" = "loess"))
    )
  )
})

output$controls <- renderControls(gv)


## In ui.r
ggvisControlOutput("controls")
}
}

