\name{ggvisControlOutput}
\alias{ggvisControlOutput}
\title{Create a ggvis control output element in UI}
\usage{
  ggvisControlOutput(outputId, plotId = NULL)
}
\arguments{
  \item{outputId}{The output variable to read the value
  from.}

  \item{plotId}{An optional plot ID or vector of plot IDs.
  The plots will have their .onControlOutput functions
  called after the controls are drawn.}
}
\description{
  This is effectively the same as
  \code{\link{shiny::uiOutput}}, except that on the client
  side it may call some plot resizing functions after new
  controls are drawn.
}
\details{
  \code{ggvisControlOutput} is intended to be used with
  \code{\link{renderControls}} on the server side.
}
\examples{
htmlOutput("summary")
}

