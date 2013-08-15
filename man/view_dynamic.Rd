\name{view_dynamic}
\alias{view_dynamic}
\title{Generate a dynamic shiny app with the embedded ggvis graph}
\usage{
  view_dynamic(gv, renderer = "canvas", launch = TRUE,
    port = 8228)
}
\arguments{
  \item{port}{the port on which to start the shiny app}

  \item{gv}{A ggvis object.}

  \item{renderer}{The renderer to use in the browser. Can
  be \code{"canvas"} (the default) or \code{"svg"}.}

  \item{launch}{If \code{TRUE}, launch this web page in a
  browser.}
}
\description{
  Generate a dynamic shiny app with the embedded ggvis
  graph
}
\keyword{internal}

