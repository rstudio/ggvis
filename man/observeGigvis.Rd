\name{observeGgvis}
\alias{observeGgvis}
\title{Set up Shiny observers for a dynamic gigvis plot}
\usage{
  observeGgvis(r_gv, id, session, renderer = "canvas",
    ...)
}
\arguments{
  \item{r_gv}{A reactive expression which returns a gigvis
  object.}

  \item{id}{The ID of the plot on the web page.}

  \item{session}{A Shiny session object.}

  \item{renderer}{The renderer type ("canvas" or "svg")}

  \item{...}{Other arguments passed to \code{as.vega}.}
}
\description{
  Set up Shiny observers for a dynamic gigvis plot
}

