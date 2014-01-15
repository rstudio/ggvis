\name{print.ggvis}
\alias{print.ggvis}
\alias{view_dynamic}
\alias{view_static}
\title{View in a ggvis plot in the browser.}
\usage{
\method{print}{ggvis}(x, dynamic = NA, spec = getOption("ggvis.print_spec",
  FALSE), ...)

view_static(x, renderer = getOption("ggvis.renderer", default = "canvas"),
  launch = interactive())

view_dynamic(x, renderer = getOption("ggvis.renderer", default = "canvas"),
  launch = TRUE, port = NULL)
}
\arguments{
  \item{x}{A ggvis object.}

  \item{dynamic}{Uses \code{view_dynamic} if \code{TRUE},
  \code{view_static} if \code{FALSE}. The default,
  \code{NA}, chooses automatically based on the presence of
  reactives or interactive inputs in \code{x}.}

  \item{spec}{If \code{TRUE}, override usual printing and
  instead print the json plot spec. If a character vector,
  will display just those components of the spec. This is
  useful for generating regression tests.}

  \item{...}{Other arguments passed on to
  \code{view_dynamic} and \code{view_static} from
  \code{print}.}

  \item{launch}{If \code{TRUE}, launch this web page in a
  browser or Rstudio.}

  \item{port}{the port on which to start the shiny app. If
  NULL (the default), Shiny will select a random port.}
}
\description{
\code{view_static} creates a static web page in a temporary
directory; \code{view_dynamic} generate a dynamic shiny app
and launches it. \code{print} picks between the two methods
automatically based on whether or not your plot has
interactive features.
}
\keyword{internal}

