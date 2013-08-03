\name{delayed_reactive}
\alias{delayed_reactive}
\title{Create a delayed reactive.}
\usage{
  delayed_reactive(fun, controls = NULL, id = rand_id())
}
\arguments{
  \item{fun}{Must always return a value - see
  \code{from_input} one way of ensuring the function yields
  a value even before the reactiveValues have be
  initialised for the first time by user input. It is
  passed either no arguments, or if it has a single
  argument called session, the shiny session object (from
  which input and output can be extract)}

  \item{controls}{a control object}

  \item{id}{a unique identifier for this reactive - used to
  de-duplicate the controls when the same delayed reactive
  is used in multiple places in a visualisation}
}
\description{
  A delayed reactive allows us to specify reactive values
  outside of a shiny app. They are used to specify
  interactive behaviour (e.g. \code{\link{input_slider}})
  that is instantiated when the visualisation is launched.
}
\keyword{internal}

