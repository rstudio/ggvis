\name{input}
\alias{input}
\alias{is.input}
\title{Create an interactive input.}
\usage{
  input(subclass, fun, controls = NULL, id = rand_id())

  is.input(x)
}
\arguments{
  \item{class}{The name of a class to be used in addition
  to "input". This is useful for functions that generate
  controls from the object.}

  \item{fun}{Must always return a value - see
  \code{from_input} one way of ensuring the function yields
  a value even before the reactiveValues have be
  initialised for the first time by user input. It is
  passed either no arguments, or if it has a single
  argument called session, the shiny session object (from
  which input and output can be extract)}

  \item{controls}{a control object}

  \item{id}{a unique identifier for this reactive - used to
  de-duplicate the controls when the same interactive input
  is used in multiple places in a visualisation}

  \item{x}{object to test for "input"-ness}
}
\description{
  An interactive input represents a reactive value bound to
  a UI control. Interactive inputs were previously called
  delayed reactives because they represent an reactive
  value that would be created when the plot was drawn.
}
\keyword{internal}

