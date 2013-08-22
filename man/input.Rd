\name{input}
\alias{input}
\alias{is.input}
\title{Create an interactive input.}
\usage{
  input(subclass, control_args = list(), value = NULL,
    map = identity, id = rand_id(), scale = FALSE,
    control_f = NULL)

  is.input(x)
}
\arguments{
  \item{subclass}{The name of a class to be used in
  addition to "input". Automatically prefixed with
  "input_"}

  \item{control_args}{a list of arguments passed to
  \code{control_f}}

  \item{value}{the default value of the input}

  \item{map}{a function with a singe argument that takes
  the value returned from the input control and converts it
  to an argument useful for ggvis.  Defaults to
  \code{identity}, leaving the output unchanged.}

  \item{id}{a unique identifier for this interactive input
  - used to de-duplicate the controls when the same input
  is used in multiple places in a visualisation}

  \item{scale}{default scale value when this input is used
  in a prop}

  \item{control_f}{The name of a function used to create an
  html control.}

  \item{x}{object to test for "input"-ness}
}
\description{
  An interactive input represents a reactive value bound to
  a UI control. Interactive inputs were previously called
  delayed reactives because they represent an reactive
  value that would be created when the plot was drawn.
}
\keyword{internal}

