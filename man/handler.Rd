\name{handler}
\alias{handler}
\alias{is.handler}
\title{Create handler S3 class.}
\usage{
handler(subclass, listener, control_args = list(), value = NULL,
  map = identity, id = rand_id())

is.handler(x)
}
\arguments{
  \item{subclass}{name of the subclass. \code{handler} is
  an abstract base class so this must always be provided.}

  \item{listener}{name of the js listener (with
  corresponding R event broker) that this handler is
  associated with}

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

  \item{x}{object to test for "input"-ness}
}
\description{
This is currently a subclass on input, but it should
probably be the other way around since inputs are handlers
that have controls.
}
\examples{
p <- ggvis(mtcars, props(x = ~mpg, y = ~wt, size = left_right(1, 100)),
  mark_symbol())
p$props$size.update$dr

# Handlers are extracted with the internal handlers() function
# ggvis:::handlers(p)
}

