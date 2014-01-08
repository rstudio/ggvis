\name{left_right}
\alias{left_right}
\title{An interactive input bound to the left and right arrows.}
\usage{
left_right(min, max, value = (min + max)/2, step = (max - min)/40)
}
\arguments{
  \item{value}{The initial value before any keys are
  pressed. Defaults to half-way between \code{min} and
  \code{max}.}

  \item{step}{How much each key press changes \code{value}.
  Defaults to 40 steps along range}

  \item{min}{The minimum value (inclusive) that can be
  selected.}

  \item{max}{The maximum value (inclusive) that can be
  selected.}
}
\description{
An interactive input bound to the left and right arrows.
}
\examples{
ggvis(mtcars, props(x = ~mpg, y = ~wt, size := left_right(1, 100))) +
  mark_symbol()
}

