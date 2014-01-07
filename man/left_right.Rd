\name{left_right}
\alias{left_right}
\title{An interactive input bound to the left and right arrows.}
\usage{
left_right(min, max, value = min, step = (max - min)/50)
}
\arguments{
  \item{value}{The initial value before any keys are
  pressed.}

  \item{step}{How much each key press changes
  \code{value}.}

  \item{min}{The minimum value (inclusive) that can be
  selected.}

  \item{max}{The maximum value (inclusive) that can be
  selected.}
}
\description{
An interactive input bound to the left and right arrows.
}
\examples{
p <- ggvis(mtcars, props(x = ~mpg, y = ~wt, size = left_right(1, 100)),
  mark_symbol())
}

