\name{left_right}
\alias{left_right}
\title{An interactive input bound to the left and right arrows.}
\usage{
left_right(min, max, value = min, step = (max - min)/50)
}
\arguments{
  \item{min}{The minimum value (inclusive) that can be
  selected.}

  \item{max}{The maximum value (inclusive) that can be
  selected.}

  \item{value}{The initial value of the slider. A numeric
  vector of length one will create a regular slider; a
  numeric vector of length two will create a double-ended
  range slider. A warning will be issued if the value
  doesn't fit between \code{min} and \code{max}.}

  \item{step}{Specifies the interval between each
  selectable value on the slider (\code{NULL} means no
  restriction).}
}
\description{
An interactive input bound to the left and right arrows.
}
\examples{
ggvis(mtcars, props(x = ~mpg, y = ~wt, size = left_right(1, 100)),
  mark_symbol())
}

