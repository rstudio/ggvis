\name{left_right}
\alias{left_right}
\alias{up_down}
\title{Interactive inputs bound to arrow keys.}
\usage{
left_right(min, max, value = (min + max)/2, step = (max - min)/40)

up_down(min, max, value = (min + max)/2, step = (max - min)/40)
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
Interactive inputs bound to arrow keys.
}
\examples{
size <- left_right(1, 801, value = 51, step = 50)
opacity <- up_down(0, 1, value = 0.9, step = 0.05)

qvis(mtcars, ~mpg, ~wt, size := size, opacity := opacity)
}

