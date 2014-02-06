\name{waggle}
\alias{waggle}
\title{Waggle back and forth between two numbers}
\usage{
waggle(min, max, value = (min + max)/2, step = (max - min)/60, fps = 30)
}
\arguments{
  \item{value}{Starting value. Defaults to half-way between
  \code{min} and \code{max}.}

  \item{step}{How much value changes at each frame.
  Defaults to 60 steps between min and max so it takes 2
  seconds to waggle once.}

  \item{fps}{number of frames per second.}

  \item{min}{The minimum value (inclusive) that can be
  selected.}

  \item{max}{The maximum value (inclusive) that can be
  selected.}
}
\description{
Waggle back and forth between two numbers
}
\examples{
span <- waggle(0.2, 1)
ggvis(mtcars, props(~mpg, ~wt)) +
 layer_point() +
 layer_smooth(span = span, method = "loess", formula = y ~ x)
}

