\name{constant}
\alias{constant}
\title{Property: constant}
\usage{
  constant(value, scale = FALSE)
}
\arguments{
  \item{value}{The value of the constant. If \code{scale =
  FALSE}, the default then this value needs to be
  interpretable in the aesthetic space (e.g.  for colour,
  "red"; for position or size, an integer number of
  pixels).  Otherwise, it will be scaled before plot}

  \item{scale}{Should the value be scaled? \code{FALSE},
  the default, then the value will be left as is. If
  \code{TRUE}, the default scale for that property will be
  used. Otherwise, you can supply the name of a specific
  scale as a string.}
}
\description{
  Use a constant value for a mark property.
}
\examples{
constant("red")
constant("red", scale = TRUE)
constant("red", scale = "alarm")
}

