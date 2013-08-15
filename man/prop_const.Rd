\name{prop_const}
\alias{prop_const}
\title{Property: constant}
\usage{
  prop_const(value = NULL, scale = FALSE, mult = NULL,
    offset = NULL)
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

  \item{mult}{A multiplier for the value, equivalent to
  (mult * value).  Multipliers are applied after any scale
  transformation.}

  \item{offset}{A simple additive offset to bias the final
  value, equivalent to (value + offset). Offsets are added
  after any scale transformation and multipliers.}
}
\description{
  Use a constant value for a mark property.
}
\examples{
prop_const("red")
prop_const("red", scale = TRUE)
prop_const("red", scale = "alarm")
prop_const(offset = -4)
}

