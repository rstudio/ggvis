\name{scale}
\alias{scale}
\title{Create a new vega scale object}
\usage{
  scale(name, type = NULL, domain = NULL, range = NULL,
    reverse = FALSE, round = FALSE, ..., subclass = NULL)
}
\arguments{
  \item{name}{name of the scale.}

  \item{type}{type of scale. Should be one of "linear",
  "ordinal", "time", "utc", "linear", "log", "pow", "sqrt",
  "quantile", "quantize", "threshold".}

  \item{domain,range}{For ordinal scales, a character
  vector. For quantitative scales, a numeric vector of
  length two. Either value (but not both) may be missing,
  in which case \code{domainMin}/\code{rangeMin} or
  \code{domainMax}/\code{rangeMin} is set.}

  \item{reverse}{If true, flips the scale range.}

  \item{round}{If true, rounds numeric output values to
  integers. This can be helpful for snapping to the pixel
  grid.}

  \item{...}{other named arguments.}

  \item{subclass}{Class name for subclass.  Will have
  \code{scale_} prepended.}
}
\description{
  Usually you would not call this function directly, but
  would instead call one of the subclasses.
}
\examples{
scale("x", "linear")
scale("x", "ord")
}
\seealso{
  \url{https://github.com/trifacta/vega/wiki/Scales}
}
\keyword{internal}

