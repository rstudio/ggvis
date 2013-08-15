\name{scale}
\alias{is.scale}
\alias{scale}
\title{Create a new vega scale object}
\usage{
  scale(name, type = NULL, domain = NULL, range = NULL,
    reverse = FALSE, round = FALSE, ..., subclass = NULL)

  is.scale(x)
}
\arguments{
  \item{name}{name of the scale.}

  \item{type}{type of scale. Should be one of "linear",
  "ordinal", "time", "utc", "linear", "log", "pow", "sqrt",
  "quantile", "quantize", "threshold".}

  \item{domain}{The domain of the scale, representing the
  set of data values.  For ordinal scales, a character
  vector; for quantitative scales, a numeric vector of
  length two. Either value (but not both) may be missing,
  in which case \code{domainMin} or \code{domainMax} is
  set.}

  \item{range}{The range of the scale, representing the set
  of visual values.  For numeric values, the range can take
  the form of a two-element array with minimum and maximum
  values. For ordinal data, the range may by an array of
  desired output values, which are mapped to elements in
  the specified domain. The following range literals are
  also available: "width", "height", "shapes",
  "category10", "category20".}

  \item{reverse}{If true, flips the scale range.}

  \item{round}{If true, rounds numeric output values to
  integers. This can be helpful for snapping to the pixel
  grid.}

  \item{...}{other named arguments.}

  \item{subclass}{Class name for subclass.  Will have
  \code{scale_} prepended.}

  \item{x}{object to test for scale-ness}
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

