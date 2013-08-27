\name{scale_quantitative}
\alias{scale_quantitative}
\title{Create a quantitative scale}
\usage{
  scale_quantitative(name, trans = "linear",
    exponent = NULL, clamp = FALSE, nice = TRUE,
    zero = FALSE, domain = NULL, range = NULL,
    reverse = FALSE, round = FALSE)
}
\arguments{
  \item{trans}{A scale transformation: one of "linear",
  "log", "pow", "sqrt", "quantile", "quantize",
  "threshold"}

  \item{exponent}{Sets the exponent of the scale
  transformation. For pow transform only.}

  \item{clamp}{If \code{TRUE}, values that exceed the data
  domain are clamped to either the minimum or maximum range
  value.}

  \item{nice}{If \code{TRUE}, modifies the scale domain to
  use a more human-friendly number range (e.g., 7 instead
  of 6.96).}

  \item{zero}{If \code{TRUE}, ensures that a zero baseline
  value is included in the scale domain. This option is
  ignored for non-quantitative scales.}

  \item{name}{name of the scale.}

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
}
\description{
  A quantitative scale controls the mapping of continuous
  variables to visual properties.
}
\details{
  Generally, you should create new scales with
  \code{\link{dscale}} because that will automatically set
  the range to a reasonable default, and it will
  automatically pick the correct type of scale given the
  variable type.
}
\examples{
scale_quantitative("y")
dscale("y", "numeric")

scale_quantitative("y", "pow", 0.5)
dscale("y", "numeric", trans = "pow", exp = 0.5)

scale_quantitative("x", clamp = TRUE, nice = FALSE, zero = TRUE)
dscale("x", "numeric", clamp = TRUE, nice = FALSE, zero = TRUE)
}
\seealso{
  \url{https://github.com/trifacta/vega/wiki/Scales#quantitative-scale-properties}

  Other vega scales: \code{\link{scale_ordinal}},
  \code{\link{scale_time}}
}

