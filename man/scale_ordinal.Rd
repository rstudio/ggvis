\name{scale_ordinal}
\alias{scale_ordinal}
\title{Ordinal scale}
\usage{
  scale_ordinal(name, points = TRUE, padding = NULL,
    sort = TRUE, domain = NULL, range = NULL,
    reverse = FALSE, round = FALSE)
}
\arguments{
  \item{points}{If \code{TRUE}, distributes the ordinal
  values over a quantitative range at uniformly spaced
  points. The spacing of the points can be adjusted using
  the padding property. If \code{FALSE}, the ordinal scale
  will construct evenly-spaced bands, rather than points.}

  \item{padding}{Applies spacing among ordinal elements in
  the scale range.  The actual effect depends on how the
  scale is configured. If the points parameter is true, the
  padding value is interpreted as a multiple of the spacing
  between points. A reasonable value is 1.0, such that the
  first and last point will be offset from the minimum and
  maximum value by half the distance between points.
  Otherwise, padding is typically in the range [0, 1] and
  corresponds to the fraction of space in the range
  interval to allocate to padding. A value of 0.5 means
  that the range band width will be equal to the padding
  width.}

  \item{sort}{If \code{TRUE}, the values in the scale
  domain will be sorted according to their natural order.
  The default value is \code{FALSE}.}

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
  Ordinal scale
}
\examples{
scale_ordinal("x")
scale_ordinal("x", padding = 0.5, sort = TRUE, points = FALSE)
}
\seealso{
  \url{https://github.com/trifacta/vega/wiki/Scales#ordinal-scale-properties}

  \url{https://github.com/mbostock/d3/wiki/Ordinal-Scales}

  Other vega scales: \code{\link{scale_quantitative}},
  \code{\link{scale_time}}
}

