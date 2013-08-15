\name{scale_time}
\alias{scale_time}
\title{Date time scale.}
\usage{
  scale_time(name, utc = FALSE, clamp = FALSE, nice = NULL,
    domain = NULL, range = NULL, reverse = FALSE,
    round = FALSE)
}
\arguments{
  \item{clamp}{If true, values that exceed the data domain
  are clamped to either the minimum or maximum range
  value.}

  \item{nice}{If specified, modifies the scale domain to
  use a more human-friendly value range. Should be a string
  indicating the desired time interval; legal values are
  "second", "minute", "hour", "day", "week", "month", or
  "year"}

  \item{utc}{if \code{TRUE}, uses UTC times.}

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
  Date time scale.
}
\examples{
scale_time("x", nice = "year")
scale_time("x", utc = TRUE)
}
\seealso{
  \url{https://github.com/trifacta/vega/wiki/Scales#time-scale-properties}

  Other vega scales: \code{\link{scale_ordinal}},
  \code{\link{scale_quantitative}}
}

