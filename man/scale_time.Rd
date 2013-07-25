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

  \item{utc}{if \code{TRUE}, uses UTC times.}

  \item{name}{name of the scale.}

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

  Other vega.scales: \code{\link{scale_ordinal}},
  \code{\link{scale_quantitative}}
}

