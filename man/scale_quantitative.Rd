\name{scale_quantitative}
\alias{scale_quantitative}
\title{Quantitative scale}
\usage{
  scale_quantitative(name, trans = "linear",
    exponent = NULL, clamp = FALSE, nice = TRUE,
    zero = FALSE, domain = NULL, range = NULL,
    reverse = FALSE, round = FALSE)
}
\arguments{
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

  \item{reverse}{If true, flips the scale range.}

  \item{round}{If true, rounds numeric output values to
  integers. This can be helpful for snapping to the pixel
  grid.}
}
\description{
  Quantitative scale
}
\examples{
scale_quantitative("y")
scale_quantitative("y", "pow", 0.5)
scale_quantitative("x", clamp = TRUE, nice = FALSE, zero = TRUE)
}
\seealso{
  \url{https://github.com/trifacta/vega/wiki/Scales#quantitative-scale-properties}

  Other vega.scales: \code{\link{scale_ordinal}},
  \code{\link{scale_time}}
}

