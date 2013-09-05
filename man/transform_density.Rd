\name{transform_density}
\alias{transform_density}
\title{Transformation: density estimate}
\usage{
  transform_density(..., adjust = 1, kernel = "gaussian",
    trim = FALSE, n = 200L, na.rm = FALSE)
}
\arguments{
  \item{adjust}{Bandwidth adjustment. See
  \code{\link{density}} for details.}

  \item{kernel}{Smoothing kernel. See \code{\link{density}}
  for details.}

  \item{trim}{If \code{TRUE}, the default, density
  estimates are trimmed to the actual range of the data.
  If \code{FALSE}, they are extended by the default 3
  bandwidths (as specified by the \code{cut} parameter to
  \code{\link{density}}).}

  \item{n}{Number of points (along x) to use in the density
  estimate.}

  \item{na.rm}{If \code{TRUE} missing values will be
  silently removed, otherwise they will be removed with a
  warning.}
}
\description{
  Transformation: density estimate
}

