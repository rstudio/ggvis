\name{gigvis}
\alias{gigvis}
\alias{is.gigvis}
\alias{node}
\title{Create a gigvis object, or a node in a gigvis plot.}
\usage{
  gigvis(data = NULL, props = NULL, ...)

  is.gigvis(x)

  node(..., data = NULL, props = NULL)
}
\arguments{
  \item{data}{A data \code{\link{pipeline}}, or anything
  that can be coerced to a pipeline using
  \code{\link{as.pipeline}}}

  \item{props}{a list of \code{\link{props}} defining
  default mark properties for this node and all its
  children.}

  \item{...}{components: \code{node}s, \code{\link{marks}},
  \code{\link{scales}}, \code{\link{axis}} or
  \code{\link{legend}} objects}
}
\description{
  Create a gigvis object, or a node in a gigvis plot.
}

