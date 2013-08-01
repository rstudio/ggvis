\name{gigvis}
\alias{gigvis}
\alias{is.gigvis}
\alias{node}
\title{Create a gigvis object, or a node in a gigvis plot.}
\usage{
  gigvis(data = NULL, props = NULL, ..., scales = NULL,
    axes = NULL, legends = NULL)

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

  \item{...}{children \code{node}s or \code{\link{marks}}.}

  \item{scales}{a \code{\link{scales}} object, to override
  the default scales}

  \item{axes}{a list of \code{\link{axis}} specifications}

  \item{legends}{a list of \code{\link{axis}}
  specifications}
}
\description{
  Create a gigvis object, or a node in a gigvis plot.
}

