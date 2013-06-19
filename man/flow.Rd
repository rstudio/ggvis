\name{flow}
\alias{flow}
\title{Flow data down a pipe or pipeline}
\usage{
  flow(x, data = NULL, props, ...)
}
\arguments{
  \item{x}{a pipeline or pipe}

  \item{data}{the data source to start the flow}

  \item{properties}{a \code{props} object}
}
\description{
  This flows data down a pipeline (starting with
  \code{NULL}), applying each pipe (transformation) in
  sequence.
}
\keyword{internal}

