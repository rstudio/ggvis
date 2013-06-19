\name{flow}
\alias{flow}
\title{Flow data down a pipe or pipeline}
\usage{
  flow(x, props, data = NULL)
}
\arguments{
  \item{x}{a pipeline or pipe}

  \item{properties}{a \code{props} object}

  \item{data}{the data source to start the flow}
}
\description{
  This flows data down a pipeline (starting with
  \code{NULL}), applying each pipe (transformation) in
  sequence.
}
\details{
  Every element in a pipeline recieves the same set of
  properties: this generally means that for more
  complicated transformations you will need to create
  multiple branches.
}
\keyword{internal}

