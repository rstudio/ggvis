\name{compute}
\alias{compute}
\title{Compute the transformation.}
\usage{
compute(x, props, data)
}
\arguments{
  \item{x}{the \code{\link{transform}} object}

  \item{props}{a list of properties, \code{\link{props}}}

  \item{data}{input data, often (but not necessarily) a
  data frame}
}
\value{
  a data frame
}
\description{
  This generic represents the actual numerical computation
  performed by the transformation. This is the method that
  does most of the work, and only needs to know about props
  and values, not about reactive objects.
}
\keyword{internal}

