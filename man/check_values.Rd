\name{check_values}
\alias{check_values}
\title{Check transformation values are ok}
\usage{
  check_values(x)
}
\arguments{
  \item{x}{a \code{\link{transform}} object}
}
\value{
  invisible TRUE on success, otherwise throws an error
}
\description{
  Subclasses of \code{\link{transform}} can optionally
  implement this method to check that the parameters of the
  transformation are valid. It is called every time before
  the transformation is computed (because the parameters
  may be reactive and changing over time), so should be
  quick to execute.
}
\keyword{internal}

