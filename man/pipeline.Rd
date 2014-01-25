\name{pipeline}
\alias{as.pipeline}
\alias{is.pipeline}
\alias{pipeline}
\title{Create a new "pipeline" object.}
\usage{
pipeline(..., .pipes = list(), .id = NULL)

is.pipeline(x)

as.pipeline(x, ...)
}
\arguments{
  \item{...}{a list of pipes}

  \item{.pipes}{if you already have the pipes in a list,
  use this argument.}

  \item{x}{an object to test/coerce}

  \item{.id}{Use a specific ID for this pipeline instead of
  an auto-generated one. This is primarily for internal
  use.}
}
\description{
A pipeline represents a sequence of data transformations,
and is the key data structure that underlies the data
hierarchy. The most important method for a pipeline is
\code{\link{connect}}, which connects together the multiple
pipelines that underlying a ggvis graphic to create a
reactive pipeline which is automatically updated when the
underlying data changes.
}
\details{
This function allows you to explicitly connect a series of
\code{\link{pipe}} objects into a pipeline. You should
never need to call it explicitly as \code{\link{ggvis}} and
\code{\link{layer}} automatically create a pipeline using
\code{as.pipeline}.
}
\examples{
pipeline(mtcars)
as.pipeline(mtcars)
pipeline(cars = mtcars)

# A pipeline can contain multiple data sets, but only the last one is
# returned
pipeline(mtcars, sleep)

# More useful pipelines combine data and transformations
pipeline(mtcars, transform_bin())
pipeline(mtcars, by_group(cyl), transform_bin())
}
\keyword{internal}

