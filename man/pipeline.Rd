\name{pipeline}
\alias{as.pipeline}
\alias{is.pipeline}
\alias{pipeline}
\title{A data pipeline.}
\usage{
  pipeline(..., .pipes = list())

  is.pipeline(x)

  as.pipeline(x, ...)
}
\arguments{
  \item{...}{a list of pipes}

  \item{.pipes}{if you already have the pipes in a list,
  use this argument.}

  \item{x}{an object to test/coerce}
}
\description{
  This creates an S3 object that represents pipeline of
  data tranformations. The data argument to a ggvis branch,
  must always be a data pipeline: to ensure this
  \code{as.pipeline} is also called on the input,
  automatically converting simpler expressions (like data
  frames and strings).
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
pipeline(mtcars, by_group("cyl"), transform_bin())
}

