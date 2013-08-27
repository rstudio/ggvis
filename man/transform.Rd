\name{transform}
\alias{build_transform}
\alias{transform}
\title{Create a new "transform" object.}
\usage{
  transform(type, ..., dots = list())

  build_transform(name, ..., .env = parent.frame())
}
\arguments{
  \item{type}{A string representing type of transform.}

  \item{dots}{A list of arguments to pass to the underlying
  statistical transformation function.}

  \item{...}{Other arguments to pass to the specific
  transform.}

  \item{name}{Name of transform class to create.}
}
\description{
  A transform object is a \code{\link{pipe}} that
  represents a data transformation. \code{build_transform}
  provides an alternative way to create transformation
  objects: it ignores unnamed arguments, making it suitable
  for use inside \code{branch_} functions.
}
\details{
  This function is designed to be used by authors of new
  types of transform If you are a ggvis user, please use
  one of the more specific transform functions starting
  with the \code{transform_}.
}
\section{Important methods}{
  An transform subclass should provide methods for:

  \itemize{ \item \code{\link{compute}}: this method should
  check props, perform the transformation, typically by
  dispatching to another S3 generic based on the type of
  input, and then add back in constant variables with
  \code{preserve_constants}. }
}
\keyword{internal}

