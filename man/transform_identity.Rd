\name{transform_identity}
\alias{transform_identity}
\title{Transformation: identity.}
\usage{
  transform_identity()
}
\description{
  This transformation simply evaluates each property in the
  context of the data.
}
\examples{
transform_identity()
sluice(transform_identity(), props(x ~ disp), mtcars)
}

