\name{add_scale_defaults}
\alias{add_scale_defaults}
\title{Add new default scale properties.}
\usage{
  add_scale_defaults(scale, types, ...)
}
\arguments{
  \item{scale}{scale name}

  \item{types}{a vector of property types, converted to
  scale types by \code{\link{proptype_to_scale}}}

  \item{...}{other arguments to be passed onto the scale
  function}
}
\description{
  You can use this function to add new defaults
}
\examples{
\dontrun{
# Add a default size scale for nominal variable types: this is probably
# a bad idea since sizes are ordered but nominal variables are not. On
# the other hand, many factors should probably actually be ordered factors.
add_scale_defaults("size", "nominal", range = c(1, 20))
}
}
\keyword{internal}

