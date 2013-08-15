\name{scales}
\alias{is.scales}
\alias{scales}
\title{Create a ggvis_scales object}
\usage{
  scales(..., .scales = list())

  is.scales(x)
}
\arguments{
  \item{...,.scales}{scales to combine into a single scales
  object}

  \item{x}{object to test for scales-ness}
}
\description{
  Given arguments which are scales object, return a list
  with those objects, where the entries also have names
  that are the same as the "name" field of the objects.
}
\keyword{internal}

