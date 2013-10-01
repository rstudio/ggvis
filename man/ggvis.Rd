\name{ggvis}
\alias{ggvis}
\title{Create a ggvis graphic.}
\usage{
  ggvis(...)
}
\arguments{
  \item{...}{components: data, \code{\link{props}},
  \code{\link{branch}}es, \code{\link{marks}},
  \code{\link{scales}}, \code{\link{axis}} or
  \code{\link{legend}} objects.}

  \item{width,height}{width and height of plot, in pixels}

  \item{padding}{padding, as described by
  \code{\link{padding}}}
}
\value{
  a \code{branch} object. Will display the plot when
  printed; see \code{\link{save_spec}},
  \code{\link{view_static}} and \code{\link{view_dynamic}}
  for other options.
}
\description{
  Create a ggvis graphic.
}

