\name{default_scale}
\alias{default_scale}
\title{Create a default scale for a given property and variable type.}
\usage{
  default_scale(prop, type, ...)
}
\arguments{
  \item{prop}{A vega property name, converted into a scale
  name by \code{\link{prop_to_scale}}}

  \item{type}{A variable type, as returned by
  \code{\link{prop_type}}}

  \item{...}{other arguments passed to the scale function}
}
\description{
  Default scales depend on both the property (e.g. fill, x,
  opacity) and the type of variable (e.g. numeric, nominal,
  ordinal). For this reason \code{default_scale} implements
  a crude S3-like double dispatch mechanism, looking for a
  function called \code{dscale_prop_type}.
}

