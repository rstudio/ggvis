\name{default_scale}
\alias{default_scale}
\alias{dscale}
\title{Create a default scale for a given property and variable type.}
\usage{
  default_scale(prop, type, ..., name = NULL)

  dscale(prop, type, ..., name = NULL)
}
\arguments{
  \item{prop}{A vega property name, converted into a scale
  name by \code{\link{prop_to_scale}}}

  \item{type}{A variable type, as returned by
  \code{\link{prop_type}}.  One of datetime, numeric,
  ordinal, nominal, logical.}

  \item{...}{other arguments passed to the scale function}

  \item{name}{Use to override the default scale name - this
  is useful if you want to (god forbid) create a secondary
  y scale, or more usefully create multiple stroke or fill
  scales.}
}
\description{
  Default scales depend on both the property (e.g. fill, x,
  opacity) and the type of variable (e.g. numeric, nominal,
  ordinal). For this reason \code{default_scale} implements
  a crude S3-like double dispatch mechanism, looking for a
  function called \code{dscale_prop_type}. \code{dscale} is
  a short-hand useful for interactive exploration.
}
\section{Scale selection

First, the type of scale is selected based on the \code{type}}{
  \itemize{ \item datetime: \code{\link{scale_time}} \item
  ordinal, nominal, logical: \code{\link{scale_ordinal}}
  \item numeric: \code{\link{scale_quantitative}} }

  then the range is selected based on the combination of
  the \code{type} and the \code{prop} - for example, you
  get a different range of colours depending on whether the
  data is numeric, ordinal, or nominal.

  Not all combinations have an existing default scale - if
  you use a combination that does not have an existing
  combination, it may suggest you're displaying the data in
  a suboptimal way. For example, there is no default for a
  numeric shape scale, because there's no obvious way to
  map continuous values to discrete shapes. On the other
  hand, I may have just forgotten to add the appropriate
  default :/.
}
\examples{
default_scale("x", "numeric")
default_scale("fillOpacity", "ordinal")
default_scale("stroke", "nominal")
}

