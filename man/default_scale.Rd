\name{default_scale}
\alias{default_scale}
\alias{dscale}
\title{Create a default scale for a given property and variable type.}
\usage{
  default_scale(prop, type, ..., name = NULL)

  dscale(prop, type, ..., name = NULL)
}
\arguments{
  \item{prop}{A vega property name.}

  \item{type}{A variable type.  One of datetime, numeric,
  ordinal, nominal, logical.}

  \item{...}{other arguments passed to the scale function.
  See the help for \code{\link{scale_quantitative}},
  \code{\link{scale_ordinal}} and \code{\link{scale_time}}
  for more details. For example, you might supply
  \code{trans = "log"} to create a log scale.}

  \item{name}{If \code{NULL}, the default, the scale name
  is computed by calling \code{\link{prop_to_scale}(prop)}.
  This ensures that by default properties like \code{y} and
  \code{y2}, or \code{opacity}, \code{fillOpacity} and
  \code{strokeOpacity} all share the same scale.  Set this
  to a custom name to override that behaviour, or to create
  multiple scales for stroke or fill, or (god forbid) a
  secondary y scale.}
}
\description{
  Default scales depend on both the property (e.g. fill, x,
  opacity) and the type of variable (e.g. numeric, nominal,
  ordinal). For this reason \code{default_scale} implements
  a crude S3-like double dispatch mechanism, looking for a
  function called \code{dscale_prop_type}. \code{dscale} is
  a short-hand useful for interactive exploration.
}
\section{Scale selection}{
  First, the type of scale is selected based on the
  \code{type}:

  \itemize{ \item datetime: \code{\link{scale_time}} \item
  ordinal, nominal, logical: \code{\link{scale_ordinal}}
  \item numeric: \code{\link{scale_quantitative}} }

  (see \code{\link{proptype_to_scale}} for more details)

  then the range is selected based on the combination of
  the \code{type} and the \code{prop} - for example, you
  get a different range of colours depending on whether the
  data is numeric, ordinal, or nominal. Some scales also
  set other properties - for example, nominal/ordinal
  position scales also add some padding so that points are
  spaced away from plot edges.

  Not all combinations have an existing default scale - if
  you use a combination that does not have an existing
  combination, it may suggest you're displaying the data in
  a suboptimal way. For example, there is no default for a
  numeric shape scale, because there's no obvious way to
  map continuous values to discrete shapes. On the other
  hand, I may have just forgotten to add the appropriate
  default :/.

  You can add your own defaults (or override existing) by
  calling \code{\link{add_scale_defaults}}: just be aware
  that this is a global setting.
}
\examples{
default_scale("x", "numeric")
default_scale("fillOpacity", "ordinal")
default_scale("stroke", "nominal")

# You can also supply additional arguments or override the defaults
default_scale("x", "numeric", trans = "log")
default_scale("stroke", "nominal", range = c("red", "blue"))
}

