\name{band}
\alias{band}
\alias{is.band}
\title{A band}
\usage{
  band(offset = NULL, mult = NULL)

  is.band(x)
}
\arguments{
  \item{offset}{A simple additive offset to bias the final
  value, equivalent to (value + offset). Offsets are added
  after any scale transformation and multipliers.}

  \item{mult}{A multiplier for the value, equivalent to
  (mult * value).  Multipliers are applied after any scale
  transformation.}
}
\description{
  Bands are used to set the width or height on categorical
  scales.
}

