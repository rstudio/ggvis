\name{variable}
\alias{is.variable}
\alias{variable}
\title{Property: variable}
\usage{
  variable(x, scale = TRUE, offset = NULL, mult = NULL)

  is.variable(x)
}
\arguments{
  \item{x}{A quoted object}

  \item{scale}{Should the value be scaled? \code{FALSE},
  the default, then the value will be left as is. If
  \code{TRUE}, the default scale for that property will be
  used. Otherwise, you can supply the name of a specific
  scale as a string.}

  \item{offset}{A simple additive offset to bias the final
  value, equivalent to (value + offset). Offsets are added
  after any scale transformation and multipliers.}

  \item{mult}{A multiplier for the value, equivalent to
  (mult * value).  Multipliers are applied after any scale
  transformation.}
}
\description{
  Given a quoted object, wrap it in a list and attach a
  class. The list-wrapping is needed because attaching a
  class directly to a symbol is not possible.
}
\details{
  Long-term this function needs to behave more like
  dplyr::partial_eval so that it captures local values
  immediately.
}
\examples{
variable(quote(x))
variable(quote(1))
variable(quote(x * y))

variable(quote(cyl))
variable(quote(cyl), offset = -1, scale = FALSE)
}

