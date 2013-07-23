\name{variable}
\alias{is.variable}
\alias{variable}
\title{Property: variable}
\usage{
  variable(x, scale = TRUE)

  is.variable(x)
}
\arguments{
  \item{x}{A quoted object}
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

v <- variable(quote(cyl))
prop_value(v, mtcars)
}

