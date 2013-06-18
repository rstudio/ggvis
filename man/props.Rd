\name{props}
\alias{props}
\title{Manage a list of properties.}
\usage{
  props(..., inherit = TRUE)
}
\arguments{
  \item{...}{A set of name-value pairs. The name should be
  a valid vega property.}

  \item{inherit}{If \code{TRUE}, the defaults, will inherit
  from properties from the parent node. If \code{FALSE}, it
  will start from nothing.}
}
\description{
  Manage a list of properties.
}
\examples{
# Set to constant values
props(x = 1, y = 2)
# Map to variables in the dataset
props(x ~ mpg, y ~ cyl)
# Set to a constant value in the data space
props(x ~ 1, y ~ 1)
}

