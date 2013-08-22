\name{prop}
\alias{is.prop}
\alias{prop}
\title{Create a property.}
\usage{
  prop(x, scale = NULL, offset = NULL, mult = NULL,
    env = parent.frame())

  is.prop(x)
}
\arguments{
  \item{x}{The value of the property. This can be an atomic
  vector (a constant), a name or quoted call (a variable),
  or a delayed reactive (which can be either variable or
  constant).}

  \item{scale}{If \code{TRUE} uses the default scale
  associated with property; If \code{FALSE}, does not scale
  the value. Otherwise supply a string to select a custom
  scale. If \code{x} is an interactive input, then this
  defaults to the scale parameter of the input.}

  \item{offset,mult}{Additive and multiplicate pixel offset
  used to adjust scaled values. These are useful if you
  want to place labels offset from points.}

  \item{env}{If \code{x} is a quoted call this provides the
  environment in which to look for variables not in the
  data. You should not need this in ordinary operation.}
}
\description{
  Properties are used to describe the visual properties of
  \link{marks}. You create a single property defintion with
  \code{prop}, and manage sets of named properties with
  \code{\link{props}} (which also provides shortcuts for
  creating the most common kind of properties)
}
\examples{
prop(1)
prop(quote(cyl))

# If you have a variable name as a string
var <- "cyl"
prop(as.name(var))

# Override regular scale
prop(quote(cyl), scale = "y-2")

# Don't scale variable (i.e. it already makes sense in the visual space)
prop(quote(colour), scale = FALSE)
}
\seealso{
  \code{\link{props}} to manage multiple properties and to
  succintly create the most common types.
}

