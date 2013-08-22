\name{props}
\alias{is.ggvis_props}
\alias{props}
\title{Manage a list of properties.}
\usage{
  props(..., inherit = TRUE)

  is.ggvis_props(x)
}
\arguments{
  \item{...}{A set of name-value pairs. The name should be
  a valid vega property.}

  \item{inherit}{If \code{TRUE}, the defaults, will inherit
  from properties from the parent node. If \code{FALSE}, it
  will start from nothing.}

  \item{x}{an object to test for props-ness.}
}
\description{
  \code{props()} provides a tool for concise creation of
  \code{prop} objects using a set of conventions designed
  to capture the most common use cases. If you need
  something less common, you'll need to use
  \code{\link{prop}} to access all possible options. See
  the \link{marks} documentation to see what properties the
  marks understand.
}
\section{Heuristics}{
  If the values are not already objects of class
  \code{prop}, \code{props} uses the following heuristics
  to when creating the prop:

  \itemize{ \item atomic vectors, e.g. \code{x = 1}: scaled
  = FALSE \item an interative input, e.g. \code{x =
  input_slider}: scaled = FALSE \item a formula containing
  a single value, e.g. \code{x ~ 1}: scaled = TRUE \item a
  formula containing a name or expression, \code{x ~ mpg}:
  scaled = TRUE }
}

\section{Non-standard evaluation}{
  \code{props} uses non-standard evaluation in a slightly
  unusual way: if you provide a formula input, the LHS of
  the formula will provide the name of the component. In
  otherwise, \code{props(x = y ~ 1)} is the same as
  \code{props(y ~ 1)}.

  You can combine variables from the dataset and variables
  defined in the local environment: expressions will be
  evaluated in the environment which the formula was
  defined.

  If you have the name of a variable in a string, see the
  props vignette for how to create the needed property
  mapping.
}
\examples{
# Set to constant values
props(x = 1, y = 2)
# Map to variables in the dataset
props(x ~ mpg, y ~ cyl)
# Set to a constant value in the data space
props(x ~ 1, y ~ 1)
# Use an interactive slider
props(opacity = input_slider(0, 1))
# Use any other prop settings
props(x = prop("old", scale = TRUE, offset = -1))
}

