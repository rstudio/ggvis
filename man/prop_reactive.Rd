\name{prop_reactive}
\alias{is.prop_reactive}
\alias{prop_reactive}
\title{Property: reactive}
\usage{
  prop_reactive(x, constant = TRUE, scale = TRUE,
    offset = NULL, mult = NULL)

  is.prop_reactive(x)
}
\arguments{
  \item{x}{A delayed reactive object}

  \item{constant}{is it constant or variable}

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
  Property: reactive
}
\examples{
prop_reactive(input_select(c("red", "blue")), TRUE, FALSE)
prop_reactive(input_select(c("a", "b")), TRUE, TRUE)
prop_reactive(input_select(c("mpg", "wt")), FALSE, TRUE)
# If the data frame has colums 'colour' and 'colour2', which contain actual
# colors:
prop_reactive(input_select(c("colour", "colour2")), FALSE, FALSE)
}

