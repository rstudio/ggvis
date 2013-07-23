\name{source_reactive}
\alias{source_reactive}
\title{A reactive source.}
\usage{
  source_reactive(data, name = NULL)
}
\arguments{
  \item{data}{A reactive values object or reactive
  expression.}

  \item{name}{A name for the object.}
}
\description{
  A reactive source.
}
\examples{
source_reactive(reactive(mtcars))

# Pipeline automatically puts reactive objects in a source_reactive
pipeline(reactive(mtcars))

# A simple example
v <- reactiveValues(n = 10)
p <- pipeline(reactive(mtcars[1:v$n, ]))
props <- props(x ~ wt, y ~ mpg)

sluice(p, props)

v$n <- 5
sluice(p, props)
}

