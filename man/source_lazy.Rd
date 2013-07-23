\name{source_lazy}
\alias{source_lazy}
\title{A lazy source.}
\usage{
  source_lazy(name)
}
\arguments{
  \item{name}{the name of the data frame}
}
\description{
  A lazy source is evaluated when the plot is created. It
  will be evaluated in the global environment, so it is not
  suitable to use when creating plot components inside a
  function, but since it only needs to store the name of
  the object, not its conents, it makes a much lighter
  weight gigvis object.
}
\examples{
p <- pipeline(source_lazy("mtcars"))
props <- props(x ~ wt, y ~ mpg)

sluice(p, props)
}

