\name{datasource}
\alias{datasource}
\title{Create a new data source.}
\usage{
  datasource(data, name = deparse(substitute(data)))
}
\arguments{
  \item{data}{a data frame}

  \item{name}{the name of the data frame (used in error
  messages etc.)}
}
\description{
  Create a new data source.
}
\examples{
datasource(mtcars)

# A simple example of a reactive data source
v <- reactiveValues(n = 10)
p <- pipeline(reactive(mtcars[1:v$n, ]))
props <- props(x ~ wt, y ~ mpg)

sluice(p, props)

v$n <- 5
sluice(p, props)
}

