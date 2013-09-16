\name{connect}
\alias{connect}
\alias{sluice}
\title{Connect a pipeline, creating a reactive expression.}
\usage{
  connect(x, props, source = NULL, session = NULL)

  sluice(x, props, source = NULL, session = NULL)
}
\arguments{
  \item{x}{a pipeline or pipe}

  \item{properties}{a \code{\link{props}} object}

  \item{source}{the data source to start the pipeline. This
  is used when the output from one pipeline into the input
  of another. Methods must check if is a reactive object
  and deal with appropriately.}

  \item{session}{the session object, for if this is called
  within a Shiny application.}
}
\description{
  \code{connect} creates a reactive pipeline (starting with
  \code{NULL}), applying each pipe (transformation) in
  sequence. \code{sluice} is a convenient helper method
  that creates the pipeline, then flows the data along it
  to render a final result.
}
\details{
  Every element in a pipeline recieves the same set of
  properties: this generally means that for more
  complicated transformations you will need to create
  multiple branches.
}
\examples{
df <- data.frame(x = 1:10, y = 1:10)
asis <- props(x = ~x, y = ~y)

sluice(transform_scale(), asis, df)
sluice(transform_scale(mult = -1), asis, df)
sluice(transform_scale(add = 10), asis, df)

# Length three pipeline
p <- pipeline(df, transform_scale(mult = -1), transform_scale(mult = -1))
sluice(p, asis)
p <- pipeline(df, transform_scale(mult = 2), transform_scale(mult = 2))
sluice(p, asis)

# Reactive pipeline
library(shiny)
v <- reactiveValues(add = 0, mult = 1)
ts <- transform_scale(add = reactive(v$add), mult = reactive(v$mult))
r <- connect(ts, asis, df)
isolate(r())
v$add <- 2
isolate(r())

# Reactive pipeline with reactive data
v <- reactiveValues(add = 0, mult = 1, n = 5)
p <- pipeline(
   reactive(df[1:v$n, , ]),
   transform_scale(add = reactive(v$add), mult = reactive(v$mult))
)
r <- connect(p, asis)
isolate(r())
v$add <- 2
v$n <- 10
isolate(r())
}
\keyword{internal}

