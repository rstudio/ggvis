\name{transform_scale}
\alias{transform_scale}
\title{Transform scale: a simple transform for testing}
\usage{
  transform_scale(add = 0, mult = 1)
}
\arguments{
  \item{add}{constant to add to x}

  \item{mult}{constant to multiply x by}
}
\description{
  Transform scale: a simple transform for testing
}
\examples{
neg <- transform_scale(mult = -1)
neg
compute(neg, props(x = ~ mpg, y = ~ disp), mtcars)
}
\keyword{internal}

