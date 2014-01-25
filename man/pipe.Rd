\name{pipe}
\alias{as.pipe}
\alias{is.pipe}
\alias{pipe}
\title{Create new "pipe" object.}
\usage{
pipe(type, ...)

is.pipe(x)

as.pipe(x, ...)
}
\description{
A pipe object represents a component in a
\code{\link{pipeline}}. Pipes provide a declarative
specification of interactive behaviour, and define the
behaviour of each component in the data hierarchy.
}
\details{
This function is designed to be used by authors of new
types of pipes. If you are a ggvis user, please use an
existing pipe: a data frame, a transform, a mark, or a
layer
}
\keyword{internal}

