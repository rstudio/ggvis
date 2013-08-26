\name{mark}
\alias{is.mark}
\alias{mark}
\title{Create a new mark object.}
\usage{
  mark(type, props, data = NULL)

  is.mark(x)
}
\arguments{
  \item{type}{vega mark list}

  \item{props}{list of properties}

  \item{data}{optional data pipeline}
}
\description{
  A mark object is a close mapping to a vega mark object.
  Vega marks are documented on
  \url{https://github.com/trifacta/vega/wiki/Marks}.
}
\details{
  This object is used by all \code{\link{marks}} - you
  should not have to call it directly, unless you are
  creating a new R mark object to represent a vega mark
  that is not currently available in ggvis.
}
\keyword{internal}

