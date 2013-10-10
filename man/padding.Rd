\name{padding}
\alias{padding}
\title{Define padding.}
\usage{
  padding(top = NULL, right = NULL, bottom = NULL,
    left = NULL)
}
\arguments{
  \item{top,right,bottom,left}{Amount of padding on each
  border. Can either be a single number, "auto", or
  "strict"}
}
\description{
  Define padding.
}
\examples{
opts(padding = padding())
opts(padding = padding(10, 10, 10, 10))
}

