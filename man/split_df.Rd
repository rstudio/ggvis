\name{split_df}
\alias{is.split_df}
\alias{split_df}
\title{Create a data frame split up by specified variables}
\usage{
  split_df(data, split, env = NULL)

  is.split_df(x)
}
\arguments{
  \item{data}{A data frame.}

  \item{split}{A quoted expression which, when evaluated
  with \code{data} (and optionally \code{env}), specifies
  the split groups.}

  \item{env}{An enclosing environment in which to evaluate
  \code{split}.}

  \item{x}{object to test for split_df-ness}
}
\description{
  Create a data frame split up by specified variables
}
\keyword{internal}

