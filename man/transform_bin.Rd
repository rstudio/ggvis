\name{transform_bin}
\alias{transform_bin}
\title{Transformation: bin continuous variable.}
\usage{
  transform_bin(binwidth = guess(), origin = NULL,
    right = TRUE)
}
\arguments{
  \item{binwidth}{The width of the bins. The default is
  \code{guess()}, which yields 30 bins that cover the range
  of the data.}

  \item{origin}{The initial position of the left-most bin.
  If \code{NULL}, the the default, will using the smallest
  value in the dataset}

  \item{right}{Should bins be right-open, left-closed, or
  right-closed, left-open}
}
\description{
  Transformation: bin continuous variable.
}
\section{Input}{
  The data that \code{transform_bin} is applied to, must
  have methods for \code{prop_type}, \code{prop_range} and
  \code{bin}. Currently, this implies that the input data
  must be a data frame.
}

\section{Properties}{
  It must have an \code{x} property, and that property must
  be numeric.
}

\section{Ouput}{
  \code{transform_bin} creates a data frame with columns:

  \itemize{ \item \code{count__} \item \code{x} \item
  \code{xmin__} \item \code{xmax__} \item \code{width__} }
}
\examples{
transform_bin()
transform_bin(binwidth = 10, origin = 1)
transform_bin(right = FALSE)

# You can see the results of a transformation by creating your own pipeline
# and flowing data through it
sluice(transform_bin(10), props(x ~ disp), mtcars)
sluice(pipeline(mtcars, by_group(variable(quote(cyl))), transform_bin(10)), props(x ~ disp))
# Or
pl <- pipeline("mtcars", transform_bin(10))
sluice(pl, props(x ~ disp))
}

