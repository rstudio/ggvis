\name{transform_bin}
\alias{branch_freqpoly}
\alias{branch_histogram}
\alias{transform_bin}
\title{Transformation: bin continuous variable.}
\usage{
transform_bin(..., binwidth = guess(), origin = NULL, right = TRUE)

branch_histogram(...)

branch_freqpoly(...)
}
\arguments{
  \item{binwidth}{The width of the bins. The default is
  \code{guess()}, which yields 30 bins that cover the range
  of the data. You should always override this value,
  exploring multiple widths to find the best to illustrate
  the stories in your data.}

  \item{origin}{The initial position of the left-most bin.
  If \code{NULL}, the the default, will use the smallest
  value in the dataset.}

  \item{right}{Should bins be right-open, left-closed, or
  right-closed, left-open}

  \item{...}{For \code{transform_bin}: ignored, all
  transforms must use named arguments.  For
  \code{branch_histogram}: named arguments are passed on to
  the transform, unnamed arguments are passed on to the
  branch.}
}
\description{
\code{transform_bin} is a data transformation that reduces
a one-d vector of positions to a data frame of binned
counts. \code{branch_histogram} combines
\code{transform_bin} with \code{mark_rect} to create a
histogram, and \code{branch_freqpoly} combines
\code{transform_bin} with \code{mark_line} to create a
frequency polygon.
}
\section{Input}{
  Currently \code{transform_bin} only works with data
  frames and requires the following properties:

  \itemize{ \item \code{x}, numeric, the values to bin and
  count }
}

\section{Ouput}{
  \code{transform_bin} creates a data frame with columns:

  \itemize{ \item \code{count__}: the number of points
  \item \code{x}: mid-point of bin \item \code{xmin__}:
  left boundary of bin \item \code{xmax__}: right boundary
  of bin \item \code{width__}: width of bin }
}
\examples{
# Create histograms and frequency polygons with branches
ggvis(mtcars, props(x = ~mpg), branch_histogram())
ggvis(mtcars, props(x = ~mpg), branch_histogram(binwidth = 2))
ggvis(mtcars, props(x = ~mpg), branch_freqpoly(binwidth = 2))

# These are equivalent to combining transform_bin with the corresponding
# mark
ggvis(mtcars, props(x = ~mpg), transform_bin(binwidth = 2),
  mark_rect(props(x = ~xmin__, x2 = ~xmax__, y = ~count__, y2 = 0)),
  mark_line(props(x = ~x, y = ~count__, stroke := "red", strokeWidth := 4))
)

# You can also combine other data transformations like splitting
ggvis(mtcars, props(x = ~mpg, stroke = ~cyl, strokeWidth = 4),
   by_group(cyl), branch_freqpoly(binwidth = 2))

# You can see the results of a transformation by creating your own pipeline
# and flowing data through it
sluice(pipeline(mtcars, transform_bin(2)), props(x = ~mpg))
sluice(pipeline(mtcars, by_group(cyl), transform_bin(2)), props(x = ~disp))
# Or
pl <- pipeline(mtcars, transform_bin(10))
sluice(pl, props(x = ~disp))
}

