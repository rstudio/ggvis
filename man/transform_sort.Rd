\name{transform_sort}
\alias{transform_sort}
\title{Transformation: sort the data}
\usage{
  transform_sort(..., var = "x")
}
\arguments{
  \item{var}{The variables to sort on. This is the variable
  name after mapping.  For example, with \code{props(x = ~
  mpg)}, you would use \code{"x"}, not \code{"mpg"}.
  Multiple variables can be used, as in \code{c("x",
  "y")}.}

  \item{...}{Named arguments for sort function. Unnamed
  arguments will be dropped.}
}
\description{
  Transformation: sort the data
}
\examples{
# Sort on mpg column
sluice(pipeline(mtcars, transform_sort()), props(x = ~ mpg))
# Same effect, but this time mpg is mapped to y
sluice(pipeline(mtcars, transform_sort(var = "y")), props(y = ~ mpg))

# Sort on multiple columns
sluice(pipeline(mtcars, transform_sort(var = c("x", "y"))),
  props(x = ~ cyl, y = ~ mpg))
}

