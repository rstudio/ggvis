\name{branch_line}
\alias{branch_line}
\title{A branch to add a line.}
\usage{
branch_line(..., sort = TRUE)
}
\arguments{
  \item{props}{An optional props object which will be
  passed to \code{\link{mark_line}}.}

  \item{sort}{Should the data be sorted? If \code{TRUE}
  (the default), a \code{\link{transform_sort}} will be
  used. By default, this will sort the data on the x
  variable.}

  \item{...}{Named arguments are passed on to
  \code{\link{transform_sort}}; unnamed arguments are not
  used.}
}
\description{
A branch to add a line.
}
\examples{
ggvis(mtcars,
  props(x = ~wt, y = ~mpg, stroke = ~factor(cyl)),
  branch_line(),
  mark_symbol()
)
}
\seealso{
\code{link{mark_line}}
}

