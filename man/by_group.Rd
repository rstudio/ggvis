\name{by_group}
\alias{by_group}
\title{Split data by group}
\usage{
  by_group(..., env = parent.frame())
}
\arguments{
  \item{...}{Variables to split on.  These are coerced to
  variable \code{\link{prop}} objects with \code{as.prop}.}
}
\description{
  Split data by group
}
\examples{
by_group("cyl")
by_group(quote(cyl))

pl <- pipeline(mtcars, by_group("cyl"), transform_bin())
sluice(pl, props(x ~ disp))
}

