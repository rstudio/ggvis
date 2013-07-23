\name{by_group}
\alias{by_group}
\title{Split data by group}
\usage{
  by_group(...)
}
\arguments{
  \item{...}{Variables to split on.  These are coerced to
  \code{\link{variable}} objects with \code{as.variable}.}
}
\description{
  Split data by group
}
\examples{
by_group("cyl")
by_group(quote(cyl))
by_group(variable(quote(cyl)))

pl <- pipeline(mtcars, by_group("cyl"), transform_bin())
sluice(pl, props(x ~ disp))
}

