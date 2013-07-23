\name{by_group}
\alias{by_group}
\title{Split data by group}
\usage{
  by_group(...)
}
\arguments{
  \item{...}{Variables to split on.}
}
\description{
  Split data by group
}
\examples{
pl <- pipeline(mtcars, by_group(variable(quote(cyl))), transform_bin())
sluice(pl, props(x ~ disp))
}

