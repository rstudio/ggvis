\name{by_group}
\alias{by_group}
\title{Split data by group}
\usage{
  by_group(variables)
}
\arguments{
  \item{variables}{A character vector of variable names to
  split on.}
}
\description{
  Split data by group
}
\examples{
pl <- pipeline(mtcars, by_group("cyl"), transform_bin())
flow(pl, props(x ~ disp))
}

