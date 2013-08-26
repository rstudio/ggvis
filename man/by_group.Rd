\name{by_group}
\alias{by_group}
\title{Visualise data by group.}
\usage{
  by_group(..., env = parent.frame())
}
\arguments{
  \item{...}{Variables to split on. These can either be
  strings (for variable names) or quoted expressions (for
  more complicated transformations)}
}
\description{
  Use \code{by_group} to split up a dataset into multiple
  pieces, and have each piece rendered by the same mark.
}
\details{
  The interface for this function is highly likely to
  change prior to the initial release of ggvis.
}
\examples{
by_group("cyl")
by_group(quote(cyl))
by_group(quote(vs + am))

# One line
ggvis(mtcars, props(x ~ disp, y ~ mpg), mark_line())
# One line for each level of cyl
ggvis(mtcars, by_group("cyl"), props(x ~ disp, y ~ mpg), mark_line())
}

