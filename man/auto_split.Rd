\name{auto_split}
\alias{auto_split}
\title{Automatically split data by groups}
\usage{
auto_split()
}
\description{
  Use \code{auto_split} to split up a dataset on all
  categorical variables specified by props, and have each
  piece rendered by the same mark.
}
\examples{
# One line
ggvis(mtcars, props(x = ~disp, y = ~mpg), mark_line())
# One line for each level of cyl
ggvis(mtcars, auto_split(cyl), props(x = ~disp, y = ~mpg), mark_line())

# This shows the data generated using by_group
sluice(pipeline(mtcars, by_group(cyl)), props(x = ~disp, y = ~mpg))
# Note that the props aren't used for splitting, but sluice() needs
# props to be present to work.
}
\seealso{
  To manually specify grouping variables, see
  \code{\link{by_group}}.
}

