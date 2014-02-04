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
# Make cyl a factor (as it really should be)
mtcars2 <- mtcars
mtcars2$cyl <- factor(mtcars2$cyl)

# One line
ggvis(mtcars2, props(x = ~disp, y = ~mpg, stroke = ~cyl)) +
  mark_path()
# One line for each level of cyl
ggvis(mtcars2, auto_split(), props(x = ~disp, y = ~mpg, stroke = ~cyl)) +
  mark_path()

# This shows the data generated using by_group
sluice(pipeline(mtcars, by_group(cyl)), props(x = ~disp, y = ~mpg))
# Note that the props aren't used for splitting, but sluice() needs
# props to be present to work.
}
\seealso{
To manually specify grouping variables, see
\code{\link{by_group}}.
}

