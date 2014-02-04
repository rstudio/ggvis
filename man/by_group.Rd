\name{by_group}
\alias{by_group}
\title{Visualise data by group.}
\usage{
by_group(..., .vars = list(), .env = parent.frame())
}
\arguments{
  \item{...}{Unquoted variable names or expressions
  describe how to split up the dataset.}

  \item{.vars}{a list of quoted expressions.}

  \item{.env}{environment in which to evalute expressions.
  In ordinary use, the default is adequate.}
}
\description{
Use \code{by_group} to split up a dataset into multiple
pieces, and have each piece rendered by the same mark.
}
\examples{
by_group(cyl)
by_group(vs, am)
by_group(vs + am)

# One line
ggvis(mtcars, props(x = ~disp, y = ~mpg), mark_path())
# One line for each level of cyl
ggvis(mtcars, by_group(cyl), props(x = ~disp, y = ~mpg), mark_path())

# This shows the data generated using by_group
sluice(pipeline(mtcars, by_group(cyl)), props(x = ~disp, y = ~mpg))
# Note that the props aren't used for splitting, but sluice() needs some
# props to be present to work.

# Special evaluation -------------------

# If you have previously quoted variables, use .vars
v <- quote(cyl)
by_group(.vars = list(v))

# If you have the name of a variable as a string, use as.name
var <- "cyl"
by_group(.vars = list(as.name(var)))
}
\seealso{
To automatically detect grouping variables, see
\code{\link{auto_split}}.
}

