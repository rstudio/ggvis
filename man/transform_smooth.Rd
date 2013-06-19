\name{transform_smooth}
\alias{transform_smooth}
\title{Transformation: smooth the data}
\usage{
  transform_smooth(method = guess(), formula = guess(),
    se = TRUE, level = 0.95, n = 80L, na.rm = FALSE, ...)
}
\description{
  Transformation: smooth the data
}
\examples{
transform_smooth()
flow(transform_smooth(), props(x ~ mpg, y ~ disp), mtcars)
# You can see the results of a transformation by creating your own pipeline
# and flowing data through it
flow(transform_smooth(n = 5L), props(x ~ disp, y ~ mpg), mtcars)
# Or
pl <- pipeline("mtcars", by_group("cyl"), transform_smooth(n = 5L))
flow(pl, props(x ~ disp, y ~ mpg))
}

