\name{resize}
\alias{resize}
\usage{
resize(f)
}
\examples{
ggvis(mtcars, props(x = ~mpg, y = ~wt)) +
  mark_symbol() +
  resize()
}

