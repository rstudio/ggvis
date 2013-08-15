\name{last_vis}
\alias{last_vis}
\title{Retrieve the last visualisation created by ggvis.}
\usage{
  last_vis()
}
\description{
  Visualisations are saved whenever they are created or
  plotted.
}
\examples{
ggvis(mtcars, props(x ~ mpg, y ~ cyl), mark_symbol())
str(last_vis())
}

