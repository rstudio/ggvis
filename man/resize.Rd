\name{resize}
\alias{resize}
\title{An interactive input bound to resize events.}
\usage{
resize(f)
}
\arguments{
  \item{f}{A function which is called each time the plot
  area is resized.}
}
\description{
An interactive input bound to resize events.
}
\examples{
\dontrun{}
# This example just prints out the current dimensions to the console
print_info <- function(x) {
  cat(str(x))
}

ggvis(mtcars, props(x = ~mpg, y = ~wt)) +
  mark_symbol() +
  resize(print_info)
}
}

