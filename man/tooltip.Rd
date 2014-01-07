\name{tooltip}
\alias{tooltip}
\title{Display tooltips.}
\usage{
tooltip(f)
}
\arguments{
  \item{f}{A function that takes a single argument as
  input. This argument will be a list containing the data
  in the mark currently under the mouse. It should return a
  string containing HTML.}
}
\description{
Display tooltips.
}
\examples{
all_values <- function(x) {
  paste0(names(x), ": ", format(x), collapse = "<br />")
}

ggvis(mtcars, props(x = ~wt, y = ~mpg), mark_symbol(), tooltip(all_values))
}

