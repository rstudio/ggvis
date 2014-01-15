\name{tooltip}
\alias{click_tooltip}
\alias{tooltip}
\title{Display tooltips}
\usage{
tooltip(f)

click_tooltip(f)
}
\arguments{
  \item{f}{A function that takes a single argument as
  input. This argument will be a list containing the data
  in the mark currently under the mouse. It should return a
  string containing HTML.}
}
\description{
Display tooltips
}
\examples{
\dontrun{
all_values <- function(x) {
  if(is.null(x)) return(NULL)
  paste0(names(x), ": ", format(x), collapse = "<br />")
}

# Display tooltip when hovering over objects
ggvis(mtcars, props(x = ~wt, y = ~mpg)) +
  mark_symbol() +
  tooltip(all_values)

# Display tooltip when objects are clicked
ggvis(mtcars, props(x = ~wt, y = ~mpg)) +
  mark_symbol() +
  click_tooltip(all_values)

}
}

