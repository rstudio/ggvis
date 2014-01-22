\name{brush_tooltip}
\alias{brush_tooltip}
\alias{click_tooltip}
\alias{tooltip}
\title{Display tooltips}
\usage{
brush_tooltip(f)

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

# Grab the mean and standard deviations of brushed values
brushed_summary <- function(x) {
 if(is.null(x) || length(x) == 0) return(NULL)
 # Get the names of variables other than "key__"
 names <- setdiff(names(x[[1]]), "key__")

 # Print out the mean and sd for each variable
 lines <- lapply(names, function(name) {
   vals <- vapply(x, `[[`, name, FUN.VALUE = numeric(1))
   paste0(name, ": ", round(mean(vals), 2), " (", round(sd(vals), 2), ")")
 })

 paste0(lines, collapse = "<br />")
}

# Display tooltip when objects are brushed
ggvis(mtcars, props(x = ~wt, y = ~mpg)) +
  mark_symbol(props(size.brush := 400)) +
  branch_brush() +
  brush_tooltip(brushed_summary)
}
}

