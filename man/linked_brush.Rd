\name{linked_brush}
\alias{linked_brush}
\title{Create a lnked brush object}
\usage{
linked_brush(keys, fill = "red")
}
\description{
Create a lnked brush object
}
\examples{
lb <- linked_brush(keys = 1:nrow(mtcars))
qvis(mtcars, ~disp, ~mpg, fill := lb$fill_prop(), size.brush := 400) + lb$brush_handler()

ggvis(mtcars, props(x = ~disp, y = ~mpg, fill := lb$fill_prop())) +
  mark_point() +
  lb$brush_handler()
}

