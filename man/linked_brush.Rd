\name{linked_brush}
\alias{linked_brush}
\usage{
linked_brush(keys, fill = "red")
}
\examples{
lb <- linked_brush(keys = 1:nrow(mtcars))

qvis(mtcars, ~disp, ~mpg, fill := lb$fill_prop()) + lb$brush_handler()

ggvis(mtcars, props(x = ~disp, y = ~mpg, fill := )) + mark_point() +
  lb$brush_handler()
}

