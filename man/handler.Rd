\name{handler}
\alias{handler}
\alias{is.handler}
\title{Create handler S3 class.}
\usage{
handler(subclass, listener, control_args = list(), value = NULL,
  map = identity, id = rand_id())

is.handler(x)
}
\description{
This is currently a subclass on input, but it should
probably be the other way around since inputs are handlers
that have controls.
}
\examples{
p <- ggvis(mtcars, props(x = ~mpg, y = ~wt, size = left_right(1, 100)),
  mark_symbol())
p$props$size.update$dr
handlers(p)
}

