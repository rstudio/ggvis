\name{transform_stack}
\alias{transform_stack}
\title{Transformation: stack the data}
\usage{
  transform_stack(direction = "y")
}
\arguments{
  \item{direction}{The direction to stack. This is a
  variable name after mapping, and must be either
  \code{"y"} (the default) or \code{"x"}.  For example,
  with \code{props(y = ~mpg)}, you would use \code{"y"},
  not \code{"mpg"}.}
}
\description{
  \code{transform_stack} is a data transformation that
  stacks values in a a data object. Typically, y values are
  stacked at each unique x value, although it's also
  possible to stack x values at each unique y.
}
\section{Input}{
  \code{transform_stack} takes a data frame or a split_df
  as input.
}

\section{Ouput}{
  \code{transform_stack} returns a sorted data frame or
  split_df with the same columns as the input as well as
  columns named \code{ymin__} and \code{ymax__} (or
  \code{xmin__} and \code{xmax__} if stacking x values).
  These columns specify the upper and lower bounds of each
  stacked object.

  Note that \code{transform_stack} does not sort the
  values. If you want to sort on another variable, you can
  use \code{\link{transform_sort}} before stacking. This is
  useful when, for example, your data is unsorted and you
  want stacked bar chart where each stack of bars appears
  in the same order.  Also, if you use \code{by_group}, it
  will result in the data being sorted by the grouping
  variables.
}
\examples{
library(plyr)
hec <- as.data.frame(HairEyeColor)
# Collapse across Sex
hec <- ddply(hec, c("Hair", "Eye"), summarise, Freq = sum(Freq))

# Without stacking - bars overlap
ggvis(hec,
  props(x = ~Hair, y = ~Freq, fill = ~Eye, fillOpacity := 0.5),
  dscale("x", "nominal", range = "width", padding = 0, points = FALSE),
  mark_rect(props(y2 = 0, width = band()))
)

# With stacking
ggvis(hec, transform_stack(),
  props(x = ~Hair, y = ~Freq, fill = ~Eye, fillOpacity := 0.5),
  dscale("x", "nominal", range = "width", padding = 0, points = FALSE),
  mark_rect(props(y = ~ymin__, y2 = ~ymax__, width = band()))
)

# Stacking in x direction instead of default y
ggvis(hec, transform_stack(direction = "x"),
  props(x = ~Freq, y = ~Hair, fill = ~Eye, fillOpacity := 0.5),
  dscale("y", "nominal", range = "height", padding = 0, points = FALSE),
  mark_rect(props(x = ~xmin__, x2 = ~xmax__, height = band()))
)


# Stack y values at each x
sluice(pipeline(hec, transform_stack()), props(x = ~Hair, y = ~Freq))

# Same effect, but this time stack x values at each y
sluice(pipeline(hec, transform_stack(direction = "x")),
  props(x = ~Freq, y = ~Hair))
}

