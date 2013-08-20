\name{ggvis}
\alias{ggvis}
\alias{node}
\title{Create a ggvis object, or a node in a ggvis plot.}
\usage{
  ggvis(...)

  node(...)
}
\arguments{
  \item{...}{components: data, \code{\link{props}},
  \code{node}s, \code{\link{marks}}, \code{\link{scales}},
  \code{\link{axis}} or \code{\link{legend}} objects. A
  node can only contain other nodes and marks.}
}
\value{
  a \code{ggvis_node} object. Will display the plot when
  printed; see \code{\link{save_spec}},
  \code{\link{view_static}} and \code{\link{view_dynamic}}
  for other options.
}
\description{
  Create a ggvis object, or a node in a ggvis plot.
}
\section{Hierarchy}{
  A ggvis plot has a hierarchical structure, where each
  node inherits data and properties from its parent. This
  is somewhat similar to ggplot2, but ggplot2 plots only
  had a single layer of hierarchy - with ggvis, you can
  have multiple levels, making it easier to avoid
  redundancy, both in your specification and in
  computation.

  For example, take a linear model. You often want to
  display both the predictions and the standard error from
  a linear model. In ggplot2, you had to use
  \code{geom_smooth()}, which was a special geom that
  combined a line and a ribbon. With ggvis, you can do it
  yourself by using two marks nested inside a node: (and in
  fact, this is exactly how \code{\link{branch_smooth}})
  works.

  \code{ ggvis(mtcars, props(x ~ disp, y ~ mpg), node(data
  = transform_smooth(), mark_area(props(y ~ y_min, y2 ~
  y_max, fill = "#eee")), mark_line() ), mark_symbol() ) }
}

