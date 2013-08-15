\name{gigvis}
\alias{gigvis}
\alias{node}
\title{Create a gigvis object, or a node in a gigvis plot.}
\usage{
  gigvis(data = NULL, props = NULL, ...)

  node(..., data = NULL, props = NULL)
}
\arguments{
  \item{data}{A data \code{\link{pipeline}}, or anything
  that can be coerced to a pipeline using
  \code{\link{as.pipeline}}. Typically this will be a data
  frame, but some transforms will accept other types of
  objects, and you can also supply a
  \code{\link[shiny]{reactive}} whose value changes over
  time.}

  \item{props}{a list of \code{\link{props}} defining
  default mark properties for this node and all its
  children.}

  \item{...}{components: \code{node}s, \code{\link{marks}},
  \code{\link{scales}}, \code{\link{axis}} or
  \code{\link{legend}} objects.  A node can only contain
  other nodes and marks.}
}
\value{
  a \code{gigvis_node} object. Will display the plot when
  printed; see \code{\link{save_spec}},
  \code{\link{view_static}} and \code{\link{view_dynamic}}
  for other options.
}
\description{
  Create a gigvis object, or a node in a gigvis plot.
}
\section{Hierarchy}{
  A gigvis plot has a hierarchical structure, where each
  node inherits data and properties from its parent. This
  is somewhat similar to ggplot2, but ggplot2 plots only
  had a single layer of hierarchy - with gigvis, you can
  have multiple levels, making it easier to avoid
  redundancy, both in your specification and in
  computation.

  For example, take a linear model. You often want to
  display both the predictions and the standard error from
  a linear model. In ggplot2, you had to use
  \code{geom_smooth()}, which was a special geom that
  combined a line and a ribbon. With gigvis, you can do it
  yourself by using two marks nested inside a node: (and in
  fact, this is exactly how \code{\link{branch_smooth}})
  works.

  \code{ gigvis(mtcars, props(x ~ disp, y ~ mpg), node(data
  = transform_smooth(), mark_area(props(y ~ y_min, y2 ~
  y_max, fill = "#eee")), mark_line() ), mark_symbol() ) }
}

