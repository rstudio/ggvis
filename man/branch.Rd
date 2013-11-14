\name{branch}
\alias{branch}
\alias{is.branch}
\title{Create a new branch.}
\usage{
branch(..., drop_named = FALSE)

is.branch(x)
}
\arguments{
  \item{...}{components: data, \code{\link{props}},
  \code{branch}es, or \code{\link{marks}}}

  \item{drop_unnamed}{if \code{FALSE}, the default, will
  throw an error if any of the arguments in \code{...} are
  named. If \code{TRUE} it will silently drop them - this
  is primarily useful for \code{branch_} functions which
  send named arguments to the transform, and unnamed
  arguments to the branch.}

  \item{x}{object to test for "branch"-ness}
}
\description{
  Branches are used to describe the data hierarchy of
  ggvis. As well as using this function to create them, you
  can also use the many specialised \code{branch_}
  functions that combine marks and transforms to create
  useful visualisations.
}
\section{Hierarchy}{
  A ggvis plot has a hierarchical structure, where each
  branch inherits data and properties from its parent. This
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
  yourself by using two marks nested inside a branch: (and
  in fact, this is exactly how \code{\link{branch_smooth}})
  works.

  \code{ ggvis(mtcars, props(x = ~disp, y = ~mpg),
  branch(transform_smooth(), mark_area(props(y = ~y_min, y2
  = ~y_max, fill := "#eee")), mark_line() ), mark_symbol()
  ) }
}

