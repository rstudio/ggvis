\name{transform_smooth}
\alias{branch_smooth}
\alias{transform_smooth}
\title{Transformation: smooth the data}
\usage{
  transform_smooth(method = guess(), formula = guess(),
    se = TRUE, level = 0.95, n = 80L, na.rm = FALSE, ...)

  branch_smooth(props = NULL, ...)
}
\arguments{
  \item{method}{Model fitting function to use - it must
  support R's standard modelling interface, taking a
  formula and data frame as input, and returning
  predictions with \code{\link{predict}}. If not supplied,
  will use \code{\link{loess}} for <= 1000 points,
  otherwise it will use \code{\link[mgcv]{gam}}. Other
  modelling functions that will work include
  \code{\link{lm}}, \code{\link{glm}} and
  \code{\link[MASS]{rlm}}}

  \item{formula}{Formula passed to modelling function. Can
  only use \code{y} and \code{x} variables.  If not
  specified, defaults to \code{y ~ s(x)} for \code{method =
  gam}, \code{y ~ x} otherwise.}

  \item{se}{include standard errors in output? Requires
  approprate method of \code{predictdf}, since the
  interface for returning predictions with standard errors
  is not consistent acrossing modelling frameworks.}

  \item{level}{the confidence level of the standard
  errors.}

  \item{n}{the number of grid points to use in the
  prediction}

  \item{na.rm}{If \code{TRUE} missing values will be
  silently removed, otherwise they will be removed with a
  warning.}

  \item{...}{other arguments passed on to the underlying
  transform function}

  \item{props}{a \code{\link{props}} object describing a
  property mapping}
}
\description{
  \code{transform_smooth} is a data transformation that can
  be passed into a node. \code{branch_smooth} creates a
  node that transforms the data and then displays it with
  \code{\link{mark_line}}.
}
\section{Ouput}{
  \code{transform_smooth} creates a data frame with
  columns:

  \itemize{ \item \code{x} \item \code{y} }

  If standard errors are requested, it will also contain:

  \itemize{ \item \code{y_lower} \item \code{y_upper} \item
  \code{se} }
}
\examples{
transform_smooth()
sluice(transform_smooth(), props(x ~ mpg, y ~ disp), mtcars)
# You can see the results of a transformation by creating your own pipeline
# and flowing data through it
sluice(transform_smooth(n = 5L), props(x ~ disp, y ~ mpg), mtcars)
# Or
pl <- pipeline(
  mtcars,
  by_group(quote(cyl)),
  transform_smooth(n = 5L, method = "lm")
)
sluice(pl, props(x ~ disp, y ~ mpg))
}

