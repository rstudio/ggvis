\name{transform_density}
\alias{branch_density}
\alias{transform_density}
\title{Transformation: density estimate}
\usage{
  transform_density(..., adjust = 1, kernel = "gaussian",
    trim = FALSE, n = 200L, na.rm = FALSE)

  branch_density(..., area = TRUE)
}
\arguments{
  \item{adjust}{Bandwidth adjustment. See
  \code{\link{density}} for details.}

  \item{kernel}{Smoothing kernel. See \code{\link{density}}
  for details.}

  \item{trim}{If \code{TRUE}, the default, density
  estimates are trimmed to the actual range of the data.
  If \code{FALSE}, they are extended by the default 3
  bandwidths (as specified by the \code{cut} parameter to
  \code{\link{density}}).}

  \item{n}{Number of points (along x) to use in the density
  estimate.}

  \item{na.rm}{If \code{TRUE} missing values will be
  silently removed, otherwise they will be removed with a
  warning.}

  \item{area}{Should there be a shaded region drawn under
  the curve?}

  \item{...}{Named arguments are passed on to the
  transform, unnamed arguments are passed on to the
  branch.}
}
\description{
  \code{transform_density} is a data transformation that
  computes a kernel density estimate from a dataset.
  \code{branch_density} combines \code{transform_density}
  with \code{mark_line} and \code{mark_area} to display a
  smooth line and its standard errror.
}
\section{Input}{
  Currently \code{transform_density} only works with data
  frames and requires the following properties:

  \itemize{ \item \code{x}, numeric, horizontal position }
}

\section{Ouput}{
  \code{transform_density} creates a data frame with
  columns:

  \itemize{ \item \code{x}: regularly spaced grid of
  \code{n} locations \item \code{y}: density estimate }
}
\examples{
# Basic density estimate
ggvis(faithful, props(x = ~waiting), branch_density())

# Smaller bandwidth
ggvis(faithful, props(x = ~waiting, fill :="lightblue"),
  branch_density(adjust = .25)
)

# Control stroke and fill
ggvis(faithful, props(x = ~waiting),
  branch_density(props(stroke := "#cc3333", strokeWidth := 3,
    fill := "#666699", fillOpacity := 0.6))
)

# With groups
ggvis(PlantGrowth, by_group(group),
  props(x = ~weight, stroke = ~group, fill = ~group, fillOpacity := 0.2),
  branch_density()
)

# Using various arguments: adjust na.rm, n, area, kernel
mtc <- mtcars
mtc$mpg[5:10] <- NA
ggvis(mtc,
  props(x = ~mpg, y = ~mpg),
  branch_density(adjust = 0.3, n = 100, area = FALSE, kernel = "rectangular",
    props(stroke := "#cc0000"))
)
}

