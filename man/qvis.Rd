\name{qvis}
\alias{qvis}
\title{Quick vis: a succint way of generating simple ggvis plot specifications.}
\usage{
qvis(data, ..., layers = character())
}
\arguments{
  \item{data}{The data set where}

  \item{...}{Properties and layer arguments.

  If the name of the argument matches the name of a known
  property, it will be added to the plot properties.
  Otherwise it will be passed as an argument to every
  layer. If you want to pass different arguments to
  different layers, you'll need to add layers on one at a
  time.

  The first two unnamed components are taken to be \code{x}
  and \code{y}.  Any additional unnamed components will
  raise an error.}

  \item{layers}{A character vector listing the names of
  marks or layers to display on the plot. You can use
  either the full name of the layer (e.g. "mark_line" or
  "layer_smooth"), or just the final part (e.g. "line" or
  "smooth"). If there is both a plain mark and a layer with
  the same name, the layer will be used.

  If \code{layers} is not supplied, it defaults to
  \code{"\link{mark_point}"} if both \code{x} and \code{y}
  are supplied.  If only \code{x} is supplied, it defaults
  to \code{"\link{layer_histogram}"}.}
}
\description{
\code{qvis} is a succinct way of creating simple plots. It
achieves this succinctness by being a little bit magical
and trying to guess what you mean. It is most suitable for
creating basic data graphics like scatterplots and
histograms. As you start creating more complicated plots,
you'll want to use \code{\link{ggvis}} directly to get more
control and avoid ambiguity.
}
\section{Compared to \code{qplot}}{
  \code{qvis} is somewhat less powerful than \code{qplot}.
  \code{qvis} contains the most commonly used arguments
  from \code{qplot}, but it drops the many extra arguments
  that are easy to replicate by adding the right component
  on to the plot.
}
\examples{
# A basic scatterplot
qvis(mtcars, ~mpg, ~wt)
qvis(mtcars, ~mpg, ~wt, fill = ~cyl)
qvis(mtcars, ~mpg, ~wt, fill := "red")

# Basic histogram
qvis(mtcars, ~mpg)
qvis(mtcars, ~mpg, binwidth = 2)

# Scatterplot + smoother
qvis(mtcars, ~mpg, ~wt, layers = c("point", "smooth"))
qvis(mtcars, ~mpg, ~wt, layers = c("point", "smooth"), span = 0.25)

# It's not currently possible to create a plot of variables
# stored only in the local environment
x <- runif(10)
y <- runif(10)
\dontrun{qvis(environment(), ~x, ~y)}
}

