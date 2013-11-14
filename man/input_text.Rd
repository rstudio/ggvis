\name{input_text}
\alias{input_numeric}
\alias{input_text}
\title{Create an interactive text or numeric input box.}
\usage{
input_text(value, label = "", id = rand_id("text_"), map = identity)

input_numeric(value, label = "", id = rand_id("text_"), map = identity)
}
\arguments{
  \item{value}{Initial value}

  \item{label}{Display label for the control}

  \item{id}{a unique identifier for this interactive input
  - used to de-duplicate the controls when the same input
  is used in multiple places in a visualisation}

  \item{map}{a function with a singe argument that takes
  the value returned from the input control and converts it
  to an argument useful for ggvis.  Defaults to
  \code{identity}, leaving the output unchanged.}
}
\description{
  \code{input_numeric} only allows numbers and comes with a
  spin box control. \code{input_text} allows any type of
  input.
}
\examples{
fill_text <- input_text(label = "Point color", value = "red")
ggvis(mtcars, props(x = ~wt, y = ~mpg),
  mark_symbol(props(fill := fill_text)))

size_num <- input_numeric(label = "Point size", value = 25)
ggvis(mtcars, props(x = ~wt, y = ~mpg),
  mark_symbol(props(size := size_num)))
}
\seealso{
  Other interactive input: \code{\link{input_checkbox}},
  \code{\link{input_checkboxgroup}},
  \code{\link{input_radiobuttons}},
  \code{\link{input_select}}, \code{\link{input_slider}}
}

