\name{input_text}
\alias{input_text}
\title{Create a placeholder for a text input.}
\usage{
  input_text(value, label = "", id = rand_id("text_"),
    map = identity, scale = FALSE)
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

  \item{scale}{default scale value when this input is used
  in a prop}
}
\description{
  Create a placeholder for a text input.
}
\examples{
input_text(label = "Point color", value = "red")

ggvis(mtcars, props(x ~ wt, y ~ mpg),
  mark_symbol(
    props(fill = input_text(label = "Point color", value = "red"))
  )
)
}
\seealso{
  Other interactive input: \code{\link{input_checkbox}},
  \code{\link{input_checkboxgroup}},
  \code{\link{input_numeric}},
  \code{\link{input_radiobuttons}},
  \code{\link{input_select}}, \code{\link{input_slider}}
}

