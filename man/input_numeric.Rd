\name{input_numeric}
\alias{input_numeric}
\title{Create a placeholder for a numeric input.}
\usage{
  input_numeric(value, label = "", id = rand_id("text_"),
    map = identity)
}
\arguments{
  \item{value}{Initial value}

  \item{label}{Display label for the control}
}
\description{
  Create a placeholder for a numeric input.
}
\examples{
input_numeric(label = "Point size", value = 25)

ggvis(mtcars, props(x ~ wt, y ~ mpg),
  mark_symbol(
    props(size = prop(input_numeric(label = "Point size", value = 25))
  )
)
}
\seealso{
  Other interactive input: \code{\link{input_checkbox}},
  \code{\link{input_checkboxgroup}},
  \code{\link{input_radiobuttons}},
  \code{\link{input_select}}, \code{\link{input_slider}},
  \code{\link{input_text}}
}

