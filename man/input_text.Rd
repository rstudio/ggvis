\name{input_text}
\alias{input_text}
\title{Create a placeholder for a text input.}
\usage{
  input_text(value, label = "", id = rand_id("text_"),
    map = identity)
}
\arguments{
  \item{value}{Initial value}

  \item{label}{Display label for the control}
}
\description{
  Create a placeholder for a text input.
}
\examples{
input_text(label = "Point color", value = "red")

ggvis(mtcars, props(x ~ wt, y ~ mpg),
  mark_symbol(
    props(fill = prop(input_text(label = "Point color", value = "red"),
                      constant = TRUE, scale = FALSE))
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

