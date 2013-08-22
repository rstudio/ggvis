\name{input_checkbox}
\alias{input_checkbox}
\title{Create a placeholder for a checkbox input.}
\usage{
  input_checkbox(value = FALSE, label = "",
    id = rand_id("checkbox_"), map = identity,
    scale = FALSE)
}
\arguments{
  \item{value}{Initial value (\code{TRUE} or
  \code{FALSE}).}

  \item{label}{Display label for the control.}

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
  Create a placeholder for a checkbox input.
}
\examples{
input_checkbox(label = "Confidence interval")
input_checkbox(label = "Confidence interval", value = TRUE)

# Used in a branch_smooth
branch_smooth(se = input_checkbox(label = "Confidence interval"))
branch_smooth(se = input_checkbox(label = "Confidence interval", value = FALSE))

# Used with a map function, to convert the boolean to another type of value
branch_smooth(
  method = input_checkbox(label = "LOESS (curve) model fit",
                          map = function(val) ifelse(val, "loess", "lm"))
)
}
\seealso{
  Other interactive input:
  \code{\link{input_checkboxgroup}},
  \code{\link{input_numeric}},
  \code{\link{input_radiobuttons}},
  \code{\link{input_select}}, \code{\link{input_slider}},
  \code{\link{input_text}}
}

