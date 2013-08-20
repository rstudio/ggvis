\name{input_checkbox}
\alias{input_checkbox}
\title{Create a placeholder for a checkbox input.}
\usage{
  input_checkbox(value = FALSE, label = "",
    id = rand_id("checkbox_"), map = identity)
}
\arguments{
  \item{value}{Initial value (\code{TRUE} or
  \code{FALSE}).}

  \item{label}{Display label for the control.}
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

