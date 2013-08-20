\name{input_checkboxgroup}
\alias{input_checkboxgroup}
\title{Create a placeholder for a checkbox group input.}
\usage{
  input_checkboxgroup(choices, selected = NULL, label = "",
    id = rand_id("radio_"), map = identity)
}
\arguments{
  \item{choices}{List of values to show checkboxes for. If
  elements of the list are named then that name rather than
  the value is displayed to the user.}

  \item{selected}{Names of items that should be initially
  selected, if any.}

  \item{label}{Display label for the control.}
}
\description{
  Create a placeholder for a checkbox group input.
}
\seealso{
  Other interactive input: \code{\link{input_checkbox}},
  \code{\link{input_numeric}},
  \code{\link{input_radiobuttons}},
  \code{\link{input_select}}, \code{\link{input_slider}},
  \code{\link{input_text}}
}

