\name{input_radiobuttons}
\alias{input_radiobuttons}
\title{Create a placeholder for a radio button input.}
\usage{
  input_radiobuttons(choices, selected = NULL, label = "",
    id = rand_id("radio_"), map = identity, scale = FALSE)
}
\arguments{
  \item{choices}{List of values to select from (if elements
  of the list are named then that name rather than the
  value is displayed to the user)}

  \item{selected}{Name of initially selected item (if not
  specified then defaults to the first item)}

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
  Create a placeholder for a radio button input.
}
\examples{
input_radiobuttons(choices = c("Linear" = "lm", "LOESS" = "loess"),
                   label = "Model type")
input_radiobuttons(choices = c("Linear" = "lm", "LOESS" = "loess"),
                   selected = "LOESS",
                   label = "Model type")

# Used in a branch_smooth
branch_smooth(model = input_radiobuttons(
  choices = c("Linear" = "lm", "LOESS" = "loess"),
              selected = "LOESS",
              label = "Model type"))
}
\seealso{
  Other interactive input: \code{\link{input_checkbox}},
  \code{\link{input_checkboxgroup}},
  \code{\link{input_numeric}}, \code{\link{input_select}},
  \code{\link{input_slider}}, \code{\link{input_text}}
}

