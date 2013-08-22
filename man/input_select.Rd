\name{input_select}
\alias{input_select}
\title{Create a placeholder for a select input.}
\usage{
  input_select(choices, selected = NULL, multiple = FALSE,
    label = "", id = rand_id("select_"), map = identity,
    scale = FALSE)
}
\arguments{
  \item{choices}{List of values to select from. If elements
  of the list are named then that name rather than the
  value is displayed to the user.}

  \item{selected}{Name of initially selected item (or
  multiple names if \code{multiple = TRUE}). If not
  specified then defaults to the first item for
  single-select lists and no items for multiple select
  lists.}

  \item{multiple}{Is selection of multiple items allowed?}

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
  Create a placeholder for a select input.
}
\examples{
input_select(c("a", "b", "c"))
input_select(c("a", "b", "c"), multiple = TRUE)
input_select(c("a", "b", "c"), selected = "c")
}
\seealso{
  Other interactive input: \code{\link{input_checkbox}},
  \code{\link{input_checkboxgroup}},
  \code{\link{input_numeric}},
  \code{\link{input_radiobuttons}},
  \code{\link{input_slider}}, \code{\link{input_text}}
}

