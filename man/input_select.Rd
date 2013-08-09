\name{input_select}
\alias{input_select}
\title{Create a placeholder for a select input.}
\usage{
  input_select(choices, selected = NULL, multiple = FALSE,
    label = "", id = rand_id("select_"),
    wrapfun = identity)
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
}
\description{
  Create a placeholder for a select input.
}
\examples{
input_select(0, 100)
input_select(0, 100, "binwidth")
input_select(0, 100, value = 50)
}
\seealso{
  Other interactive input: \code{\link{input_slider}}
}

