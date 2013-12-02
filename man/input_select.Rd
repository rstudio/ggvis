\name{input_select}
\alias{input_checkboxgroup}
\alias{input_radiobuttons}
\alias{input_select}
\title{Create interactive control to select one (or more options) from a list.}
\usage{
input_select(choices, selected = NULL, multiple = FALSE, label = "",
  id = rand_id("select_"), map = identity)

input_radiobuttons(choices, selected = NULL, label = "",
  id = rand_id("radio_"), map = identity)

input_checkboxgroup(choices, selected = NULL, label = "",
  id = rand_id("radio_"), map = identity)
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
}
\description{
\itemize{ \item \code{input_radiobuttons} only ever selects
one value \item \code{input_checkboxgroup} can alway select
multiple values \item \code{input_select} can select only
one if \code{multiple = FALSE}, otherwise the user can
select multiple by using modifier keys }
}
\examples{
# Dropdown
input_select(c("a", "b", "c"))
input_select(c("a", "b", "c"), multiple = TRUE)
input_select(c("a", "b", "c"), selected = "c")

# If you want to select variable names, you need to convert
# the output of the input to a name with map so that they get
# computed correctly
input_select(names(mtcars), map = as.name)

# Radio buttons
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
\code{\link{input_numeric}}, \code{\link{input_slider}},
\code{\link{input_text}}
}

