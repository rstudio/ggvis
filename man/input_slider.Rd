\name{input_slider}
\alias{input_slider}
\title{Create a placeholder for a slider input.}
\usage{
  input_slider(min, max, value = min, step = NULL,
    round = FALSE, format = "#,##0.#####", locale = "us",
    ticks = TRUE, animate = FALSE, label = "",
    id = rand_id("slider_"), map = identity, scale = FALSE)
}
\arguments{
  \item{min}{The minimum value (inclusive) that can be
  selected.}

  \item{max}{The maximum value (inclusive) that can be
  selected.}

  \item{value}{The initial value of the slider. A warning
  will be issued if the value doesn't fit between
  \code{min} and \code{max}.}

  \item{step}{Specifies the interval between each
  selectable value on the slider (\code{NULL} means no
  restriction).}

  \item{round}{\code{TRUE} to round all values to the
  nearest integer; \code{FALSE} if no rounding is desired;
  or an integer to round to that number of digits (for
  example, 1 will round to the nearest 10, and -2 will
  round to the nearest .01). Any rounding will be applied
  after snapping to the nearest step.}

  \item{format}{Customize format values in slider labels.
  See
  \url{http://archive.plugins.jquery.com/project/numberformatter}
  for syntax details.}

  \item{locale}{The locale to be used when applying
  \code{format}. See details.}

  \item{ticks}{\code{FALSE} to hide tick marks, \code{TRUE}
  to show them according to some simple heuristics.}

  \item{animate}{\code{TRUE} to show simple animation
  controls with default settings; \code{FALSE} not to; or a
  custom settings list, such as those created using
  \code{\link{animationOptions}}.}

  \item{label}{A descriptive label to be displayed with the
  widget.}

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
  Create a placeholder for a slider input.
}
\examples{
input_slider(0, 100)
input_slider(0, 100, label = "binwidth")
input_slider(0, 100, value = 50)
}
\seealso{
  Other interactive input: \code{\link{input_checkbox}},
  \code{\link{input_checkboxgroup}},
  \code{\link{input_numeric}},
  \code{\link{input_radiobuttons}},
  \code{\link{input_select}}, \code{\link{input_text}}
}

