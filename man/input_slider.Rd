\name{input_slider}
\alias{input_slider}
\title{Create a placeholder for a slider input.}
\usage{
  input_slider(min, max, label = "", value = min, ...,
    id = rand_id("slider_"))
}
\arguments{
  \item{...}{other arguments passed on to
  \code{\link[shiny]{sliderInput}} to control the
  appearance of the slider.}
}
\description{
  Create a placeholder for a slider input.
}
\examples{
input_slider(0, 100)
input_slider(0, 100, "binwidth")
input_slider(0, 100, value = 50)
}

