#' Create a placeholder for a slider input.
#' 
#' @param ... other arguments passed on to \code{\link[shiny]{sliderInput}}
#'   to control the appearance of the slider.
#' @importFrom shiny sliderInput
#' @export
#' @examples
#' input_slider(0, 100)
#' input_slider(0, 100, "binwidth")
#' input_slider(0, 100, value = 50)
input_slider <- function(min, max, label = "", value = min, ...,
                         id = rand_id("slider_")) {
  assert_that(is.string(label), is.string(id))

  delayed_reactive(
    from_input(id, value),
    sliderInput(id, label, value = value, min = min, max = max, ...)
  )
}
