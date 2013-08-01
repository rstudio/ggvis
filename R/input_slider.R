#' Create a placeholder for a slider input
#' @export
input_slider <- function(id = NULL, label = NULL, value = NULL,
                         min = NULL, max = NULL, step = NULL) {

  assert_that(!is.null(value) && !is.null(min) && !is.null(max))

  if (is.null(id)) id <- paste0("slider_", rand_id())
  if (is.null(label)) label <- id

  delayed_reactive(
    function(session) {
      session$input[[id]]
    },
    shiny::sliderInput(inputId = id, label = label, value = value, min = min,
      max = max, step = step)
  )
}
