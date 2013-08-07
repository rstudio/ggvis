#' Create a placeholder for a select input.
#' 
#' @importFrom shiny selectInput
#' @inheritParams shiny::selectInput
#' @family interactive input
#' @export
#' @examples
#' input_select(0, 100)
#' input_select(0, 100, "binwidth")
#' input_select(0, 100, value = 50)
input_select <- function(choices, selected = NULL, multiple = FALSE,
                         label = "", id = rand_id("select_")) {
  assert_that(is.string(label), is.string(id))

  control <- function(session) {
    selectInput(id, label, choices = choices, selected = selected,
      multiple = multiple)
  }

  if (is.null(selected)) {
    if (multiple) value <- ""
    else value <- choices[1]
  } else {
    value <- choices[selected]
  }

  delayed_reactive(from_input(id, value), control, id = id)
}
