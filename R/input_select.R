#' Create a placeholder for a select input.
#'
#' @importFrom shiny selectInput
#' @inheritParams shiny::selectInput
#' @param id a unique identifying name for this control - only one control
#'   for a given name will be displayed on a page
#' @param map a function with a singe argument that takes the value returned
#'   from the input control and converts it to an argument useful for ggvis.
#'   Defaults to \code{identity}, leaving the output unchanged.
#' @family interactive input
#' @export
#' @examples
#' input_select(c("a", "b", "c"))
#' input_select(c("a", "b", "c"), multiple = TRUE)
#' input_select(c("a", "b", "c"), selected = "c")
input_select <- function(choices, selected = NULL, multiple = FALSE,
                         label = "", id = rand_id("select_"),
                         map = identity) {
  assert_that(is.string(label), is.string(id))

  controls <- list(id, label, choices = choices, selected = selected,
      multiple = multiple)

  if (is.null(selected)) {
    if (multiple) value <- ""
    else value <- choices[1]
  } else {
    value <- choices[selected]
  }

  delayed_reactive("input_select", from_input(id, value, map), controls,
    id = id)
}

#' @S3method controls input_select
controls.input_select <- function(x, session = NULL) {
  setNames(list(do.call(selectInput, x$controls)), x$id)
}
