#' Create an interactive slider.
#'
#' @importFrom shiny sliderInput
#' @inheritParams shiny::sliderInput
#' @inheritParams input
#' @family interactive input
#' @export
#' @examples
#' input_slider(0, 100)
#' input_slider(0, 100, label = "binwidth")
#' input_slider(0, 100, value = 50)
#' 
#' # Supply two values to value to make a double-ended sliders
#' input_slider(0, 100, c(25, 75))
#' 
#' # You can use map to transform the outputs
#' input_slider(-5, 5, label = "Log scale", map = function(x) 10 ^ x)
input_slider <- function(min, max, value = min, step = NULL, round = FALSE,
                         format = "#,##0.#####", locale = "us", ticks = TRUE,
                         animate = FALSE, label = "", id = rand_id("slider_"),
                         map = identity, scale = FALSE) {

  assert_that(is.string(label), is.string(id))

  args <- list(id, label, min = min, max = max, value = value, step = step,
      round = round, format = format, locale = locale, ticks = ticks)

  input("slider", args, value, map, id, scale)
}

#' Create an interactive checkbox.
#'
#' @importFrom shiny checkboxInput
#' @inheritParams shiny::checkboxInput
#' @inheritParams input
#' @family interactive input
#' @export
#' @examples
#'
#' input_checkbox(label = "Confidence interval")
#' input_checkbox(label = "Confidence interval", value = TRUE)
#'
#' # Used in a branch_smooth
#' branch_smooth(se = input_checkbox(label = "Confidence interval"))
#'
#' # Used with a map function, to convert the boolean to another type of value
#' model_type <- input_checkbox(label = "Use flexible curve",
#'   map = function(val) if(val) "loess" else "lm")
#' branch_smooth(method = model_type)
input_checkbox <- function(value = FALSE, label = "", 
                           id = rand_id("checkbox_"), map = identity, 
                           scale = FALSE) {

  assert_that(is.string(label), is.string(id))

  args <- list(id, label, value = value)
  input("checkbox", args, value, map, id)
}

#' Create an interactive text or numeric input box.
#'
#' \code{input_numeric} only allows numbers and comes with a spin box control.
#' \code{input_text} allows any type of input.
#'
#' @importFrom shiny textInput
#' @inheritParams shiny::textInput
#' @inheritParams input
#' @family interactive input
#' @export
#' @examples
#' input_text(label = "Point color", value = "red")
#'
#' ggvis(mtcars, props(x ~ wt, y ~ mpg),
#'   mark_symbol(
#'     props(fill = input_text(label = "Point color", value = "red"))
#'   )
#' )
#'
#' input_numeric(label = "Point size", value = 25)
#'
#' ggvis(mtcars, props(x ~ wt, y ~ mpg),
#'   mark_symbol(
#'     props(size = input_numeric(label = "Point size", value = 25))
#'   )
#' )
input_text <- function(value, label = "", id = rand_id("text_"),
                       map = identity, scale = FALSE) {

  assert_that(is.string(label), is.string(id), is.string(value))

  args <- list(id, label, value = value)
  input("text", args, value, map, id, scale)
}

#' @rdname input_text
#' @export
#' @importFrom shiny numericInput
input_numeric <- function(value, label = "", id = rand_id("text_"),
                          map = identity, scale = FALSE) {

  assert_that(is.string(label), is.string(id), is.numeric(value))

  args <- list(id, label, value = value)

  input("numeric", args, value, map, id, scale)
}

#' Create interactive control to select one (or more options) from a list.
#' 
#' \itemize{
#'  \item \code{input_radiobuttons} only ever selects one value
#'  \item \code{input_checkboxgroup} can alway select multiple values
#'  \item \code{input_select} can select only one if \code{multiple = FALSE},
#'    otherwise the user can select multiple by using modifier keys
#' }
#'
#' @importFrom shiny selectInput
#' @inheritParams shiny::selectInput
#' @inheritParams input
#' @family interactive input
#' @export
#' @examples
#' # Dropdown
#' input_select(c("a", "b", "c"))
#' input_select(c("a", "b", "c"), multiple = TRUE)
#' input_select(c("a", "b", "c"), selected = "c")
#' 
#' # If you want to select variable names, you need to convert
#' # the output of the input to a name with map so that they get 
#' # computed correctly
#' input_select(names(mtcars), map = as.name)
#' 
#' # Radio buttons
#' input_radiobuttons(choices = c("Linear" = "lm", "LOESS" = "loess"),
#'                    label = "Model type")
#' input_radiobuttons(choices = c("Linear" = "lm", "LOESS" = "loess"),
#'                    selected = "LOESS",
#'                    label = "Model type")
#'
#' # Used in a branch_smooth
#' branch_smooth(model = input_radiobuttons(
#'   choices = c("Linear" = "lm", "LOESS" = "loess"),
#'               selected = "LOESS",
#'               label = "Model type"))
input_select <- function(choices, selected = NULL, multiple = FALSE,
  label = "", id = rand_id("select_"),
  map = identity, scale = FALSE) {
  assert_that(is.string(label), is.string(id))
  
  args <- list(id, label, choices = choices, selected = selected,
    multiple = multiple)
  
  if (is.null(selected)) {
    if (multiple) value <- ""
    else value <- choices[1]
  } else {
    value <- choices[selected]
  }
  
  input("select", args, value, map, id, scale)
}

#' @rdname input_select
#' @export
#' @importFrom shiny radioButtons
input_radiobuttons <- function(choices, selected = NULL, label = "",
                               id = rand_id("radio_"), map = identity, 
                               scale = FALSE) {

  assert_that(is.string(label), is.string(id))

  args <- list(id, label, choices = choices, selected = selected)

  if (is.null(selected)) {
    value <- choices[1]
  } else {
    value <- choices[selected]
  }

  input("radio_buttons", args, value, map, id, scale, 
    control_f = "radioButtons")
}

#' @rdname input_select
#' @export
#' @importFrom shiny checkboxGroupInput
input_checkboxgroup <- function(choices, selected = NULL, label = "",
                                id = rand_id("radio_"), map = identity,
                                scale = FALSE) {

  assert_that(is.string(label), is.string(id))

  args <- list(id, label, choices = choices, selected = selected)

  if (is.null(selected)) {
    value <- character(0)
  } else {
    value <- choices[selected]
  }

  input("checkbox_group", args, value, map, id, scale)
}
