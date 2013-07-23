#' A reactive source.
#'
#' @param data A reactive values object or reactive expression.
#' @param name A name for the object.
#' @export
#' @examples
#' source_reactive(reactive(mtcars))
#' 
#' # Pipeline automatically puts reactive objects in a source_reactive
#' pipeline(reactive(mtcars))
#' 
#' # A simple example
#' v <- reactiveValues(n = 10)
#' p <- pipeline(reactive(mtcars[1:v$n, ]))
#' props <- props(x ~ wt, y ~ mpg)
#'
#' sluice(p, props)
#'
#' v$n <- 5
#' sluice(p, props)
source_reactive <- function(data, name = NULL) {
  assert_that(is.reactive(data))
  
  if (is.null(name))
    name <- paste0("reactive_", digest(data, algo = "crc32"))
  assert_that(is.string(name))
  
  pipe("source_reactive", data = data, name = name)
}

#' @S3method connect source_reactive
connect.source_reactive <- function(x, props, data) {
  x$data
}

#' @S3method format source_reactive
format.source_reactive <- function(x, ...) {
  paste0("|-> ", x$name, " [reactive]")
}

#' @S3method is_source source_reactive
is_source.source_reactive <- function(x) TRUE

#' @S3method pipe_id source_reactive
pipe_id.source_reactive <- function(x, props) {
  x$name
}
