#' Create a new data source.
#'
#' @param data a data frame
#' @param name the name of the data frame (used in error messages etc.)
#' @export
#' @importFrom digest digest
#' @examples
#' datasource(mtcars)
#'
#' # A simple example of a reactive data source
#' library(shiny)
#' v <- reactiveValues(n = 10)
#' p <- pipeline(reactive(mtcars[1:v$n, ]))
#' props <- props(x ~ wt, y ~ mpg)
#'
#' sluice(p, props)
#'
#' v$n <- 5
#' sluice(p, props)
datasource <- function(data, name = deparse(substitute(data))) {
  if (is.null(data)) return(NULL)

  structure(list(
    env = environment(),
    name = name,
    hash = digest(data)
  ), class = c(source_class(data), "datasource", "pipe"))
}

source_class <- function(x) UseMethod("source_class")
#' @S3method source_class reactive
source_class.reactive <- function(x) "datasource_reactive"
#' @S3method source_class default
source_class.default <- function(x) NULL

#' @S3method format datasource
format.datasource <- function(x, ...) {
  paste0("|-> ", x$name, " (", x$hash, ")")
}

#' @S3method is_source datasource
is_source.datasource <- function(x) TRUE

#' @S3method pipe_id datasource
pipe_id.datasource <- function(x, props) paste0(x$name, "_", x$hash)

# Connect methods --------------------------------------------------------------

#' @S3method connect datasource
#' @importFrom shiny reactive
connect.datasource <- function(x, props, source = NULL, session = NULL) {
  reactive(x$env$data)
}
#' @S3method connect datasource_reactive
connect.datasource_reactive <- function(x, props, source = NULL, session = NULL) {
  x$env$data
}

