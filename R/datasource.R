#' Create a new data source.
#'
#' A data source implements a pointer (using a environment and a promise)
#' to an existing data frame or other data source in R. Most of the time you
#' should not need to call this function directly - just supply an object
#' to \code{\link{ggvis}} and \code{datasource} will be automatically called.
#' You may want to call this function directly in order to control the 
#' datasource name used when printing and in the vega plot specification.
#'
#' @param data a data frame
#' @param name the name of the data frame (used in error messages etc.)
#' @export
#' @keywords internal
#' @importFrom digest digest
#' @examples
#' datasource(mtcars)
#'
#' # A simple example of a reactive data source
#' library(shiny)
#' v <- reactiveValues(n = 10)
#' p <- pipeline(reactive(mtcars[1:v$n, ]))
#' props <- props(x = ~wt, y = ~mpg)
#'
#' sluice(p, props)
#'
#' v$n <- 5
#' sluice(p, props)
datasource <- function(data, name = deparse2(substitute(data))) {
  if (is.null(data)) return(NULL)

  structure(list(
    env = environment(),
    name = name,
    hash = digest(data)
  ), class = c(source_class(data), "datasource", "pipe"))
}

source_class <- function(x) UseMethod("source_class")
#' @export
source_class.reactive <- function(x) "datasource_reactive"
#' @export
source_class.default <- function(x) NULL

#' @export
format.datasource <- function(x, ...) {
  paste0("|-> ", x$name, " (", x$hash, ")")
}

#' @export
is_source.datasource <- function(x) TRUE

#' @export
pipe_id.datasource <- function(x, props) paste0(x$name, "_", x$hash)
