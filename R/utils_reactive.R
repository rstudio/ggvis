as.reactive <- function(x, session = NULL, ...) UseMethod("as.reactive")

#' @export
as.reactive.function <- function(x, session = NULL, ...) x
#' @export
as.reactive.reactive <- function(x, session = NULL, ...) x
#' @export
as.reactive.default <- function(x, session = NULL, ...) reactive(x, ...)
