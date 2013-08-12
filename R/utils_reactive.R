as.reactive <- function(x, session = NULL, ...) UseMethod("as.reactive")

#' @S3method as.reactive function
as.reactive.function <- function(x, session = NULL, ...) x
#' @S3method as.reactive reactive
as.reactive.reactive <- function(x, session = NULL, ...) x
#' @S3method as.reactive default
as.reactive.default <- function(x, session = NULL, ...) reactive(x, ...)
