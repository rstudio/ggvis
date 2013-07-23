as.reactive <- function(x, ...) UseMethod("as.reactive")

#' @S3method as.reactive function
as.reactive.function <- function(x, ...) x
#' @S3method as.reactive refMethodDef
as.reactive.refMethodDef <- function(x, ...) x
#' @S3method as.reactive default
as.reactive.default <- function(x, ...) reactive(x, ...)

is.reactive <- function(x) {
  inherits(x, "refMethodDef")
}

needs_shiny <- function() {
  if (!require("shiny")) {
    stop("Please install the shiny package", call. = FALSE)
  }
}