as.reactive <- function(x, session = NULL, ...) UseMethod("as.reactive")

#' @S3method as.reactive function
as.reactive.function <- function(x, session = NULL, ...) x
#' @S3method as.reactive refMethodDef
as.reactive.refMethodDef <- function(x, session = NULL, ...) x
#' @S3method as.reactive default
as.reactive.default <- function(x, session = NULL, ...) reactive(x, ...)

is.reactive <- function(x) {
  inherits(x, "refMethodDef")
}

needs_shiny <- function() {
  if (suppressWarnings(require("shiny", quietly = TRUE))) {
    return(invisible(TRUE))
  }

  stop("This functionality requires the shiny package.\n", 
       "Please install it with `install.packages('shiny')`", call. = FALSE)
}
