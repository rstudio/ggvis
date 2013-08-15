#' Guess the value.
#'
#' This is an object used to indicate that the default value is to guess at
#' the paramter. Guess values will always generate a message on the console
#' indicating what the guess was. To supress this message supply a concrete
#' value.
#'
#' @export
guess <- function() {
  structure(list(NULL), class = "guess")
}

#' @export
#' @rdname guess
#' @param x object to test for guess-ness
is.guess <- function(x) inherits(x, "guess")

#' @S3method toString guess
toString.guess <- function(x, ...) "guess()"
