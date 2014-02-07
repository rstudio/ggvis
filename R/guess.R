#' Guess the value.
#'
#' This is an object used to indicate that the default value is to guess at
#' the paramter. Guess values will always generate a message on the console
#' indicating what the guess was. To supress this message supply a concrete
#' value.
#'
#' @export
guess <- function() {
  structure(list(new.env(parent = emptyenv())), class = "guess")
}

#' @export
print.guess <- function(x, ...) cat(toString(x))

#' @export
#' @rdname guess
#' @param x object to test for guess-ness
is.guess <- function(x) inherits(x, "guess")

#' @export
toString.guess <- function(x, ...) "guess()"


guess_cache <- function(x, name, value) {
  if (exists(name, envir = x[[1]])) {
    return(get(name, envir = x[[1]]))
  }

  assign(name, value, envir = x[[1]])
  message("Guessing ", name, " = ", format(value))
  value
}
