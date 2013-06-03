#' @export
scale <- function(name, type = "linear", zero = FALSE) {
  list(
    name = name,
    type = type,
    zero = zero
  )
  # TODO: validate arguments. Some scales don't use some properties; e.g.,
  # color doesn't use zero.
}
