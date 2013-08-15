#' Define padding.
#'
#' @param top,right,bottom,left Amount of padding on each border. Can either
#'   be a single number, "auto", or "strict"
#' @export
#' @examples
#' padding()
#' padding(10, 10, 10, 10)
padding <- function(top = NULL, right = NULL, bottom = NULL, left = NULL) {
  assert_that(is_padding(top), is_padding(right), is_padding(bottom),
    is_padding(left))
  structure(
    compact(list(
      top = top,
      right = right,
      bottom = bottom,
      left = left)
    ),
    class = "padding"
  )
}

#' @S3method as.vega padding
as.vega.padding <- function(x) {
  if (length(x) == 0) {
    NULL
  } else {
    unclass(x)
  }
}

is_padding <- function(x) {
  if (is.null(x)) return(TRUE)

  length(x) == 1 && (
    is.numeric(x) || (x %in% c("auto", "strict")))
}
on_failure(is_padding) <- function(call, env) {
  paste0(deparse(call$x), " is not a single number, 'auto', or 'strict'.")
}
