#' Generate sequence of fixed size intervals covering range.
#'
#' @param range range
#' @param size interval size
#' @param ... other arguments passed on to methods
#' @keywords internal
#' @export
#' @seealso \code{\link[plyr]{round_any}}
fullseq <- function(range, size, ...) UseMethod("fullseq")

#' @export
fullseq.numeric <- function(range, size, ..., pad = FALSE) {
  # if (zero_range(range)) return(range + size * c(-1, 1) / 2)

  x <- seq(
    round_any(range[1], size, floor),
    round_any(range[2], size, ceiling),
    by = size
  )

  if (pad) {
    # Add extra bin on bottom and on top, to guarantee that we cover complete
    # range of data, whether right = T or F
    c(min(x) - size, x, max(x) + size)
  } else {
    x
  }
}

#' @export
fullseq.POSIXt <- function(range, size, ...) {
  seq(range[1], range[2], by = size)
}
#' @export
fullseq.Date <- fullseq.POSIXt

round_any <- function(x, accuracy, f = round) {
  f(x / accuracy) * accuracy
}
