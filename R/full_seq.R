#' Generate sequence of fixed size intervals covering range.
#'
#' @param range range
#' @param size interval size
#' @param ... other arguments passed on to methods
#' @keywords internal
#' @export
#' @seealso \code{\link[plyr]{round_any}}
fullseq <- function(range, size, ...) UseMethod("fullseq")

#' @S3method fullseq numeric
fullseq.numeric <- function(range, size, ..., pad = FALSE) {
  # if (zero_range(range)) return(range + size * c(-1, 1) / 2)

  x <- seq(
    plyr::round_any(range[1], size, floor),
    plyr::round_any(range[2], size, ceiling),
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

#' @S3method fullseq Date
fullseq.Date <- function(range, size, ...) {
  seq(floor_date(range[1], size), ceiling_date(range[2], size), by = size)
}
#' @S3method fullseq POSIXt
fullseq.POSIXt <- function(range, size, ...) {
  seq(floor_time(range[1], size), ceiling_time(range[2], size), by = size)
}
