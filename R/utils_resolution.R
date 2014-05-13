#' Compute the "resolution" of a data vector.
#'
#' The resolution is is the smallest non-zero distance between adjacent
#' values.  If there is only one unique value, then the resolution is defined
#' to be one.
#'
#' If x is an integer vector, then it is assumed to represent a discrete
#' variable, and the resolution is 1.
#'
#' @param x numeric vector
#' @param zero should a zero value be automatically included in the
#'   computation of resolution
#' @export
#' @examples
#' resolution(1:10)
#' resolution((1:10) - 0.5)
#' resolution((1:10) - 0.5, FALSE)
#' resolution(c(1,2, 10, 20, 50))
#' resolution(as.integer(c(1, 10, 20, 50)))  # Returns 1
resolution <- function(x, zero = TRUE) {
  if (is.integer(x) || zero_range(range(x, na.rm = TRUE)))
    return(1)

  x <- unique(as.numeric(x))
  if (zero) {
    x <- unique(c(0, x))
  }

  min(diff(sort(x)))
}


#' Determine if range of vector is close to zero, with a specified tolerance
#'
#' The machine epsilon is the difference between 1.0 and the next number
#' that can be represented by the machine. By default, this function
#' uses epsilon * 100 as the tolerance. First it scales the values so that
#' they have a mean of 1, and then it checks if the difference between
#' them is larger than the tolerance.
#'
#' @examples
#' eps <- .Machine$double.eps
#' zero_range(c(1, 1 + eps))       # TRUE
#' zero_range(c(1, 1 + 99 * eps))  # TRUE
#' zero_range(c(1, 1 + 101 * eps)) # FALSE - Crossed the tol threshold
#' zero_range(c(1, 1 + 2 * eps), tol = eps) # FALSE - Changed tol
#'
#' # Scaling up or down all the values has no effect since the values
#' # are rescaled to 1 before checking against tol
#' zero_range(100000 * c(1, 1 + eps))        # TRUE
#' zero_range(100000 * c(1, 1 + 200 * eps))  # FALSE
#' zero_range(.00001 * c(1, 1 + eps))        # TRUE
#' zero_range(.00001 * c(1, 1 + 200 * eps))  # FALSE
#'
#' # NA values
#' zero_range(c(1, NA))   # NA
#' zero_range(c(1, NaN))  # NA
#'
#' # Infinite values
#' zero_range(c(1, Inf))     # FALSE
#' zero_range(c(-Inf, Inf))  # FALSE
#' zero_range(c(Inf, Inf))   # TRUE
#'
#' @param x numeric range: vector of length 2
#' @param tol A value specifying the tolerance. Defaults to
#'   \code{.Machine$double.eps * 100}.
#' @return logical \code{TRUE} if the relative difference of the endpoints of
#' the range are not distinguishable from 0.
#' @export
zero_range <- function(x, tol = .Machine$double.eps * 100) {
  if (length(x) == 1) return(TRUE)
  if (length(x) != 2) stop("x must be length 1 or 2")
  if (any(is.na(x)))  return(NA)

  # Special case: if they are equal as determined by ==, then there
  # is zero range. Also handles (Inf, Inf) and (-Inf, -Inf)
  if (x[1] == x[2]) return(TRUE)

  # If we reach this, then x must be (-Inf, Inf) or (Inf, -Inf)
  if (all(is.infinite(x))) return(FALSE)

  # Take the smaller (in magnitude) value of x, and use it as the scaling
  # factor.
  m <- min(abs(x))

  # If we get here, then exactly one of the x's is 0. Return FALSE
  if (m == 0) return(FALSE)

  # If x[1] - x[2] (scaled to 1) is smaller than tol, then return
  # TRUE; otherwise return FALSE
  abs((x[1] - x[2])/m) < tol
}
