#' Transformation: bin continuous variable.
#'
#' @section Input:
#' The data that \code{transform_bin} is applied to, must have methods
#' for \code{prop_type}, \code{prop_range} and \code{bin}. Currently, this
#' implies that the input data must be a data frame.
#'
#' @section Properties:
#' It must have an \code{x} property, and that property must be numeric.
#'
#' @section Ouput:
#'
#' \code{transform_bin} creates a data frame with columns:
#'
#' \itemize{
#'  \item \code{count__}
#'  \item \code{x}
#'  \item \code{xmin__}
#'  \item \code{xmax__}
#'  \item \code{width__}
#' }
#'
#' @param binwidth The width of the bins. The default is \code{guess()}, which
#'   yields 30 bins that cover the range of the data.
#' @param origin The initial position of the left-most bin. If \code{NULL}, the
#'   the default, will using the smallest value in the dataset
#' @param right Should bins be right-open, left-closed, or
#'   right-closed, left-open
#' @export
#' @examples
#' transform_bin()
#' transform_bin(binwidth = 10, origin = 1)
#' transform_bin(right = FALSE)
#'
#' # You can see the results of a transformation by creating your own pipeline
#' # and flowing data through it
#' sluice(transform_bin(10), props(x ~ disp), mtcars)
#' sluice(pipeline(mtcars, by_group("cyl"), transform_bin(10)), props(x ~ disp))
#' # Or
#' pl <- pipeline(mtcars, transform_bin(10))
#' sluice(pl, props(x ~ disp))
transform_bin <- function(binwidth = guess(), origin = NULL, right = TRUE) {
  transform("bin",
    binwidth = binwidth,
    origin = origin,
    right = right
  )
}


#' @rdname transform_bin
#' @export
#' @param props a \code{\link{props}} object describing a property mapping
#' @param ... other arguments passed on to the underlying transform function
branch_histogram <- function(props = NULL, ...) {
  if (is.null(props)) props <- props()

  default <- props(
    x ~ xmin__,
    x2 ~ xmax__,
    y ~ count__,
    y2 = prop_const(0, scale = TRUE)
  )
  props <- merge_props(props, default)

  node(
    data = transform_bin(...),
    mark_rect(props)
  )
}

#' @rdname transform_bin
#' @export
#' @inheritParams branch_histogram
branch_freqpoly <- function(props = NULL, ...) {
  if (is.null(props)) props <- props()

  default <- props(
    x ~ x,
    y ~ count__
  )
  props <- merge_props(props, default)

  node(
    data = transform_bin(...),
    mark_line(props)
  )
}

#' @S3method format transform_bin
format.transform_bin <- function(x, ...) {
  paste0(" -> bin", param_string(x))
}

#' @S3method compute transform_bin
compute.transform_bin <- function(x, props, data) {
  check_prop(x, props, data, "x", "numeric")

  if (is.guess(x$binwidth)) {
    x$binwidth <- diff(prop_range(data, props$x)) / 30
    message("Guess: transform_bin(binwidth = ", format(x$binwidth, digits = 3),
      ") # range / 30")
  }

  output <- bin(data, x_var = props$x,
    binwidth = x$binwidth, origin = x$origin, right = x$right)

  preserve_constants(data, output)
}

bin <- function(data, ...) UseMethod("bin")

#' @S3method bin split_df
bin.split_df <- function(x, x_var, ...) {
  x[] <- lapply(x, bin, x_var = x_var, ...)
  x
}

#' @S3method bin data.frame
bin.data.frame <- function(x, x_var, ...) {
  x_val <- prop_value(x_var, x)
  bin(x_val, ...)
}

#' @S3method bin numeric
bin.numeric <- function(x, weight = NULL, binwidth = 1, origin = NULL, right = TRUE) {
  stopifnot(is.numeric(binwidth) && length(binwidth) == 1)
  stopifnot(is.null(origin) || (is.numeric(origin) && length(origin) == 1))
  stopifnot(is.flag(right))

  if (length(na.omit(x)) == 0)  return(data.frame())

  if (is.null(weight))  weight <- rep(1, length(x))
  weight[is.na(weight)] <- 0

  if (is.null(origin)) {
    breaks <- fullseq(range(x), binwidth, pad = TRUE)
  } else {
    breaks <- seq(origin, max(range) + binwidth, binwidth)
  }

  # Adapt break fuzziness from base::hist - this protects from floating
  # point rounding errors
  diddle <- 1e-07 * stats::median(diff(breaks))
  if (right) {
    fuzz <- c(-diddle, rep.int(diddle, length(breaks) - 1))
  } else {
    fuzz <- c(rep.int(-diddle, length(breaks) - 1), diddle)
  }
  fuzzybreaks <- sort(breaks) + fuzz

  bins <- cut(x, fuzzybreaks, include.lowest = TRUE, right = right)
  left <- breaks[-length(breaks)]
  right <- breaks[-1]
  x <- (left + right)/2
  width <- diff(breaks)

  count <- as.numeric(tapply(weight, bins, sum, na.rm=TRUE))
  count[is.na(count)] <- 0

  results <- data.frame(
    count__ = count,
    x = x,
    xmin__ = x - width/2,
    xmax__ = x + width/2,
    width__ = width
  )

  results
}
