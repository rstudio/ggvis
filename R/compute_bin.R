#' Bin data along a continuous variable
#'
#' @param x Dataset-like object to bin. Built-in methods for data frames,
#'   grouped data frames and ggvis visualisations.
#' @param x_var,w_var Names of x and weight variables. The x variable must be
#'   continuous.
#' @param width The width of the bins. The default is \code{NULL}, which yields
#'   30 bins that cover the range of the data. You should always override this
#'   value, exploring multiple widths to find the best to illustrate the stories
#'   in your data.
#' @param center The center of one of the bins.  Note that if center is above or
#'   below the range of the data, things will be shifted by an appropriate
#'   number of \code{width}s. To center on integers, for example, use
#'   \code{width=1} and \code{center=0}, even if \code{0} is outside the range
#'   of the data.  At most one of \code{center} and \code{boundary} may be
#'   specified.
#' @param boundary A boundary between two bins. As with \code{center}, things
#'   are shifted when \code{boundary} is outside the range of the data. For
#'   example, to center on integers, use \code{width = 1} and \code{boundary =
#'   0.5}, even if \code{1} is outside the range of the data.  At most one of
#'   \code{center} and \code{boundary} may be specified.
#' @param closed One of \code{"right"} or \code{"left"} indicating whether right
#'   or left edges of bins are included in the bin.
#' @param pad If \code{TRUE}, adds empty bins at either end of x. This ensures
#'   frequency polygons touch 0. Defaults to \code{FALSE}.
#' @param binwidth Deprecated; use \code{width} instead.
#' @seealso \code{\link{compute_count}} For counting cases at specific locations
#'   of a continuous variable. This is useful when the variable is continuous
#'   but the data is granular.
#' @export
#' @return A data frame with columns:
#'  \item{count_}{the number of points}
#'  \item{x_}{mid-point of bin}
#'  \item{xmin_}{left boundary of bin}
#'  \item{xmax_}{right boundary of bin}
#'  \item{width_}{width of bin}
#' @examples
#' mtcars %>% compute_bin(~mpg)
#' mtcars %>% compute_bin(~mpg, width = 10)
#' mtcars %>% group_by(cyl) %>% compute_bin(~mpg, width = 10)
#'
#' # It doesn't matter whether you transform inside or outside of a vis
#' mtcars %>% compute_bin(~mpg) %>% ggvis(~x_, ~count_) %>% layer_paths()
#' mtcars %>% ggvis(~ x_, ~ count_) %>% compute_bin(~mpg) %>% layer_paths()
#'
#' # Missing values get own bin
#' mtcars2 <- mtcars
#' mtcars2$mpg[sample(32, 5)] <- NA
#' mtcars2 %>% compute_bin(~mpg, width = 10)
#'
#' # But are currently silently dropped in histograms
#' mtcars2 %>% ggvis() %>% layer_histograms(~mpg)
compute_bin <- function(x, x_var, w_var = NULL, width = NULL,
                        center = NULL, boundary = NULL,
                        closed = c("right", "left"), pad = FALSE,
                        binwidth) {
  UseMethod("compute_bin")
}

#' @export
compute_bin.data.frame <- function(x, x_var, w_var = NULL, width = NULL,
                                   center = NULL, boundary = NULL,
                                   closed = c("right", "left"), pad = FALSE,
                                   binwidth) {

  if (!missing(binwidth)) {
    width <- binwidth
    deprecated("binwidth", "width", version = "0.3.0")
  }

  closed <- match.arg(closed)
  assert_that(is.formula(x_var))

  x_val <- eval_vector(x, x_var)

  # Special case zero-row input
  if (length(x_val) == 0) return(bin_out())

  params <- bin_params(range2(x_val, na.rm = TRUE), width = width,
                       center = center, boundary = boundary, closed = closed)

  if (is.null(w_var)) {
    w_val <- NULL
  } else {
    w_val <- eval_vector(x, w_var)
  }

  bin_vector(x_val, weight = w_val, width = params$width,
             origin = params$origin, closed = params$closed, pad = pad)
}

#' @export
compute_bin.grouped_df <- function(x, x_var, w_var = NULL, width = NULL,
                                   center = NULL, boundary = NULL,
                                   closed = c("right", "left"), pad = FALSE,
                                   binwidth) {

  if (!missing(binwidth)) {
    width <- binwidth
    deprecated("binwidth", "width", version = "0.3.0")
  }

  closed <- match.arg(closed)
  x_val <- eval_vector(x, x_var)

  # We want to use the same boundary and width across groups, so calculate
  # bin params here.
  params <- bin_params(range2(x_val), width = width, center = center,
                       boundary = boundary, closed = closed)

  dplyr::do(x, compute_bin(.,
    x_var,
    w_var = w_var,
    width = params$width,
    center = NULL,
    boundary = params$origin,
    closed = params$closed,
    pad = pad
  ))
}

#' @export
compute_bin.ggvis <- function(x, x_var, w_var = NULL, width = NULL,
                              center = NULL, boundary = NULL,
                              closed = c("right", "left"), pad = FALSE,
                              binwidth) {
  if (!missing(binwidth)) {
    width <- binwidth
    deprecated("binwidth", "width", version = "0.3.0")
  }

  closed <- match.arg(closed)
  args <- list(x_var = x_var, w_var = w_var, width = width,
               center = center, boundary = boundary, closed = closed, pad = pad)

  register_computation(x, args, "bin", function(data, args) {
    output <- do_call(compute_bin, quote(data), .args = args)
    preserve_constants(data, output)
  })
}

# Compute parameters -----------------------------------------------------------

bin_params <- function(x_range, width = NULL, center = NULL, boundary = NULL,
                       closed = c("right", "left")) {
  UseMethod("bin_params")
}

#' @export
bin_params.default <- function(x_range, width = NULL, center = NULL,
                               boundary = NULL, closed = c("right", "left")) {
  closed <- match.arg(closed)

  if (length(x_range) == 0) {
    return(list(width = width, origin = NULL, closed = closed))
  }

  stopifnot(length(x_range) == 2)
  if (!is.null(boundary) && !is.null(center)) {
    stop("Only one of 'boundary' and 'center' may be specified.")
  }

  if (is.null(width)) {
    # Find a nice-looking value for width
    bounds <- pretty(x_range, 30)
    width <- bounds[2] - bounds[1]
    notify_guess(width, paste0("range / ", length(bounds)-1))
  }

  if (is.null(boundary)) {
    if (is.null(center)) {
      # If neither edge nor center given, compute both using tile layer's
      # algorithm. This puts min and max of data in outer half of their bins.
      boundary <- width / 2

    } else {
      # If center given but not boundary, compute boundary.
      boundary <- center - width / 2
    }
  }

  # Inputs could be Dates or POSIXct, so make sure these are all numeric
  x_range <- as.numeric(x_range)
  width <- as.numeric(width)
  boundary <- as.numeric(boundary)

  origin <- find_origin(x_range, width, boundary)

  list(width = width, origin = origin, closed = closed)
}

#' @export
bin_params.POSIXct <- function(x_range, width = NULL, center = NULL,
                               boundary = NULL, closed = c("right", "left")) {
  if (length(x_range) == 0) {
    return(list(width = width, origin = NULL, closed = closed))
  }

  if (is.null(width)) {
    bounds <- pretty(x_range, 30)
    width <- bounds[2] - bounds[1]
    notify_guess(width, paste0("range / ", length(bounds)-1))
  }

  # Period object from lubridate package - need lubridate::as.difftime to find
  # the correct generic, instead of base::as.difftime.
  if (is(width, "Period")) {
    width <- as.numeric(lubridate::as.difftime(width, units = "secs"))
  }

  bin_params(
    as.numeric(x_range),
    as.numeric(width, units = "secs"),
    as_numeric(center),
    as_numeric(boundary),
    closed
  )
}

# Find the left side of left-most bin
find_origin <- function(x_range, width, boundary) {
  shift <- floor((x_range[1] - boundary) / width)
  boundary + shift * width
}

# Bin individual vector --------------------------------------------------------

#' Bin vectors
#'
#' A generic and several implementations for binning vectors.
#'
#' @param x A vector to bin
#' @param weight If specified, an integer vector of the same length as \code{x}
#'   representing the number of occurances of each value in \code{x}
#' @param width The width of a bin
#' @param origin The left-most value for bins.
#' @param closed One of \code{"right"} or \code{"left"} indicating whether
#'   right or left edges of bins are included in the bin.
#' @param pad A logical indicating whether the bins should be padded to include
#'   an empty bin on each side.
#' @param ... additional arguments passed through to methods.
#' @keywords internal
bin_vector <- function(x, weight = NULL, ...) {
  UseMethod("bin_vector")
}

#' @export
bin_vector.numeric <- function(x, weight = NULL, ..., width = 1,
                               origin = 0, closed = c("right", "left"),
                               pad = FALSE) {
  closed <- match.arg(closed)

  if (all(is.na(x))) {
    return(bin_out(length(x), NA, NA, xmin = NA, xmax = NA))
  }

  stopifnot(is.numeric(width) && length(width) == 1)
  stopifnot(is.numeric(origin) && length(origin) == 1)

  if (is.null(weight)) {
    weight <- rep(1, length(x))
  } else {
    weight[is.na(weight)] <- 0
  }

  min_x <- origin
  # Small correction factor so that we don't get an extra bin when, for
  # example, origin=0, max(x)=20, width=10.
  max_x <- max(x, na.rm = TRUE) + (1 - 1e-08) * width
  breaks <- seq(min_x, max_x, width)
  fuzzybreaks <- adjust_breaks(breaks, closed = closed)

  bins <- cut(x, fuzzybreaks, include.lowest = TRUE, right = (closed == "right"))

  left <- breaks[-length(breaks)]
  right <- breaks[-1]
  x <- (left + right) / 2
  bin_widths <- diff(breaks)

  count <- as.numeric(tapply(weight, bins, sum, na.rm = TRUE))
  count[is.na(count)] <- 0

  if (pad) {
    count <- c(0, count, 0)
    bin_widths <- c(width, bin_widths, width)
    x <- c(x[1] - width, x, x[length(x)] + width)
  }

  # Add row for missings
  if (any(is.na(bins))) {
    count <- c(count, sum(is.na(bins)))
    left <- c(left, NA)
    right <- c(right, NA)
    x <- c(x, NA)
    bin_widths <- c(bin_widths, NA)
  }

  bin_out(count, x, bin_widths)
}

#' @export
bin_vector.POSIXct <- function(x, weight = NULL, ..., width = 1,
                               origin = NULL, closed = c("right", "left"),
                               pad = FALSE) {

  # Convert times to raw numbers (seconds since UNIX epoch)
  if (is(width, "Period")) {
    width <- as.numeric(lubridate::as.difftime(width, units = "secs"))
  }

  results <- bin_vector(
    as.numeric(x),
    weight = weight,
    width = width,
    origin = if (is.null(origin)) NULL else as.numeric(origin),
    closed = closed,
    pad = pad
  )

  # Convert some columns from numeric back to POSIXct objects
  tz <- attr(x, "tzone", TRUE)
  time_cols <- c("x_", "xmin_", "xmax_")
  results[time_cols] <- lapply(results[time_cols], function(col) {
    as.POSIXct(col, origin = "1970-01-01", tz = tz)
  })

  results
}

#' @export
bin_vector.Date <- function(x, weight = NULL, ..., width = 1,
                            origin = NULL, closed = c("right", "left"),
                            pad = FALSE) {

  results <- bin_vector(
    as.numeric(x),
    weight = weight,
    width = width,
    origin = if (is.null(origin)) NULL else as.numeric(origin),
    closed = closed,
    pad = pad
  )

  # Convert some columns from numeric back to Date objects
  time_cols <- c("x_", "xmin_", "xmax_")
  results[time_cols] <- lapply(results[time_cols], function(col) {
    class(col) <- "Date"
    col
  })

  results
}


#' @export
bin_vector.default <- function(x, weight = NULL, ...) {
  stop("Don't know how to bin vector of type ", class(x))
}

bin_out <- function(count = integer(0), x = numeric(0), width = numeric(0),
                    xmin = x - width / 2, xmax = x + width / 2) {
  data.frame(
    count_ = count,
    x_ = x,
    xmin_ = xmin,
    xmax_ = xmax,
    width_ = width,
    stringsAsFactors = FALSE
  )
}

# Adapt break fuzziness from base::hist - this protects from floating
# point rounding errors
adjust_breaks <- function(breaks, closed = "left") {
  closed <- match.arg(closed, c("right", "left"))

  diddle <- 1e-08 * median(diff(breaks))
  if (closed == "right") {
    fuzz <- c(-diddle, rep.int(diddle, length(breaks) - 1))
  } else {
    fuzz <- c(rep.int(-diddle, length(breaks) - 1), diddle)
  }
  sort(breaks) + fuzz
}
