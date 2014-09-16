#' Bin data along a continuous variable
#'
#' @param x Dataset-like object to bin. Built-in methods for data frames,
#'   grouped data frames and ggvis visualisations.
#' @param x_var,w_var Names of x and weight variables. The x variable must be
#'   continuous.
#' @param width The width of the bins. The default is \code{NULL}, which
#'   yields 30 bins that cover the range of the data. You should always override
#'   this value, exploring multiple widths to find the best to illustrate the
#'   stories in your data.
#' @param center The center of one of the bins.  Note that if center is above or below
#'   the range of the data, things will be shifted by an appropriate number of \code{width}s.
#'   To center on integers,
#'   for example, use \code{width = 1} and \code{center = 0}, even if \code{0} is
#'   outside the range of the data.  At most one of \code{center} and \code{boundary} may be
#'   specified.
#' @param boundary A boundary between two bins. As with \code{center}, things are shifted
#'   when \code{boundary} is outside the range of the data. For example, to center on
#'   integers, use \code{width = 1} and \code{boundary = 0.5}, even if \code{1} is outside
#'   the range of the data.  At most one of \code{center} and \code{boundary} may be
#'   specified.
#' @param closed One of \code{"right"} or \code{"left"} indicating whether 
#'   right or left edges of bins are included in the bin.
#' @param pad If \code{TRUE}, adds empty bins at either end of x. This
#'   ensures frequency polygons touch 0, and adds padding between the data
#'   and axis for histograms.
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
compute_bin <- function(x, x_var, w_var = NULL, width = NULL,
                        center = NULL, boundary = NULL,
                        closed = c("right", "left"), pad = TRUE) {
  UseMethod("compute_bin")
}

#' @export
compute_bin.data.frame <- function(x, x_var, w_var = NULL, width = NULL,
                                   center = NULL, boundary = NULL,
                                   closed = c("right", "left"), pad = TRUE) {
  closed <- match.arg(closed)
  assert_that(is.formula(x_var))

  x_val <- eval_vector(x, x_var)

  x_na <- is.na(x_val)
  if (any(x_na)) {
    message("compute_bin: NA values ignored for binning.")
    x_val <- x_val[!x_na]
  }

  if (is.null(w_var)) {
    w_val <- NULL
  } else {
    w_val <- eval_vector(x, w_var)
  }

  params <- bin_params(range2(x_val), width = width, center = center,
                       boundary = boundary, closed = closed)

  # note: origin is a boundary, so this works.
  bin_vector(x_val, weight = w_val, width = params$binwidth, boundary = params$origin,
    closed = params$closed, pad = pad)
}

#' @export
compute_bin.grouped_df <- function(x, x_var, w_var = NULL, width = NULL,
                                   center = NULL, boundary = NULL,
                                   closed = c("right", "left"), pad = TRUE) {

  closed <- match.arg(closed)
  x_val <- eval_vector(x, x_var)
  params <- bin_params(range2(x_val), width = width, center = center,
                       boundary = boundary, closed = closed)

  dplyr::do(x, compute_bin(.,
    x_var,
    w_var = w_var,
    width = params$binwidth,
    boundary = params$origin,   # origin is a boundary, so this works
    closed = params$closed,
    pad = pad))
}

#' @export
compute_bin.ggvis <- function(x, x_var, w_var = NULL, width = NULL,
                              center = NULL, boundary = NULL,
                              closed = c("right", "left"), pad = TRUE) {
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
                       closed=c("right", "left")) {
  UseMethod("bin_params")
}

# compute origin from x_range and width
tilelayer_origin <- function(x_range, width) {
  stopifnot(is.numeric(x_range) && length(x_range) == 2)
  stopifnot(is.numeric(width) && length(width) == 1)
  num_central_bins <- trunc(diff(x_range) / width) - 1
  side_width <- (diff(x_range) - num_central_bins * width) / 2  # width of partial tiles on either side
  x_range[1] + side_width - width
  # adjust_breaks should be called to handle any round-off fuzziness issues
}

compute_origin <- function(x_range, width, boundary) {
  shift <- floor( (x_range[1] - boundary) / width )
  boundary + shift * width
}

#' @export
bin_params.numeric <- function(x_range, width = NULL,
                               center = NULL,
                               boundary = NULL,
                               closed = c("right", "left")) {
  closed <- match.arg(closed)
  stopifnot(is.numeric(x_range) && length(x_range) == 2)
  if (!is.null(boundary) && !is.null(center)) {
    stop( "Only one of 'boundary' and 'center' may be specified." )
  }

  if (is.null(width)) {
    width <- diff(x_range) / 30
    notify_guess(width, "range / 30")
  }

  # if neither edge nor center given, compute both using tile layer's algorithm
  # this puts min and max of data in outer half of their bins.

  if (is.null(boundary) && is.null(center)) {
    boundary <- tilelayer_origin(x_range, width)
    # center <- boundary + width / 2
  }

  # if center given but not boundary, compute boundary from center
  if (is.null(boundary)) boundary <- center - width / 2

  origin <- compute_origin( x_range, width, boundary )

  list(binwidth = width, origin = origin, closed = closed)
}

#' @export
bin_params.POSIXct <- function(x_range, width = NULL,
                               center = NULL, boundary = NULL,
                               closed = c("right", "left")) {
  closed <- match.arg(closed)
  if (!is.null(boundary) && !is.null(center)) {
    stop( "Only one of 'boundary' and 'center' may be specified." )
  }

  x_range <- as.numeric(x_range)
  if (inherits(width, "Period")) {
    width <- as.numeric(as.difftime(width, units = "secs"))
  }
  if (!is.null(width)) width <- as.numeric(width)
  if (!is.null(center)) width <- as.numeric(center)
  if (!is.null(boundary)) width <- as.numeric(boundary)

  if (is.null(width)) {
    width <- diff(x_range) / 30
    notify_guess(width, "range / 30 (in seconds)")
    width <- as.numeric(width,  units = "secs")
  }

  if (is.null(boundary) && is.null(center)) {
    boundary <- tilelayer_origin(x_range, width)
    center <- boundary + width / 2
  }

  if (!is.numeric(width)) width <- as.numeric(width, units = 'secs')
  if (!is.null(center)) center <- as.numeric(center)
  if (!is.null(boundary)) boundary<- as.numeric(boundary)

  # if we have center but not boundary, compute boundary
  if (is.null(boundary)) boundary <- center - width / 2

  origin <- compute_origin( x_range, width, boundary )

  list(binwidth = width, origin = origin, closed = closed,
       origin.POSIX = structure(origin, class = c("POSIXct", "POSIXt"))
  )
}

### check on this -- get center in instead of origin
#' @export
bin_params.Date <- function(x_range, width = NULL, center = NULL,
                            boundary = NULL, closed = c("right", "left")) {
  
  closed <- match.arg(closed)
  
  # convert things to numeric as we go along

  x_range <- as.numeric(x_range)

  if (is.null(width)) {
    width <- diff(x_range) / 30
    notify_guess(width, "range / 30")
  }
  width <- as.numeric(width)

  if (is.null(boundary) && is.null(center)) {
    boundary <- tilelayer_origin(x_range, width)
    center <- boundary + width / 2
  }

  # if we have center but not boundary, compute boundary
  if (is.null(boundary)) {
    center <- as.numeric(center)
    boundary <- center - width / 2
  }

  origin <- compute_origin( x_range, width, boundary )

  # do we need to convert this back to date format?

  list(binwidth = width, origin = origin, closed = closed)
}


#' @export
bin_params.integer <- function(x_range, width = NULL,
                               center = NULL, boundary = NULL,
                               closed = c("right", "left")) {

  closed <- match.arg(closed)
  
  if (!is.null(boundary) && !is.null(center)) {
    stop( "Only one of 'boundary' and 'center' may be specified." )
  }

  if (is.null(width)) {
    width <- max(pretty(round(diff(x_range) / 30)))
    if (width <= 1) width <- 1
    num_bins <- ceiling( diff(x_range) / width )
    notify_guess(width, paste0("approximately range/", num_bins) )
  }

  if (is.null(boundary) && is.null(center)) {
    boundary <- tilelayer_origin(x_range, width)
    center <- boundary + width / 2
  }

  # if we have center but not boundary, compute boundary
  if (is.null(boundary)) boundary <- center - width / 2

  origin <- compute_origin( x_range, width, boundary )

  list(binwidth = width, origin = origin, closed = closed)
}

# Bin individual vector --------------------------------------------------------

#' Bin vectors
#'
#' A generic and several implementations for binning vectors.
#'
#' @export
#' @param x a vector to bin
#' @param weight if specified, an integer vector of the same length as \code{x}
#'   representing the number of occurances of each value in \code{x}
#' @param width the width of a bin
#' @param center the center of a bin
#' @param boundary the boundary of a bin.  \code{center} and \code{boundary} should
#'   not both be specified.
#' @param closed One of \code{"right"} or \code{"left"} indicating whether 
#'   right or left edges of bins are included in the bin.
#' @param pad a logical indicatign whether the bins should be padded to include
#'   an empty bin on each side.
#' @param ... additional arguments passed through to instances of the generic
bin_vector <- function(x, weight = NULL, ...) {
  UseMethod("bin_vector")
}

#' @rdname bin_vector
#' @export

bin_vector.numeric <- function(x, weight = NULL, ..., width = NULL,
                               center = NULL, boundary = NULL,
                               closed = c("right", "left"), pad = TRUE) {
  stopifnot(is.null(width) || (is.numeric(width) && length(width) == 1))
  stopifnot(is.null(center) || (is.numeric(center) && length(center) == 1))
  stopifnot(is.null(boundary) || (is.numeric(boundary) && length(boundary) == 1))
  stopifnot(is.character(closed))
  
  closed <- match.arg(closed)
  
  if (!is.null(center) && !is.null(boundary)) {
    stop("Only one of 'center' and 'boundary' may be specified.")
  }

  if (length(na.omit(x)) == 0) {
    return(bin_out())
  }

  stopifnot(is.null(boundary) || (is.numeric(boundary) && length(boundary) == 1))
  stopifnot(is.character(closed))

  if (is.null(weight)) {
    weight <- rep(1, length(x))
  } else {
    weight[is.na(weight)] <- 0
  }

  params <- bin_params(range(x), width, center, boundary, closed)

  breaks <- seq(params$origin, max(x) + params$binwidth, params$binwidth)
  fuzzybreaks <- adjust_breaks(breaks, closed = params$closed)

  bins <- cut(x, fuzzybreaks, include.lowest = TRUE, right = params$closed == "right")
  left <- breaks[-length(breaks)]
  right <- breaks[-1]
  x <- (left + right) / 2
  bin_widths <- diff(breaks)

  count <- as.integer(tapply(weight, bins, sum, na.rm = TRUE))
  count[is.na(count)] <- 0L

  if (pad) {
    count <- c(0L, count, 0L)
    bin_widths <- c(params$binwidth, bin_widths, params$binwidth)
    x <- c(x[1] - params$binwidth, x, x[length(x)] + params$binwidth)
  }

  bin_out(count, x, bin_widths)
}

#' @rdname bin_vector
#' @export

bin_vector.POSIXct <- function(x, weight = NULL, ..., width = NULL,
                              center = NULL, boundary = NULL,
                              closed = c("right", "left"), pad=TRUE) {

  closed <- match.arg(closed)
  # Convert times to raw numbers (seconds since UNIX epoch), and call bin.numeric
  if (inherits(width, "Period")) width <- as.numeric(as.difftime(width, units = "secs"))
  if (!is.null(width)) width <- as.numeric(width)
  center <- if (!is.null(center)) center <- as.numeric(center)
  boundary <- if (!is.null(boundary)) boundary <- as.numeric(boundary)

  results <- bin_vector(as.numeric(x), weight = weight, width = width,
    center = center, boundary = boundary, closed = closed, pad=pad)

  # Convert some columns from numeric back to POSIXct objects
  tz <- attr(x, "tzone", TRUE)
  time_cols <- c("x_", "xmin_", "xmax_")
  results[time_cols] <- lapply(results[time_cols], function(col) {
    as.POSIXct(col, origin = "1970-01-01", tz = tz)
  })

  results
}

#' @export
bin_vector.Date <- function(x, weight = NULL, ..., width = NULL, center=NULL,
                            boundary = NULL, closed = c("right", "left"), pad = TRUE) {

  closed <- match.arg(closed)
  
  # Convert times to raw numbers, and call bin_vector.numeric

  if (!is.null(width))
    width <- as.numeric(width)
  if (!is.null(center))
    center<- as.numeric(center)
  if (!is.null(boundary))
    boundary <- as.numeric(boundary)

  results <- bin_vector(as.numeric(x), weight = weight,
                        width = width,
                        center=center, boundary = boundary,
                        closed = closed, pad = pad)

  # Convert some columns from numeric back to Date objects
  time_cols <- c("x_", "xmin_", "xmax_")
  results[time_cols] <- lapply(results[time_cols], function(col) {
    structure(col, class = "Date")
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
adjust_breaks <- function(breaks, closed = "right") {
  closed <- match.arg(closed, c("right", "left"))

  diddle <- 1e-08 * median(diff(breaks))
  if (closed == "right") {
    fuzz <- c(-diddle, rep.int(diddle, length(breaks) - 1))
  } else {
    fuzz <- c(rep.int(-diddle, length(breaks) - 1), diddle)
  }
  sort(breaks) + fuzz
}
