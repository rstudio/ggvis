#' Bin data along a continuous variable
#'
#' @param x Dataset-like object to bin. Built-in methods for data frames,
#'   grouped data frames and ggvis visualisations.
#' @param x_var,w_var Names of x and weight variables. The x variable must be
#'   continuous.
#' @param binwidth The width of the bins. The default is \code{NULL}, which
#'   yields 30 bins that cover the range of the data. You should always override
#'   this value, exploring multiple widths to find the best to illustrate the
#'   stories in your data.
#' @param center The center of one of the bins.  Note that if center is above or below
#'   the range of the data, things will be shifted by an appropriate number of \code{binwidth}s.
#'   To center on integers,
#'   for example, use \code{binwidth = 1} and \code{center = 0}, even if \code{0} is
#'   outside the range of the data.  At most one of \code{center} and \code{boundary} may be
#'   specified.
#' @param boundary A boundary between two bins. As with \code{center}, things are shifted
#'   when \code{boundary} is outside the range of the data. For example, to center on
#'   integers, use \code{binwidth = 1} and \code{boundary = 0.5}, even if \code{1} is outside
#'   the range of the data.  At most one of \code{center} and \code{boundary} may be
#'   specified.
#' @param right Should bins be right-open, left-closed, or
#'   right-closed, left-open.
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
#' mtcars %>% compute_bin(~mpg, binwidth = 10)
#' mtcars %>% group_by(cyl) %>% compute_bin(~mpg, binwidth = 10)
#'
#' # It doesn't matter whether you transform inside or outside of a vis
#' mtcars %>% compute_bin(~mpg) %>% ggvis(~x_, ~count_) %>% layer_paths()
#' mtcars %>% ggvis(~ x_, ~ count_) %>% compute_bin(~mpg) %>% layer_paths()
compute_bin <- function(x, x_var, w_var = NULL, binwidth = NULL,
                        center = NULL, boundary = NULL,
                        right = TRUE, pad = TRUE) {
  UseMethod("compute_bin")
}

#' @export
compute_bin.data.frame <- function(x, x_var, w_var = NULL, binwidth = NULL,
                                   center = NULL, boundary = NULL,
                                   right = TRUE, pad = TRUE) {
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

  params <- bin_params(range2(x_val), binwidth = binwidth, center = center,
                       boundary = boundary, right = right)

  # note: origin is a boundary, so this works.
  bin_vector(x_val, weight = w_val, binwidth = params$binwidth, boundary = params$origin,
    right = params$right, pad = pad)
}

#' @export
compute_bin.grouped_df <- function(x, x_var, w_var = NULL, binwidth = NULL,
                                   center = NULL, boundary = NULL,
                                   right = TRUE, pad = TRUE) {

  x_val <- eval_vector(x, x_var)
  params <- bin_params(range2(x_val), binwidth = binwidth, center = center,
                       boundary = boundary, right = right)

  dplyr::do(x, compute_bin(.,
    x_var,
    w_var = w_var,
   binwidth = params$binwidth,
    boundary = params$origin,   # origin is a boundary, so this works
    right = params$right,
    pad = pad))
}

#' @export
compute_bin.ggvis <- function(x, x_var, w_var = NULL, binwidth = NULL,
                              center = NULL, boundary = NULL,
                              right = TRUE, pad = TRUE) {
  args <- list(x_var = x_var, w_var = w_var, binwidth = binwidth,
               center = center, boundary = boundary, right = right, pad = pad)

  register_computation(x, args, "bin", function(data, args) {
    output <- do_call(compute_bin, quote(data), .args = args)
    preserve_constants(data, output)
  })
}

# Compute parameters -----------------------------------------------------------

bin_params <- function(x_range, binwidth = NULL, center = NULL, boundary = NULL,
                       right = TRUE) {
  UseMethod("bin_params")
}

# compute origin from x_range and binwidth
tilelayer_origin <- function(x_range, binwidth) {
  stopifnot(is.numeric(x_range) && length(x_range) == 2)
  stopifnot(is.numeric(binwidth) && length(binwidth) == 1)
  num_central_bins <- trunc(diff(x_range) / binwidth) - 1
  side_width <- (diff(x_range) - num_central_bins * binwidth) / 2  # binwidth of partial tiles on either side
  x_range[1] + side_width - binwidth
  # adjust_breaks should be called to handle any round-off fuzziness issues
}

compute_origin <- function(x_range, binwidth, boundary) {
  shift <- floor( (x_range[1] - boundary) / binwidth )
  boundary + shift * binwidth
}

#' @export
bin_params.numeric <- function(x_range, binwidth = NULL,
                               center = NULL,
                               boundary = NULL,
                               right = TRUE) {
  stopifnot(is.numeric(x_range) && length(x_range) == 2)
  if (!is.null(boundary) && !is.null(center)) {
    stop( "Only one of 'boundary' and 'center' may be specified." )
  }

  if (is.null(binwidth)) {
    binwidth <- diff(x_range) / 30
    notify_guess(binwidth, "range / 30")
  }

  # if neither edge nor center given, compute both using tile layer's algorithm
  # this puts min and max of data in outer half of their bins.

  if (is.null(boundary) && is.null(center)) {
    boundary <- tilelayer_origin(x_range, binwidth)
    # center <- boundary + binwidth / 2
  }

  # if center given but not boundary, compute boundary from center
  if (is.null(boundary)) boundary <- center - binwidth / 2

  origin <- compute_origin( x_range, binwidth, boundary )

  list(binwidth = binwidth, origin = origin, right = right)
}

#' @export
bin_params.POSIXct <- function(x_range, binwidth = NULL,
                               center = NULL, boundary = NULL,
                               right = TRUE) {

  if (!is.null(boundary) && !is.null(center)) {
    stop( "Only one of 'boundary' and 'center' may be specified." )
  }

  x_range <- as.numeric(x_range)
  if (inherits(binwidth, "Period")) {
    binwidth <- as.numeric(as.difftime(binwidth, units = "secs"))
  }
  if (!is.null(binwidth)) binwidth <- as.numeric(binwidth)
  if (!is.null(center)) binwidth <- as.numeric(center)
  if (!is.null(boundary)) binwidth <- as.numeric(boundary)

  if (is.null(binwidth)) {
    binwidth <- diff(x_range) / 30
    notify_guess(binwidth, "range / 30 (in seconds)")
    binwidth <- as.numeric(binwidth,  units = "secs")
  }

  if (is.null(boundary) && is.null(center)) {
    boundary <- tilelayer_origin(x_range, binwidth)
    center <- boundary + binwidth / 2
  }

  if (!is.numeric(binwidth)) binwidth <- as.numeric(binwidth, units = 'secs')
  if (!is.null(center)) center <- as.numeric(center)
  if (!is.null(boundary)) boundary<- as.numeric(boundary)

  # if we have center but not boundary, compute boundary
  if (is.null(boundary)) boundary <- center - binwidth / 2

  origin <- compute_origin( x_range, binwidth, boundary )

  list(binwidth = binwidth, origin = origin, right = right,
       origin.POSIX = structure(origin, class = c("POSIXct", "POSIXt"))
  )
}

### check on this -- get center in instead of origin
#' @export
bin_params.Date <- function(x_range, binwidth = NULL, center = NULL,
                            boundary = NULL, right = TRUE) {

  # convert things to numeric as we go along

  x_range <- as.numeric(x_range)

  if (is.null(binwidth)) {
    binwidth <- diff(x_range) / 30
    notify_guess(binwidth, "range / 30")
  }
  binwidth <- as.numeric(binwidth)

  if (is.null(boundary) && is.null(center)) {
    boundary <- tilelayer_origin(x_range, binwidth)
    center <- boundary + binwidth / 2
  }

  # if we have center but not boundary, compute boundary
  if (is.null(boundary)) {
    center <- as.numeric(center)
    boundary <- center - binwidth / 2
  }

  origin <- compute_origin( x_range, binwidth, boundary )

  # do we need to convert this back to date format?

  list(binwidth = binwidth, origin = origin, right = right)
}


#' @export
bin_params.integer <- function(x_range, binwidth = NULL,
                               center = NULL, boundary = NULL,
                               right = TRUE) {

  if (!is.null(boundary) && !is.null(center)) {
    stop( "Only one of 'boundary' and 'center' may be specified." )
  }

  if (is.null(binwidth)) {
    binwidth <- max(pretty(round(diff(x_range) / 30)))
    if (binwidth <= 1) binwidth <- 1
    num_bins <- ceiling( diff(x_range) / binwidth )
    notify_guess(binwidth, paste0("approximately range/", num_bins) )
  }

  if (is.null(boundary) && is.null(center)) {
    boundary <- tilelayer_origin(x_range, binwidth)
    center <- boundary + binwidth / 2
  }

  # if we have center but not boundary, compute boundary
  if (is.null(boundary)) boundary <- center - binwidth / 2

  origin <- compute_origin( x_range, binwidth, boundary )

  list(binwidth = binwidth, origin = origin, right = right)
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
#' @param binwidth the width of a bin
#' @param center the center of a bin
#' @param boundary the boundary of a bin.  \code{center} and \code{boundary} should
#'   not both be specified.
#' @param right a logical indicating whether the right boundary of a bin is
#'   included with the bin.
#' @param pad a logical indicatign whether the bins should be padded to include
#'   an empty bin on each side.
#' @param ... additional arguments passed through to instances of the generic
bin_vector <- function(x, weight = NULL, ...) {
  UseMethod("bin_vector")
}

#' @rdname bin_vector
#' @export

bin_vector.numeric <- function(x, weight = NULL, ..., binwidth = NULL,
                               center = NULL, boundary = NULL,
                               right = TRUE, pad = TRUE) {
  stopifnot(is.null(binwidth) || (is.numeric(binwidth) && length(binwidth) == 1))
  stopifnot(is.null(center) || (is.numeric(center) && length(center) == 1))
  stopifnot(is.null(boundary) || (is.numeric(boundary) && length(boundary) == 1))
  stopifnot(is.flag(right))
  if (!is.null(center) && !is.null(boundary)) {
    stop("Only one of 'center' and 'bouncary' may be specified.")
  }

  if (length(na.omit(x)) == 0) {
    return(bin_out())
  }

  stopifnot(is.numeric(binwidth) && length(binwidth) == 1)
  stopifnot(is.null(boundary) || (is.numeric(boundary) && length(boundary) == 1))
  stopifnot(is.flag(right))

  if (is.null(weight)) {
    weight <- rep(1, length(x))
  } else {
    weight[is.na(weight)] <- 0
  }


  params <- bin_params( range(x), binwidth, center, boundary, right)

  breaks <- seq(params$origin, max(x) + params$binwidth, params$binwidth)
  fuzzybreaks <- adjust_breaks(breaks, open = if (params$right) "right" else "left")

  bins <- cut(x, fuzzybreaks, include.lowest = TRUE, right = params$right)
  left <- breaks[-length(breaks)]
  right <- breaks[-1]
  x <- (left + right) / 2
  bin_widths <- diff(breaks)

  count <- as.integer(tapply(weight, bins, sum, na.rm = TRUE))
  count[is.na(count)] <- 0L

  if (pad) {
    count <- c(0L, count, 0L)
    bin_widths <- c(binwidth, bin_widths, binwidth)
    x <- c(x[1] - binwidth, x, x[length(x)] + binwidth)
  }

  bin_out(count, x, bin_widths)
}

#' @rdname bin_vector
#' @export
## binwidth=1 by default? NULL?
bin_vector.POSIXt <- function(x, weight = NULL, ..., binwidth = NULL,
                              center = NULL, boundary = NULL,
                              right = TRUE, pad=TRUE) {

  # Convert times to raw numbers (seconds since UNIX epoch), and call bin.numeric
  if (inherits(binwidth, "Period")) binwidth <- as.numeric(as.difftime(binwidth, units = "secs"))
  if (!is.null(binwidth)) binwidth <- as.numeric(binwidth)
  center <- if (!is.null(center)) center <- as.numeric(center)
  boundary <- if (!is.null(boundary)) boundary <- as.numeric(boundary)

  results <- bin_vector(as.numeric(x), weight = weight, binwidth = binwidth,
    center = center, boundary = boundary, right = right, pad=pad)

  # Convert some columns from numeric back to POSIXct objects
  tz <- attr(x, "tzone", TRUE)
  time_cols <- c("x_", "xmin_", "xmax_")
  results[time_cols] <- lapply(results[time_cols], function(col) {
    as.POSIXct(col, origin = "1970-01-01", tz = tz)
  })

  results
}

#' @export
bin_vector.Date <- function(x, weight = NULL, ..., binwidth = NULL, center=NULL,
                            boundary = NULL, right = TRUE, pad = TRUE) {

  # Convert times to raw numbers, and call bin_vector.numeric

  if (!is.null(binwidth))
    binwidth <- as.numeric(binwidth)
  if (!is.null(center))
    center<- as.numeric(center)
  if (!is.null(boundary))
    boundary <- as.numeric(boundary)

  results <- bin_vector(as.numeric(x), weight = weight,
                        binwidth = binwidth,
                        center=center, boundary = boundary,
                        right = right, pad = pad)

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
adjust_breaks <- function(breaks, open = "right") {
  open <- match.arg(open, c("left", "right"))

  diddle <- 1e-08 * median(diff(breaks))
  if (open == "left") {
    fuzz <- c(-diddle, rep.int(diddle, length(breaks) - 1))
  } else {
    fuzz <- c(rep.int(-diddle, length(breaks) - 1), diddle)
  }
  sort(breaks) + fuzz
}
