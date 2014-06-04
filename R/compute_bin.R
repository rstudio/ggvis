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
#' the range of the data, things will be shifted by an appropriat number of \code{width}s.
#' To center on integers,
#' for example, use \code{width = 1} and \code{center = 0}, even if \code{0} is
#' outside the range of the data.  At most one of \code{center} and \code{boundary} may be
#' specified.
#' @param boundary A boundary between two bins. As with \code{center}, things are shifted
#' when \code{boundary} is outside the range of the data. For example, to center on
#' integers, use \code{width = 1} and \code{boundary = 0.5}, even if \code{1} is outside
#' the range of the data.  At most one of \code{center} and \code{boundary} may be
#' specified.
#' @param right Should bins be right-open, left-closed, or
#'   right-closed, left-open.
#' @param pad If \code{TRUE}, adds empty bins at either end of x. This
#'   ensures frequency polygons touch 0, and adds padidng between the data
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
                        center=NULL, boundary = NULL,
                        right = TRUE, pad = TRUE) {
  UseMethod("compute_bin")
}

#' @export
compute_bin.data.frame <- function(x, x_var, w_var = NULL, width = NULL,
                                   center=NULL, boundary = NULL,
                                   right = TRUE, pad = TRUE) {
  assert_that(is.formula(x_var))

  x_val <- eval_vector(x, x_var)
  if (is.null(w_var)) {
    w_val <- NULL
  } else {
    w_val <- eval_vector(x, w_var)
  }

  params <- bin_params(range(x_val), width = width, center = center, boundary = boundary,
    right = right)

  bin_vector(x_val, weight = w_val, width = params$width, boundary = params$origin,
    right = params$right, pad = pad)
}

#' @export
compute_bin.grouped_df <- function(x, x_var, w_var = NULL, width = NULL,
                                   center = NULL, boundary = NULL,
                                   right = TRUE, pad = TRUE) {

  x_val <- eval_vector(x, x_var)
  params <- bin_params(range(x_val), width = width, center = center,
                       boundary = boundary, right = right)

  dplyr::do(x, compute_bin(.,
    x_var,
    w_var = w_var,
    width = params$width,
    boundary= params$origin,
    right = params$right,
    pad = pad))
}

#' @export
compute_bin.ggvis <- function(x, x_var, w_var = NULL, width = NULL,
                              center=NULL, boundary = NULL,
                              right = TRUE, pad = TRUE) {
  args <- list(x_var = x_var, w_var = w_var, width = width,
               center = center, boundary = boundary, right = right, pad = pad)

  register_computation(x, args, "bin", function(data, args) {
    output <- do_call(compute_bin, quote(data), .args = args)
    preserve_constants(data, output)
  })
}

# Compute parameters -----------------------------------------------------------

bin_params <- function(x_range, width = NULL, center = NULL, boundary = NULL,
                       right = TRUE) {
  UseMethod("bin_params")
}

# compute origin from x_range and width
tilelayer_origin <- function(x_range, width) {
  num_central_bins <- trunc(diff(x_range) / width) - 1
  side_width <- (diff(x_range) - num_central_bins * width) / 2  # width of partial tiles on either side
  if (side_width < .5 * width - 1e-8) {
    warning( paste0("side_width=", side_width, "; width=", width))
  }
  return(x_range[1] + side_width - width)
}

compute_origin <- function(x_range, width, boundary) {
  shift <- floor( (x_range[1] - boundary) / width )
  return(boundary + shift * width)
}

#' @export
bin_params.numeric <- function(x_range, width = NULL,
                               center=NULL,
                               boundary=NULL,
                               right = TRUE) {

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
    center <- boundary + width / 2
  }

  # if center given but not boundary, compute boundary from center
  if (is.null(boundary)) boundary <- center - width / 2

  origin <- compute_origin( x_range, width, boundary )

  list(width = width, origin = origin, right = right)
}

#' @export
bin_params.POSIXct <- function(x_range, width = NULL,
                               center = NULL, boundary = NULL,
                               right = TRUE) {

  if (!is.null(boundary) && !is.null(center)) {
    stop( "Only one of 'boundary' and 'center' may be specified." )
  }

  x_range <- as.numeric(x_range)
  if (inherits(width, "Period")) {
    width <- as.numeric(as.difftime(width, units="secs"))
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

  if (!is.numeric(width)) width <- as.numeric(width, units='secs')
  if (!is.null(center)) center <- as.numeric(center)
  if (!is.null(boundary)) boundary<- as.numeric(boundary)

  # if we have center but not boundary, compute boundary
  if (is.null(boundary)) boundary <- center - width / 2

  origin <- compute_origin( x_range, width, boundary )

  list(width = width, origin = origin, right = right,
       origin.POSIX=structure(origin, class=c("POSIXct", "POSIXt"))
  )
}

#' @export
bin_params.integer <- function(x_range, width = NULL,
                               center = NULL, boundary= NULL,
                               right = TRUE) {

  if (!is.null(boundary) && !is.null(center)) {
    stop( "Only one of 'boundary' and 'center' may be specified." )
  }

  if (is.null(width)) {
    width <- min(pretty(round(diff(x_range) / 25)))
    if (width <= 2) width = 1
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

  list(width = width, origin = origin, right = right)
}

# Bin individual vector --------------------------------------------------------

bin_vector <- function(x, weight = NULL, ...) {
  UseMethod("bin_vector")
}

#' @export
bin_vector.numeric <- function(x, weight = NULL, ..., width = 1,
                               center = NULL, boundary = NULL,
                               right = TRUE, pad = TRUE) {
  stopifnot(is.numeric(width) && length(width) == 1)
  stopifnot(is.null(center) || (is.numeric(center) && length(center) == 1))
  stopifnot(is.null(boundary) || (is.numeric(boundary) && length(boundary) == 1))
  stopifnot(is.flag(right))
  if (!is.null(center) && !is.null(boundary)) {
    stop("Only one of 'center' and 'bouncary' may be specified.")
  }

  if (length(na.omit(x)) == 0) {
    return(bin_out())
  }

  if (is.null(weight)) {
    weight <- rep(1, length(x))
  } else {
    weight[is.na(weight)] <- 0
  }

  if (is.null(boundary) && is.null(center)) {
    boundary <- tilelayer_origin(range(x), width)
    center <- boundary + width / 2
  }
  params <- bin_params( range(x), width, center, boundary, right)

  breaks <- seq(params$origin, max(x) + params$width, params$width)
  fuzzybreaks <- adjust_breaks(breaks, open = if (params$right) "right" else "left")

  bins <- cut(x, fuzzybreaks, include.lowest = TRUE, right = params$right)
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

  bin_out(count, x, bin_widths)
}

#' @export
bin_vector.POSIXt <- function(x, weight = NULL, ..., width = NULL,
                              center = NULL, boundary = NULL,
                              right = TRUE) {
  # Convert times to raw numbers (seconds since UNIX epoch), and call bin.numeric
  center <- if (!is.null(center)) center <- as.numeric(center)
  boundary <- if (!is.null(boundary)) boundary <- as.numeric(boundary)
  if (inherits(width, "Period")) width <- as.numeric(as.difftime(width, units="secs"))
  width <- if (!is.null(width)) width <- as.numeric(width)

  results <- bin_vector(as.numeric(x), weight = weight, width = width,
    center = center, boundary = boundary, right = right)

  # Convert some columns from numeric back to POSIXct objects
  time_cols <- c("x_", "xmin_", "xmax_")
  results[time_cols] <- lapply(results[time_cols], function(col) {
    structure(col, class = c("POSIXct", "POSIXt"))
  })

  results
}

#' @export
bin_vector.default <- function(x, weight = NULL, ...) {
  stop("Don't know how to bin vector of type ", class(x))
}

bin_out <- function(count = numeric(0), x = numeric(0), width = numeric(0),
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
