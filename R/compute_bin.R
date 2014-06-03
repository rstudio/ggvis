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
#' @param bincenter The center of one of the bins.  This is used to calculate \code{origin}
#' if it is not specified.  Note that if center is above or below the range of the data, \code{origin}
#' will be shifted by an appropriat number of \code{binwidth}s.  To center on integers,
#' for example, use \code{binwidth = 1} and \code{bincenter = 0}, even if \code{0} is
#' outside the range of the data.
#' @param origin The initial position of the left-most bin. If \code{NULL}, the
#'   the default, will use the smallest value in the dataset.
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
#' mtcars %>% compute_bin(~mpg, binwidth = 10)
#' mtcars %>% group_by(cyl) %>% compute_bin(~mpg, binwidth = 10)
#'
#' # It doesn't matter whether you transform inside or outside of a vis
#' mtcars %>% compute_bin(~mpg) %>% ggvis(~x_, ~count_) %>% layer_paths()
#' mtcars %>% ggvis(~ x_, ~ count_) %>% compute_bin(~mpg) %>% layer_paths()
compute_bin <- function(x, x_var, w_var = NULL, binwidth = NULL,
                        bincenter=NULL,
                        origin = NULL, right = TRUE, pad = TRUE) {
  UseMethod("compute_bin")
}

#' @export
compute_bin.data.frame <- function(x, x_var, w_var = NULL, binwidth = NULL,
                                   bincenter=NULL,
                                   origin = NULL, right = TRUE, pad = TRUE) {
  assert_that(is.formula(x_var))

  x_val <- eval_vector(x, x_var)
  if (is.null(w_var)) {
    w_val <- NULL
  } else {
    w_val <- eval_vector(x, w_var)
  }

  params <- bin_params(range(x_val), binwidth = binwidth, bincenter = bincenter, origin = origin,
    right = right)

  bin_vector(x_val, weight = w_val, binwidth = params$binwidth, bincenter = params$bincenter,
    origin = params$origin, right = params$right, pad = pad)
}

#' @export
compute_bin.grouped_df <- function(x, x_var, w_var = NULL, binwidth = NULL,
                                   bincenter=NULL,
                                   origin = NULL, right = TRUE, pad = TRUE) {

  x_val <- eval_vector(x, x_var)
  params <- bin_params(range(x_val), binwidth = binwidth, bincenter = bincenter,
                       origin = origin, right = right)

  dplyr::do(x, compute_bin(.,
    x_var,
    w_var = w_var,
    binwidth = params$binwidth,
    bincenter = params$bincenter,
    origin = params$origin,
    right = params$right,
    pad = pad))
}

#' @export
compute_bin.ggvis <- function(x, x_var, w_var = NULL, binwidth = NULL,
                              bincenter=NULL,
                              origin = NULL, right = TRUE, pad = TRUE) {
  args <- list(x_var = x_var, w_var = w_var, binwidth = binwidth, bincenter = bincenter,
    origin = origin, right = right, pad = pad)

  register_computation(x, args, "bin", function(data, args) {
    output <- do_call(compute_bin, quote(data), .args = args)
    preserve_constants(data, output)
  })
}

# Compute parameters -----------------------------------------------------------

bin_params <- function(x_range, binwidth = NULL, bincenter = NULL, origin = NULL,
                       right = TRUE) {
  UseMethod("bin_params")
}

#' @export
bin_params.numeric <- function(x_range, binwidth = NULL,
                               bincenter=NULL,
                               origin = NULL,
                               right = TRUE) {

  if (is.null(binwidth)) {
    binwidth <- diff(x_range) / 30
    notify_guess(binwidth, "range / 30")
  }


  if (is.null(origin)) {
    if (is.null(bincenter)) {
      origin <- round_any(x_range[1], binwidth, floor)
    } else {
      shift <-  floor( (x_range[1] - bincenter) / binwidth )
      origin <-  bincenter + shift * binwidth - .5 * binwidth
    }
  }


  list(binwidth = binwidth, bincenter = bincenter, origin = origin, right = right)
}

#' @export
bin_params.POSIXct <- function(x_range, binwidth = NULL,
                               bincenter = NULL,
                               origin = NULL,
                               right = TRUE) {

  if (is.null(binwidth)) {
    binwidth <- diff(x_range) / 30
    notify_guess(binwidth, "range / 30")
    binwidth <- as.numeric(binwidth,  units = "secs")
  }

  if (inherits(binwidth, "Period") && require(lubridate)) {
    binwidth <- as.numeric(lubridate::as.difftime(binwidth, units="secs"))
  }
  if (!is.numeric(binwidth)) binwidth <- as.numeric(binwidth, units='secs')
  if (!is.null(bincenter)) bincenter <- as.numeric(bincenter)
  if (!is.null(origin)) origin<- as.numeric(origin)

  list(binwidth = binwidth, bincenter = bincenter, origin = origin, right = right)
}

#' @export
bin_params.integer <- function(x_range, binwidth = NULL, bincenter = NULL,
                               origin = NULL,
                               right = TRUE) {

  if (is.null(binwidth)) {
    binwidth <- min( pretty( round(diff(x_range) / 25) ))
    if (binwidth <= 2) binwidth = 1
    num_bins <- ceiling( diff(x_range) / binwidth )
    notify_guess(binwidth, paste0("approximately range/", num_bins) )
  }

  if (is.null(origin)) {
    if (is.null(bincenter)) {
      origin <- x_range[1] - 1/2
    } else {
      shift <-  floor( (x_range[1] - bincenter) / binwidth )
      origin <-  bincenter + shift * binwidth - .5 * binwidth
    }
  }

  list(binwidth = binwidth, bincenter=bincenter, origin = origin, right = right)
}

# Bin individual vector --------------------------------------------------------

bin_vector <- function(x, weight = NULL, ...) {
  UseMethod("bin_vector")
}

#' @export
bin_vector.numeric <- function(x, weight = NULL, ..., binwidth = 1, bincenter=NULL,
                               origin = NULL, right = TRUE, pad = TRUE) {
  stopifnot(is.numeric(binwidth) && length(binwidth) == 1)
  stopifnot(is.null(origin) || (is.numeric(origin) && length(origin) == 1))
  stopifnot(is.null(bincenter) || (is.numeric(bincenter) && length(bincenter) == 1))
  stopifnot(is.flag(right))

  if (length(na.omit(x)) == 0) {
    return(bin_out())
  }

  if (is.null(weight)) {
    weight <- rep(1, length(x))
  } else {
    weight[is.na(weight)] <- 0
  }

  if (is.null(origin)) {
    if (!is.null(bincenter)) {
	  # c = center; . = bin edges; o = origin (a bin edge); m=min(data)
	  # .     .  c  .     .     .     o     . m   .     .     .
	  # 
	  # in example, (m-c)/w = 4.6, so o = c + 3.5 w 
	  # could adjust this so that o is one bin farther right.

	  # how many binwidths from center to min(data)?
      shift <-  floor( (min(x) - bincenter)/binwidth )
      # set origin near left edge of data -- at left side of a bin
      origin <-  bincenter + shift * binwidth -.5 * binwidth
    } else {
      origin <- round_any(min(x), binwidth, floor)
    }
  }

  breaks <- seq(origin, max(x) + binwidth, binwidth)
  fuzzybreaks <- adjust_breaks(breaks, open = if (right) "right" else "left")

  bins <- cut(x, fuzzybreaks, include.lowest = TRUE, right = right)
  left <- breaks[-length(breaks)]
  right <- breaks[-1]
  x <- (left + right)/2
  width <- diff(breaks)

  count <- as.numeric(tapply(weight, bins, sum, na.rm = TRUE))
  count[is.na(count)] <- 0

  if (pad) {
    count <- c(0, count, 0)
    width <- c(binwidth, width, binwidth)
    x <- c(x[1] - binwidth, x, x[length(x)] + binwidth)
  }

  bin_out(count, x, width)
}

#' @export
bin_vector.POSIXt <- function(x, weight = NULL, ..., binwidth = 1, bincenter = NULL,
                              origin = NULL, right = TRUE) {
  # Convert times to raw numbers (seconds since UNIX epoch), and call bin.numeric
  bincenter <- if (!is.null(bincenter)) bincenter <- as.numeric(bincenter)
  origin <- if (!is.null(origin)) origin <- as.numeric(origin)

  results <- bin_vector(as.numeric(x), weight = weight, binwidth = binwidth,
    bincenter = bincenter, origin = origin, right = right)

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
