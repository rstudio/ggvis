#' Transformation: bin continuous variable.
#'
#' \code{transform_bin} is a data transformation that reduces a one-d vector
#' of positions to a data frame of binned counts. \code{layer_histogram}
#' combines \code{transform_bin} with \code{mark_rect} to create a histogram,
#' and \code{layer_freqpoly} combines \code{transform_bin} with
#' \code{mark_path} to create a frequency polygon.
#'
#' @section Input:
#' Currently \code{transform_bin} only works with data frames and requires the
#' following properties:
#'
#' \itemize{
#'   \item \code{x}, numeric, the values to bin and count
#' }
#'
#' @section Ouput:
#'
#' \code{transform_bin} creates a data frame with columns:
#'
#' \itemize{
#'  \item \code{count__}: the number of points
#'  \item \code{x}: mid-point of bin
#'  \item \code{xmin__}: left boundary of bin
#'  \item \code{xmax__}: right boundary of bin
#'  \item \code{width__}: width of bin
#' }
#'
#' @param binwidth The width of the bins. The default is \code{guess()}, which
#'   yields 30 bins that cover the range of the data. You should always override
#'   this value, exploring multiple widths to find the best to illustrate the
#'   stories in your data.
#' @param origin The initial position of the left-most bin. If \code{NULL}, the
#'   the default, will use the smallest value in the dataset.
#' @param right Should bins be right-open, left-closed, or
#'   right-closed, left-open
#' @param ... For \code{transform_bin}: ignored, all transforms must use
#'   named arguments.  For \code{layer_histogram}: named arguments are
#'   passed on to the transform, unnamed arguments are passed on to the layer.
#' @export
#' @examples
#' # Create histograms and frequency polygons with layers
#' ggvis(mtcars, props(x = ~mpg), layer_histogram())
#' ggvis(mtcars, props(x = ~mpg), layer_histogram(binwidth = 2))
#' ggvis(mtcars, props(x = ~mpg), layer_freqpoly(binwidth = 2))
#'
#' # These are equivalent to combining transform_bin with the corresponding
#' # mark
#' ggvis(mtcars, props(x = ~mpg), transform_bin(binwidth = 2),
#'   mark_rect(props(x = ~xmin__, x2 = ~xmax__, y = ~count__, y2 = 0)),
#'   mark_path(props(x = ~x, y = ~count__, stroke := "red", strokeWidth := 4))
#' )
#'
#' # You can also combine other data transformations like splitting
#' ggvis(mtcars, props(x = ~mpg, stroke = ~cyl, strokeWidth := 4),
#'    by_group(cyl), layer_freqpoly(binwidth = 2))
#'
#' # You can see the results of a transformation by creating your own pipeline
#' # and flowing data through it
#' sluice(pipeline(mtcars, transform_bin(2)), props(x = ~mpg))
#' sluice(pipeline(mtcars, by_group(cyl), transform_bin(2)), props(x = ~disp))
#' # Or
#' pl <- pipeline(mtcars, transform_bin(10))
#' sluice(pl, props(x = ~disp))
transform_bin <- function(vis, ..., binwidth = guess(), origin = NULL,
                          right = TRUE) {
  if (!is.ggvis(vis)) stop("First argument to transform must be a ggvis object.")

  dots <- list(...)
  dots <- dots[named(dots)]

  # Get the current data and props from the parent
  parent_data <- vis$cur_data
  parent_props <- vis$cur_props

  new_data <- reactive({
    data <- parent_data()

    check_prop("transform_bin", parent_props, data, "x.update",
      c("numeric", "datetime", "ordinal", "nominal"))

    range <- prop_range(data, parent_props$x)
    params <- bin_params(range, binwidth = binwidth, origin = origin,
                         right = right)

    output <- bin(data, x_var = parent_props$x, binwidth = params$binwidth,
                  origin = params$origin, right = params$right)

    # TODO: Check for zero-row output for other data types
    if (is.data.frame(output) && nrow(output) == 0) return(output)

    preserve_constants(data, output)
  })

  # Save data in the vis object, updating current data.
  register_data(vis,
    new_data,
    prefix = paste0(get_data_id(parent_data), "_transform_sort"),
    update_current = TRUE
  )
}

#' @rdname transform_bin
#' @export
layer_histogram <- function(vis, ...) {
  vis %>%
    transform_bin(...) %>%
    mark_rect(props(x = ~xmin__, x2 = ~xmax__, y = ~count__, y2 = 0))
}

#' @rdname transform_bin
#' @export
#' @inheritParams layer_histogram
layer_freqpoly <- function(vis, ...) {
  vis %>%
    transform_bin(...) %>%
    mark_path(props(x = ~x, y = ~count__))
}

#' @rdname transform_bin
#' @export
layer_barchart <- function(vis, ...) {
  vis %>%
    transform_bin(...) %>%
    mark_rect(props(x = ~x, width = band(mult = 0.9), y2 = ~count__, y = 0))
}

# Bin complete dataset ---------------------------------------------------------

bin <- function(data, ...) UseMethod("bin")

#' @export
bin.split_df <- function(data, x_var, ...) {
  data[] <- lapply(data, bin, x_var = x_var, ...)
  data
}

#' @export
bin.data.frame <- function(data, x_var, ...) {
  x_val <- remove_missing(prop_value(x_var, data))
  bin_vector(x_val, ...)
}

# Compute parameters -----------------------------------------------------------

bin_params <- function(x_range, binwidth = guess(), origin = guess(), right = TRUE) {
  UseMethod("bin_params")
}

#' @export
bin_params.numeric <- function(x_range, binwidth = guess(), origin = guess(),
                               right = TRUE) {

  if (is.guess(binwidth)) {
    binwidth <- diff(x_range) / 30
    message("Guess: transform_bin(binwidth = ", format(binwidth, digits = 3),
      ") # range / 30")
  }

  list(binwidth = binwidth, origin = origin, right = right)
}

#' @export
bin_params.POSIXct <- function(x_range, binwidth = guess(), origin = guess(),
                               right = TRUE) {

  if (is.guess(binwidth)) {
    binwidth <- as.numeric(diff(x_range) / 30, units = "secs")
    message("Guess: transform_bin(binwidth = ", format(binwidth, digits = 3),
      ") # range / 30")
  }

  list(binwidth = binwidth, origin = origin, right = right)
}

#' @export
bin_params.integer <- function(x_range, binwidth = guess(), origin = guess(),
                               right = TRUE) {
  if (is.guess(binwidth)) {
    binwidth <- 1
    origin <- x_range[1] - 1/2
    message("Guess: transform_bin(binwidth = 1)")
  }

  list(binwidth = binwidth, origin = origin, right = right)
}

#' @export
bin_params.character <- function(x_range, binwidth = guess(), origin = guess(),
                                 right = TRUE) {

  list()
}
#' @export
bin_params.factor <- bin_params.character

# Bin individual vector --------------------------------------------------------

bin_vector <- function(x, weight = NULL, ...) {
  UseMethod("bin_vector")
}

#' @export
bin_vector.numeric <- function(x, weight = NULL, ..., binwidth = 1,
                               origin = NULL, right = TRUE) {
  stopifnot(is.numeric(binwidth) && length(binwidth) == 1)
  stopifnot(is.null(origin) || (is.numeric(origin) && length(origin) == 1))
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
    breaks <- fullseq(range(x), binwidth, pad = TRUE)
  } else {
    breaks <- seq(origin, max(range(x)) + binwidth, binwidth)
  }
  fuzzybreaks <- adjust_breaks(breaks, open = if (right) "right" else "left")

  bins <- cut(x, fuzzybreaks, include.lowest = TRUE, right = right)
  left <- breaks[-length(breaks)]
  right <- breaks[-1]
  x <- (left + right)/2
  width <- diff(breaks)

  count <- as.numeric(tapply(weight, bins, sum, na.rm = TRUE))
  count[is.na(count)] <- 0

  bin_out(count, x, width)
}

#' @export
bin_vector.POSIXt <- function(x, weight = NULL, ..., binwidth = 1,
                              origin = NULL, right = TRUE) {
  # Convert times to raw numbers (seconds since UNIX epoch), and call bin.numeric
  results <- bin(as.numeric(x), weight, binwidth, origin, right)

  # Convert some columns from numeric back to POSIXct objects
  time_cols <- c("x", "xmin__", "xmax__")
  results[time_cols] <- lapply(results[time_cols], function(col) {
    structure(col, class = c("POSIXct", "POSIXt"))
  })

  results
}

#' @export
bin_vector.factor <- function(x, weight = NULL, ...) {
  tbl <- table(x, exclude = NULL)
  df <- as.data.frame(tbl)

  data.frame(
    count__ = df[[2]],
    x = as.character(df[[1]]),
    stringsAsFactors = FALSE
  )
}

bin_out <- function(count = numeric(0), x = numeric(0), width = numeric(0), xmin = x - width / 2, xmax = x + width / 2) {
  data.frame(
    count__ = count,
    x = x,
    xmin__ = xmin,
    xmax__ = xmax,
    width__ = width,
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
