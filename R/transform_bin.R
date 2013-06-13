#' @export
transform_bin <- function(binwidth = "auto") {
  transform("bin", binwidth = binwidth)
}


#' @S3method apply_transform transform_bin
apply_transform.transform_bin <- function(transform, data, mapping) {
  xvar <- mapping["x"]

  # Find the bindwidth; this can handle different types for `data`, since
  # find_var_range() dispatches on data's class.
  if (transform$binwidth == "auto") {
    message("transform_bin: binwidth defaulted to range/30. To adjust, use 'binwidth = n'.")
    transform$binwidth <- diff(find_var_range(data, xvar)) / 30
  }

  # We've dispatched on transform type, now dispatch on data type
  compute_transform_bin(data, transform, mapping)
}


compute_transform_bin <- function(data, transform, mapping)
  UseMethod("compute_transform_bin")

#' @S3method compute_transform_bin default
compute_transform_bin.default <- function(data, transform, mapping) {
  stop("Don't know how to compute_transform_bin for data structure with class ",
    paste(class(data), sep = ", "))
}

#' @S3method compute_transform_bin data.frame
compute_transform_bin.data.frame <- function(data, transform, mapping) {
  xvar <- mapping["x"]

  # Identify constant variables, extract and add back in
  constant_vars <- vapply(data, is_constant, logical(1))

  # Do the binning
  # TODO: implement weight, origin, right
  transformed <- bin(data[[xvar]], weight = NULL, binwidth = transform$binwidth)
  names(transformed)[names(transformed) == "x"] <- xvar

  # Add back the constant variables
  carry_over <- data[1, constant_vars, drop = FALSE]
  rownames(carry_over) <- NULL
  cbind(transformed, carry_over)
}

#' @S3method compute_transform_bin split_data_dflist
compute_transform_bin.split_data_dflist <- function(data, transform, mapping) {
  # Run compute_transform_bin on each data frame in the list
  data <- structure(
    lapply(data, compute_transform_bin, transform = transform, mapping = mapping),
    class = c("split_data_dflist", "split_data")
  )
}


bin <- function(x, weight = NULL, binwidth = 1, origin = NULL, right = TRUE) {
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

  results <- data.frame(
    count__ = as.numeric(tapply(weight, bins, sum, na.rm=TRUE)),
    x = x,
    xmin__ = x - width/2,
    xmax__ = x + width/2,
    width__ = width
  )

  results
}
