#' @export
transform_bin <- function(binwidth = "auto") {
  transform("bin", binwidth = binwidth)
}


#' @S3method compute transform_bin
compute.transform_bin <- function(transform, data, mapping) {
  xvar <- mapping["x"]

  if (transform$binwidth == "auto") {
    message("transform_bin: binwidth defaulted to range/30. To adjust, use 'binwidth = n'.")
    binwidth <- diff(range(data[[xvar]])) / 30
  } else {
    binwidth = transform$binwidth
  }

  # TODO: implement weight, origin, right
  results <- bin(data[[xvar]], weight = NULL, binwidth = binwidth)

  names(results)[names(results) == "x"] <- xvar
  results
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
