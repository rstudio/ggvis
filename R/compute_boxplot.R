#' Calculate boxplot values
#'
#' @param x Dataset-like object to compute boxplot values. There are built-in
#'   methods for data frames, grouped data frames, and ggvis visualisations.
#' @param var Name of variable for which to compute boxplot values. The variable
#'   must be continuous.
#' @param coef The maximum length of the whiskers as multiple of the
#'   inter-quartile range. Default value is 1.5.
#' @seealso \code{\link{layer_boxplots}}
#' @return A data frame with columns:
#'   \item{min_}{Lower whisker = smallest observation greater than or equal to lower hinge - 1.5 * IQR}
#'   \item{lower_}{Lower hinge (25th percentile)}
#'   \item{median_}{Median (50th percentile)}
#'   \item{upper_}{Upper hinge (75th percentile)}
#'   \item{max_}{Upper whisker = largest observation less than or equal to upper hinge + 1.5 * IQR}
#'   \item{outliers_}{A vector of values that are outside of the min and max}
#' @examples
#' mtcars %>% compute_boxplot(~mpg)
#' mtcars %>% group_by(cyl) %>% compute_boxplot(~mpg)
#' @export
compute_boxplot <- function(x, var = NULL, coef = 1.5) {
  UseMethod("compute_boxplot")
}

#' @export
compute_boxplot.grouped_df <- function(x, var = NULL, coef = 1.5) {
  dplyr::do(x, compute_boxplot(., var, coef))
}

#' @export
compute_boxplot.data.frame <- function(x, var = NULL, coef = 1.5) {
  vals <- eval_vector(x, var)

  # Special case zero-row input
  if (length(vals) == 0) {
    n0 <- numeric(0)
    res <- data.frame(
      min_ = n0, lower_ = n0, median_ = n0, upper_ = n0, max_ = n0
    )
    res$outliers_ <- list()
    return(res)
  }

  qs <- c(0, 0.25, 0.5, 0.75, 1)

  stats <- as.numeric(quantile(vals, qs))
  names(stats) <- c("min_", "lower_", "median_", "upper_", "max_")

  iqr <- diff(stats[c(2, 4)])

  outliers <- vals < (stats[2] - coef * iqr) | vals > (stats[4] + coef * iqr)
  if (any(outliers)) {
    stats[c(1, 5)] <- range(c(stats[2:4], vals[!outliers]), na.rm = TRUE)
  }

  res <- as.data.frame(as.list(stats))
  res$outliers_ <- list(vals[outliers])
  res
}

#' @export
compute_boxplot.ggvis <- function(x, var = NULL, coef = 1.5) {
  args <- list(var = var, coef = coef)

  register_computation(x, args, "boxplot", function(data, args) {
    output <- do_call(compute_boxplot, quote(data), .args = args)
    preserve_constants(data, output)
  })
}



compute_boxplot_outliers <- function(x) UseMethod("compute_boxplot_outliers")

compute_boxplot_outliers.grouped_df <- function(x) {
  dplyr::do(x, compute_boxplot_outliers(.))
}

compute_boxplot_outliers.data.frame <- function(x) {
  data.frame(value_ = unlist(x$outliers_))
}

compute_boxplot_outliers.ggvis <- function(x) {
  register_computation(x, args = NULL, "boxplot_outliers", function(data, args) {
    output <- compute_boxplot_outliers(data)
    preserve_constants(data, output)
  })
}
