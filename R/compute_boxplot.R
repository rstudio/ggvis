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

  qs <- c(0, 0.25, 0.5, 0.75, 1)

  stats <- as.numeric(quantile(vals, qs))
  names(stats) <- c("min_", "lower_", "median_", "upper_", "max_")

  iqr <- diff(stats[c(2, 4)])

  outliers <- vals < (stats[2] - coef * iqr) | vals > (stats[4] + coef * iqr)
  if (any(outliers)) {
    stats[c(1, 5)] <- range(c(stats[2:4], vals[!outliers]), na.rm = TRUE)
  }

  res <- as.data.frame(as.list(stats))
  res$outliers <- list(vals[outliers])
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
