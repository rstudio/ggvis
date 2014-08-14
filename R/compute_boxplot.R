#' @export
compute_boxplot <- function(x, var = NULL, coef = 1.5) {
  UseMethod("compute_boxplot")
}

# FIXME: plyr is imported only as a workaround for the dplyr issue; once it's
#        fixed, plyr can be removed.
#' @importFrom plyr ddply
#' @export
compute_boxplot.grouped_df <- function(x, var = NULL, coef = 1.5) {
  old_groups <- dplyr::groups(x)
  x <- dplyr::ungroup(x)

  # FIXME: Temporarily use ddply instead of dplyr::do because of dplyr issues
  #        #463 and #514.
  group_names <- vapply(old_groups, as.character, character(1))
  x <- plyr::ddply(x, group_names, function(df) compute_boxplot(df, var, coef))

  x <- dplyr::regroup(x, old_groups)
  x
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
  old_groups <- dplyr::groups(x)
  x <- dplyr::ungroup(x)

  # FIXME: Temporarily use ddply instead of dplyr::do because of dplyr issues
  #        #463 and #514.
  group_names <- vapply(old_groups, as.character, character(1))
  x <- plyr::ddply(x, group_names, compute_boxplot_outliers.data.frame)

  x <- dplyr::regroup(x, old_groups)
  x
}

compute_boxplot_outliers.data.frame <- function(x) {
  data.frame(value_ = unlist(x$outliers_))
}

compute_boxplot_outliers.ggvis <- function(x) {
  register_computation(x, args = NULL, "boxplot_outliers", function(data, args) {
    output <- do_call(compute_boxplot_outliers, quote(data))
    preserve_constants(data, output)
  })
}
