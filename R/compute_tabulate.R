#' Count data at each location of a categorical variable
#'
#' @param x Dataset-like object to count. Built-in methods for data frames,
#'   grouped data frames and ggvis visualisations.
#' @param x_var,w_var Names of x and weight variables.
#' @seealso \code{\link{compute_bin}} For counting cases within ranges of
#'   a continuous variable.
#' @seealso \code{\link{compute_count}} For counting cases at specific locations
#'   of a continuous variable. This is useful when the variable is continuous
#'   but the data is granular.
#' @export
#' @return A data frame with columns:
#'  \item{count_}{the number of points}
#'  \item{x_}{value of bin}
#' @examples
#' library(dplyr)
#' # The tabulated column must be countable (not numeric)
#' \dontrun{mtcars %>% compute_tabulate(~cyl)}
#' mtcars %>% mutate(cyl = factor(cyl)) %>% compute_tabulate(~cyl)
#'
#' # Or equivalently:
#' mtcars %>% compute_tabulate(~factor(cyl))
#'
#' # If there's one weight value at each x, it effectively just renames columns.
#' pressure %>% compute_tabulate(~factor(temperature), ~pressure)
#'
#' # It doesn't matter whether you transform inside or outside of a vis
#' mtcars %>% compute_tabulate(~factor(cyl)) %>%
#'   ggvis(x = ~x_, y = ~count_, y2 = 0) %>%
#'   layer_rects(width = band())
#'
#' mtcars %>%
#'   ggvis(x = ~x_, y = ~count_, y2 = 0) %>%
#'   compute_tabulate(~factor(cyl)) %>%
#'   layer_rects(width = band())
#'
#' # compute_tabulate is used automatically in layer_bars when no y prop
#' # is supplied.
#' mtcars %>% ggvis(x = ~factor(cyl)) %>% layer_bars()
compute_tabulate <- function(x, x_var, w_var = NULL) {
  UseMethod("compute_tabulate")
}

#' @export
compute_tabulate.data.frame <- function(x, x_var, w_var = NULL) {
  assert_that(is.formula(x_var))

  x_val <- eval_vector(x, x_var)
  if (!vector_countable(x_val)) {
    stop("compute_tabulate requires categorical (countable) data.",
      call. = FALSE)
  }

  if (is.null(w_var)) {
    w_val <- NULL
  } else {
    w_val <- eval_vector(x, w_var)
  }

  tabulate_vector(x_val, weight = w_val)
}

#' @export
compute_tabulate.grouped_df <- function(x, x_var, w_var = NULL) {
  dplyr::do(x, compute_tabulate(., x_var, w_var = w_var))
}

#' @export
compute_tabulate.ggvis <- function(x, x_var, w_var = NULL) {
  args <- list(x_var = x_var, w_var = w_var)

  register_computation(x, args, "count", function(data, args) {
    output <- do_call(compute_tabulate, quote(data), .args = args)
    preserve_constants(data, output)
  })
}

# Tabulate individual vector ---------------------------------------------------

tabulate_vector <- function(x, weight = NULL, ...) {
  if (is.null(weight)) {
    weight <- rep.int(1, length(x))
  }
  counts <- unname(as.vector(tapply(weight, x, sum, na.rm = TRUE)))

  if (is.factor(x)) {
    # Get the factor levels, preserving factor-ness. Order should align
    # with counts.
    values <- unique(x)
  } else {
    # Need to get unique values this way instead of using names(counts),
    # because names are strings but the x values aren't always strings.
    values <- unname(as.vector(tapply(x, x, unique, na.rm = TRUE)))
  }

  data.frame(
    count_ = counts,
    x_ = values,
    stringsAsFactors = FALSE
  )
}
