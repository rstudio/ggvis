range_prop <- function(x, name) {
  if (is.null(x)) return(list())

  # Character vector always left as is
  if (is.character(x)) {
    return(named_list(name, x))
  }

  assert_that(is.numeric(x), length(x) <= 2)
  n_miss <- sum(is.na(x))

  if (n_miss == 0) {
    named_list(name, x)
  } else if (n_miss == 1) {
    if (is.na(x[1])) {
      named_list(paste0(name, "Max"), x[2])
    } else {
      named_list(paste0(name, "Min"), x[1])
    }
  } else if (n_miss == 2) {
    list()
  }

}

named_list <- function(names, ...) {
  setNames(list(...), names)
}

#' @export
#' @rdname vega_scale
#' @param x object to test for scale-ness
is.scale <- function(x) inherits(x, "scale")

#' @export
format.scale <- format.vega_axis

#' @export
print.scale <- print.vega_axis
