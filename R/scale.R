#' Create a new vega scale object
#'
#' Usually you would not call this function directly, but would instead
#' call one of the subclasses.
#'
#' @param name name of the scale.
#' @param type type of scale. Should be one of "linear", "ordinal", "time",
#'   "utc", "linear", "log", "pow", "sqrt", "quantile", "quantize", "threshold".
#' @param domain The domain of the scale, representing the set of data values.
#'   For ordinal scales, a character vector; for quantitative scales, a numeric
#'   vector of length two. Either value (but not both) may be missing, in which
#'   case \code{domainMin} or \code{domainMax} is set.
#' @param range The range of the scale, representing the set of visual values.
#'   For numeric values, the range can take the form of a two-element array with
#'   minimum and maximum values. For ordinal data, the range may by an array of
#'   desired output values, which are mapped to elements in the specified
#'   domain. The following range literals are also available: "width", "height",
#'   "shapes", "category10", "category20".
#' @param reverse  If true, flips the scale range.
#' @param round If true, rounds numeric output values to integers. This can be
#'   helpful for snapping to the pixel grid.
#' @param ... other named arguments.
#' @param subclass Class name for subclass.  Will have \code{scale_} prepended.
#' @seealso \url{https://github.com/trifacta/vega/wiki/Scales}
#' @export
#' @keywords internal
#' @examples
#' scale("x", "linear")
#' scale("x", "ord")
scale <- function(name, type = NULL, domain = NULL, range = NULL,
                  reverse = FALSE, round = FALSE, ..., subclass = NULL) {
  assert_that(is.string(name))
  type <- match.arg(type, c("linear", "ordinal", "time", "utc", "log",
    "pow", "sqrt", "quantile", "quantize", "threshold"))
  assert_that(is.flag(reverse), is.flag(round))

  if (!is.null(subclass)) {
    assert_that(is.string(subclass))
    subclass <- paste0("scale_", subclass)
  }

  structure(
    drop_nulls(c(
      list(name = name, type = type, reverse = reverse, round = round, ...),
      range_prop(range, "range"),
      range_prop(domain, "domain")
    )),
    class = c(subclass, "scale")
  )
}

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
      named_list(paste0(name, "Min"), x[2])
    }
  } else if (n_miss == 2) {
    list()
  }

}

named_list <- function(names, ...) {
  setNames(list(...), names)
}

#' @export
#' @rdname scale
#' @param x object to test for scale-ness
is.scale <- function(x) inherits(x, "scale")

#' @S3method format scale
format.scale <- function(x, ...) {
  params <- param_string(x, collapse = FALSE)
  param_s <- paste0(" ", format(paste0(names(params), ":")), " ", format(params), "\n",
    collapse = "")

  paste0("<", class(x)[1], ">\n", param_s)
}

#' @S3method print scale
print.scale <- function(x, ...) cat(format(x, ...), "\n", sep = "")
