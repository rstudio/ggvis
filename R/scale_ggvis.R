#' Create a new ggvis_scale object.
#'
#' A scale object is a close mapping to a vega mark object. Vega scales
#' are documented in \url{https://github.com/trifacta/vega/wiki/Scales}.
#'
#' This function is designed to be used by authors of new types of scale.
#' If you are a ggvis user, please use one of the more specific scale
#' functions starting with the \code{scale_}.
#'
#' This is very close, but not exactly a vega scale object. Instead of being a
#' named list with a set of values, the domain can be  a vector of values, or a
#' reactive that returns such values.
#'
#' @param property The property to which the scale applies, such as "x", "y",
#'   "fill", etc.
#' @param name Name of the scale, such as "x", "y", "fill", etc. Can also be an
#'   arbitrary name like "foo".
#' @param label Label for the scale. Used for axis or legend titles.
#' @param type Type of scale. Should be one of "linear", "ordinal", "time",
#'   "utc", "linear", "log", "pow", "sqrt", "quantile", "quantize", "threshold".
#' @param domain The domain of the scale, representing the set of data values.
#'   For ordinal scales, a character vector; for quantitative scales, a numeric
#'   vector of length two. Either value (but not both) may be NA, in which
#'   case \code{domainMin} or \code{domainMax} is set. For dynamic scales, this
#'   can also be a reactive which returns the appropriate type of vector.
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
#' @param override Should the domain specified by this ggvis_scale object
#'   override other ggvis_scale objects for the same scale? Useful when domain is
#'   manually specified. For example, by default, the domain of the scale
#'   will contain the range of the data, but when this is TRUE, the specified
#'   domain will override, and the domain can be smaller than the range of the
#'   data. If \code{FALSE}, the \code{domain} will not behave this way. If
#'   left \code{NULL}, then it will be treated as \code{TRUE} whenever
#'   \code{domain} is non-NULL.
#' @seealso \url{https://github.com/trifacta/vega/wiki/Scales}
#' @export
#' @keywords internal
#' @examples
#' ggvis_scale("x", type = "linear")
#' ggvis_scale("x", "ord")
ggvis_scale <- function(property, name = property, label = name, type = NULL,
                        domain = NULL, range = NULL, reverse = NULL,
                        round = NULL, ..., subclass = NULL, override = NULL) {
  assert_that(is.string(name))
  assert_that(is.null(type) ||
              type %in% c("linear", "ordinal", "time", "utc", "log", "pow",
                          "sqrt", "quantile", "quantize", "threshold"))
  assert_that(is.null(reverse) || is.flag(reverse),
              is.null(round) || is.flag(round))

  if (!is.null(subclass)) {
    assert_that(is.string(subclass))
    subclass <- paste0("scale_", subclass)
  }

  # By default, if domain is specified, it should override other domains
  override <- override %||% !is.null(domain)

  structure(
    drop_nulls(c(
      list(
        property = property, name = name, label = label, type = type,
        reverse = reverse, round = round, domain = domain, override = override,
        ...
      ),
      range_prop(range, "range")
    )),
    class = c(subclass, "ggvis_scale")
  )
}

#' @export
#' @rdname ggvis_scale
#' @param x object to test for scale-ness
is.ggvis_scale <- function(x) inherits(x, "ggvis_scale")

#' @export
format.ggvis_scale <- format.ggvis_axis

#' @export
print.ggvis_scale <- print.ggvis_axis

#' @export
as.vega.ggvis_scale <- function(x) {
  if (shiny::is.reactive(x$domain)) {
    # Replace the domain with something that grabs it from the domain data
    x$domain <- list(
      data = paste0("scale/", x$name),
      field = "data.domain"
    )
  }

  # Remove the non-vega items
  x$property <- NULL
  x$label <- NULL
  x$override <- NULL
  x$expand <- NULL

  x
}

# Collapse all scales inside each vis
collapse_scales <- function(x) {
  if (is.ggvis(x) || is.mark_group(x)) {
    x$scales <- collapse_scales_by_name(x$scales)
    x$marks <- lapply(x$marks, collapse_scales)
    x
  } else {
    x
  }
}


collapse_scales_by_name <- function(x) {
  by_name <- split(x, vpluck(x, "name", character(1)))
  lapply(by_name, collapse_ggvis_scales)

}

# Takes a list of ggvis_scale objects and collapses them into a single
# ggvis_scale object.
collapse_ggvis_scales <- function(scales) {
  if (empty(scales)) return(NULL)

  countable <- unique(unlist(lapply(scales, scale_countable)))
  if (length(unique(countable)) > 1) {
    stop("Scales must all be countable, or all not countable.")
  }

  collapse_domains <- function(domains, overrides, countable) {
    # Set the domain based on whether there is an override domain, and the type
    # of domain.
    if (any(overrides)) {
      # Use the last overriding domain, if more than one
      over <- domains[[last(which(overrides))]]
      under <- domains[!overrides]

      if (countable) {
        # For categorical props, just use the override domain
        domain <- reactive(value(over))

      } else {
        # For continuous props, if either of the values in the override domain
        # is NA, then use the upper/lower bound of the non-override domains for
        # that value.
        domain <- reactive({
          over_vals <- value(over)

          if (any(is.na(over_vals))) {
            under_vals <- data_range(concat(values(under)))
            if (is.na(over_vals[1])) over_vals[1] <- under_vals[1]
            if (is.na(over_vals[2])) over_vals[2] <- under_vals[2]
          }
          over_vals
        })
      }

    } else {
      # If no overrides, just get range from the data
      domain <- reactive({
        data_range(concat(values(domains)))
      })
    }
    domain
  }

  domain <- collapse_domains(
    domains = pluck(scales, "domain"),
    overrides = vpluck(scales, "override", logical(1)),
    countable = countable
  )

  # Merge scales from left to right. A couple fields need special treatment.
  new_scale <- Reduce(merge_ggvis_scales, scales)
  # Get last non-NULL label
  new_scale$label <- last(compact(pluck(scales, "label")))

  new_scale$domain <- domain
  new_scale$override <- NULL

  new_scale <- apply_scale_defaults(new_scale)

  # Must expand domains after the scale defaults are applied
  expand <- new_scale$expand
  if (!scale_countable(new_scale) && !is.null(expand) && any(expand != 0)) {
    old_domain <- new_scale$domain
    new_scale$domain <- reactive({
      expand_range(old_domain(), expand)
    })
  }

  new_scale
}

# Takes a named list, where each name is the name of a scale, and each item is
# a scale object. Returns a named list where there's one item per scale,
# and each item is a reactive that returns a data frame with values for the
# domain.

scale_domain_data <- function(x) {
  # Find scales in all vises
  scales <- gather_scales(x)
  # Only need to make data domain for reactive scales
  scales <- Filter(function(scale) shiny::is.reactive(scale$domain), scales)
  names(scales) <- paste0("scale/", vpluck(scales, "name", character(1)))

  lapply(scales, function(scale) {
    force(scale)
    reactive({
      data.frame(domain = value(scale$domain))
    })
  })
}


expand_range <- function(range, mult = 0) {
  if (length(range) == 0) return(range)
  if (length(range) != 2) stop("range must have either 0 or 2 values")
  if (length(mult) == 1) mult <- c(mult, mult)

  range + diff(range) * mult * c(-1, 1)
}

merge_ggvis_scales <- function(a, b) {
  # a and b may have more than one class. If there's more than one, b's class
  # has precedence.
  if (length(class(b)) > 1) {
    new_class <- class(b)
  } else {
    new_class <- class(a)
  }

  structure(merge_vectors(a, b), class = new_class)
}
