#' @export
set_default_scale <- function(...) {
  stop("set_default_scale has been removed, and has been replaced by scale_numeric, ",
       "scale_nominal, and so on. See ?scales for more information")
}
#' @export
set_dscale <- set_default_scale


# Given the name of a scale, the ggvis scale type, and other parameters, return
# a vega scale object, with the supplied parameters merged into the defaults.
default_vega_scale <- function(scale, type, ..., name = NULL) {
  check_empty_args()
  if (!(scale %in% valid_scales)) {
    stop(scale, " not in set of valid scales: ",
         paste(valid_scales, collapse = ", "))
  }
  if (type == "NULL") return()
  if (!(type %in% valid_scale_types)) {
    stop(scale, " not in set of valid scale types: ",
         paste(valid_scale_types, collapse = ", "))
  }

  default <- scale_defaults[[paste0(scale, "_", type)]]
  if (is.null(default)) {
    # Silently drop scales that we don't know how to provide: error
    # detection is left up to the mark, because it knows exactly what
    # properties it needs.
    return(NULL)
  }

  f <- match.fun(paste0("vega_scale_", scaletype_to_vega_scaletype(type)))
  supplied <- list(name = name %||% scale, ...)
  do.call(f, merge_vectors(default$values, supplied))
}


scale_defaults <- new.env(parent = emptyenv())

#' Add new default scale properties.
#'
#' You can use this function to add new defaults
#'
#' @param scale scale name
#' @param types a vector of property types, converted to scale types by
#'   \code{\link{proptype_to_scale}}
#' @param ... other arguments to be passed onto the scale function
#' @export
#' @keywords internal
#' @examples
#' \dontrun{
#' # Add a default size scale for nominal variable types: this is probably
#' # a bad idea since sizes are ordered but nominal variables are not. On
#' # the other hand, many factors should probably actually be ordered factors.
#' add_scale_defaults("size", "nominal", range = c(1, 20))
#' }
add_scale_defaults <- function(scale, types, ...) {
  defaults <- list(...)

  for (type in types) {
    name <- paste0(scale, "_", type)
    scale_defaults[[name]] <- list(
      scale = scaletype_to_vega_scaletype(type),
      values = defaults
    )
  }
  invisible()
}

# Position scales
add_scale_defaults("x", c("numeric", "datetime"), range = "width")
add_scale_defaults("x", c("ordinal", "nominal"), range = "width", padding = 0.5)

add_scale_defaults("y", c("numeric", "datetime"), range = "height")
add_scale_defaults("y", c("ordinal", "nominal"), range = "height", padding = 0.5)

# Colour scales
add_scale_defaults("stroke", "numeric", range = c("#132B43", "#56B1F7"))
add_scale_defaults("stroke", c("nominal", "ordinal"), range = "category10")
# Fill colours should really be a little more saturated
add_scale_defaults("fill", "numeric", range = c("#132B43", "#56B1F7"))
add_scale_defaults("fill", c("nominal", "ordinal"), range = "category10")

# Other nominal properties
add_scale_defaults("shape", "nominal", range = "shapes")

# Other ordinal properties
add_scale_defaults("size", c("ordinal", "nominal"), range = c(20, 100))

# Other numeric properties
add_scale_defaults("size", "numeric", range = c(20, 100))

add_scale_defaults("fontSize", "numeric", range = c(10, 20))

add_scale_defaults("opacity", "numeric", range = c(0, 1))

add_scale_defaults("angle", "numeric", range = c(0, 2 * pi))

add_scale_defaults("radius", "numeric", range = c(0, 50))
