#' Create a default scale for a given property and variable type.
#'
#' Default scales depend on both the property (e.g. fill, x, opacity) and
#' the type of variable (e.g. numeric, nominal, ordinal). For this reason
#' \code{default_scale} implements a crude S3-like double dispatch mechanism,
#' looking for a function called \code{dscale_prop_type}. \code{dscale} is a
#' short-hand useful for interactive exploration.
#'
#' @section Scale selection:
#'
#' First, the type of scale is selected based on the \code{type}:
#'
#' \itemize{
#'   \item datetime: \code{\link{scale_time}}
#'   \item ordinal, nominal, logical: \code{\link{scale_ordinal}}
#'   \item numeric: \code{\link{scale_quantitative}}
#' }
#'
#' (see \code{\link{proptype_to_scale}} for more details)
#'
#' then the range is selected based on the combination of the \code{type}
#' and the \code{prop} - for example, you get a different range of colours
#' depending on whether the data is numeric, ordinal, or nominal. Some scales
#' also set other properties - for example, nominal/ordinal position scales
#' also add some padding so that points are spaced away from plot edges.
#'
#' Not all combinations have an existing default scale - if you use a
#' combination that does not have an existing combination, it may suggest
#' you're displaying the data in a suboptimal way. For example, there is
#' no default for a numeric shape scale, because there's no obvious way to
#' map continuous values to discrete shapes. On the other hand, I may have
#' just forgotten to add the appropriate default :/.
#'
#' You can add your own defaults (or override existing) by calling
#' \code{\link{add_scale_defaults}}: just be aware that this is a global setting.
#'
#' @param prop A vega property name.
#' @param type A variable type.  One of datetime, numeric, ordinal, nominal,
#'   logical.
#' @param ... other arguments passed to the scale function. See the help for
#'   \code{\link{scale_quantitative}}, \code{\link{scale_ordinal}} and
#'   \code{\link{scale_time}} for more details. For example, you might supply
#'   \code{trans = "log"} to create a log scale.
#' @param name If \code{NULL}, the default, the scale name is computed by
#'   calling \code{\link{prop_to_scale}(prop)}. This ensures that by default
#'   properties like \code{y} and \code{y2}, or \code{opacity},
#'   \code{fillOpacity} and \code{strokeOpacity} all share the same scale.
#'   Set this to a custom name to override that behaviour, or to create
#'   multiple scales for stroke or fill, or (god forbid) a secondary y scale.
#' @export
#' @examples
#' default_scale("x", "numeric")
#' default_scale("fillOpacity", "ordinal")
#' default_scale("stroke", "nominal")
#'
#' # You can also supply additional arguments or override the defaults
#' default_scale("x", "numeric", trans = "log")
#' default_scale("stroke", "nominal", range = c("red", "blue"))
default_scale <- function(prop, type, ..., name = NULL) {
  check_empty_args()
  assert_that(is.string(prop), is.string(type))
  if (type == "NULL") return()

  scale <- prop_to_scale(prop)

  default <- scale_defaults[[paste0(scale, "_", type)]]
  if (is.null(default)) {
    stop("Don't know how to make default scale for ", prop,
      " with variable of type ", type, call. = FALSE)
  }

  f <- match.fun(default$scale)
  supplied <- list(name = name %||% scale, ...)
  do.call(f, merge_vectors(default$values, supplied))
}
#' @export
#' @rdname default_scale
dscale <- default_scale

#' Convert the name of a property to the name of it's default scale.
#'
#' This is mainly used to ensure that similar properties share the same
#' scale by default - e.g. \code{x} and \code{x2} should use the same
#' scale.
#'
#' @param prop character vector of property names. Any unrecognised names
#'   are left unchanged.
#' @return character vector of default scale names.
#' @keywords internal
#' @export
#' @examples
#' prop_to_scale(c("x", "x2"))
#' prop_to_scale(c("foo", "bar"))
#' prop_to_scale(c("opacity", "fillOpacity", "strokeOpacity"))
prop_to_scale <- function(prop) {
  simplify <- c(
    "x2" = "x",
    "y2" = "y",
    "fillOpacity" = "opacity",
    "strokeOpacity" = "opacity",
    "innerRadius" = "radius",
    "outerRadius" = "radius",
    "startAngle" = "angle",
    "endAngle" = "angle"
  )

  matches <- match(prop, names(simplify))
  prop[!is.na(matches)] <- simplify[prop[!is.na(matches)]]
  prop
}

#' Convert the type of a property to the type of it's default scale.
#'
#' @param type property type: numeric, ordinal, nominal, logical or datetime.
#' @keywords internal
#' @export
proptype_to_scale <- function(type) {
  unname(c(
    "numeric" = "scale_quantitative",
    "ordinal" = "scale_quantitative",
    "nominal" = "scale_ordinal",
    "logical" = "scale_ordinal",
    "datetime" = "scale_time"
  )[type])
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
      scale = proptype_to_scale(type),
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
add_scale_defaults("stroke", c("numeric", "ordinal"), range = c("#132B43", "#56B1F7"))
add_scale_defaults("stroke", "nominal", range = "category10")
# Fill colours should really be a little more saturated
add_scale_defaults("fill", c("numeric", "ordinal"), range = c("#132B43", "#56B1F7"))
add_scale_defaults("fill", "nominal", range = "category10")

# Other nominal properties
add_scale_defaults("shape", "nominal", range = "shapes")

# Other ordinal properties
add_scale_defaults("size", c("numeric", "ordinal"), range = c(20, 100))

add_scale_defaults("fontSize", c("numeric", "ordinal"), range = c(10, 20))

add_scale_defaults("opacity", c("numeric", "ordinal"), range = c(0, 1))

add_scale_defaults("angle", c("numeric", "ordinal"), range = c(0, 2 * pi))

add_scale_defaults("radius", c("numeric", "ordinal"), range = c(0, 50))
