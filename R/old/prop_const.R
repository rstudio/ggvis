# #' Property: constant
# #'
# #' Use a constant value for a mark property.
# #'
# #' @param value The value of the constant. If \code{scale = FALSE}, the default
# #'   then this value needs to be interpretable in the aesthetic space (e.g.
# #'   for colour, "red"; for position or size, an integer number of pixels).
# #'   Otherwise, it will be scaled before plot
# #' @param scale Should the value be scaled? \code{FALSE}, the default, then
# #'   the value will be left as is. If \code{TRUE}, the default scale for that
# #'   property will be used. Otherwise, you can supply the name of a specific
# #'   scale as a string.
# #' @param mult A multiplier for the value, equivalent to (mult * value).
# #'   Multipliers are applied after any scale transformation.
# #' @param offset A simple additive offset to bias the final value, equivalent to
# #'   (value + offset). Offsets are added after any scale transformation and
# #'   multipliers.
# #' @export
# #' @examples
# #' prop_const("red")
# #' prop_const("red", scale = TRUE)
# #' prop_const("red", scale = "alarm")
# #' prop_const(offset = -4)
# prop_const <- function(value = NULL, scale = FALSE, mult = NULL, offset = NULL) {
#   if (!is.null(value)) stopifnot(is.atomic(value), length(value) == 1)
#   stopifnot(is.logical(scale) || is.character(scale), length(scale) == 1)
#   if (!is.null(mult)) stopifnot(is.numeric(mult), length(mult) == 1)
#   if (!is.null(offset)) stopifnot(is.numeric(offset), length(offset) == 1)

#   structure(
#     list(value = value, scale = scale, mult = mult, offset = offset),
#     class = c("prop_const", "prop"))
# }

# #' @S3method format prop_const
# format.prop_const <- function(x, ...) {
#   if (identical(x$scale, TRUE)) {
#     scale <- " [auto]"
#   } else if (is.character(x$scale)) {
#     scale <- paste0(" [", x$scale, "]")
#   } else {
#     scale <- ""
#   }
#   params <- param_string(compact(x[c("mult", "offset")]), collapse = FALSE)

#   paste0("<const> ", x$value, scale, "\n",
#     if (length(params) > 0) paste0(" * ", names(params), " ", params, collapse = "\n")
#   )
# }
# #' @S3method print prop_const
# print.prop_const  <- function(x, ...) cat(format(x, ...), "\n", sep = "")

# #' @S3method prop_value prop_const
# prop_value.prop_const <- function(x, data, processed = FALSE) {
#   x$value
# }

# #' @S3method prop_name prop_const
# prop_name.prop_const <- function(x, data) {
#   ""
# }

# #' @S3method prop_scale prop_const
# prop_scale.prop_const <- function(x, default_scale) {
#   if (isTRUE(x$scale)) {
#     default_scale
#   } else if (is.character(x$scale)) {
#     x$scale
#   } else {
#     NA
#   }
# }

# #' @S3method prop_vega prop_const
# prop_vega.prop_const <- function(x, default_scale) {
#   scale <- prop_scale(x, default_scale)
#   compact(list(
#     value = x$value,
#     scale = if (!is.na(scale)) scale,
#     mult = x$mult,
#     offset = x$offset
#   ))
# }

# #' @S3method prop_domain prop_const
# # FIXME: for scaled constants, this should really insert a literal value in
# #   to the domain, but it's not obvious how to do that in vega currently.
# prop_domain.prop_const <- function(x, data) {
#   NULL
# }

