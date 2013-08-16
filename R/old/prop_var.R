# #' Property: variable
# #'
# #' Given a quoted object, wrap it in a list and attach a class. The
# #' list-wrapping is needed because attaching a class directly to a symbol
# #' is not possible.
# #'
# #' Long-term this function needs to behave more like dplyr::partial_eval so
# #' that it captures local values immediately.
# #'
# #' @param x A quoted object
# #' @inheritParams prop_const
# #' @export
# #' @examples
# #' prop_var(quote(x))
# #' prop_var(quote(1))
# #' prop_var(quote(x * y))
# #'
# #' prop_var(quote(cyl))
# #' prop_var(quote(cyl), offset = -1, scale = FALSE)
# prop_var <- function(x, scale = TRUE, offset = NULL, mult = NULL) {
#   assert_that(is.quoted(x))
#   assert_that(is.flag(scale) || is.string(scale))

#   prop("prop_var",
#     x = x, scale = scale, offset = offset, mult = mult)
# }

# #' @S3method format prop_var
# format.prop_var <- function(x, ...) {
#   paste0("<variable> ", deparse(x[[1]]))
# }

# #' @S3method print prop_var
# print.prop_var <- function(x, ...) cat(format(x, ...), "\n", sep = "")

# #' @rdname prop_var
# is.prop_var <- function(x) inherits(x, "prop_var")

# #' @S3method prop_value prop_var
# prop_value.prop_var <- function(x, data, processed = FALSE) {
#   if (processed)
#     data[[prop_name(x)]]
#   else
#     eval(x[[1]], data, baseenv())
# }

# #' @S3method prop_name prop_var
# prop_name.prop_var <- function(x) {
#   var <- x[[1]]

#   if (is.symbol(var)) {
#     safe_vega_var(as.character(var))

#   } else if (is.language(var)) {
#     # var is calculated from an expression; get a unique, JS-safe name. Prepend
#     # a string to so that an expression with same text as a var will have a
#     # different hash, e.g., the expression wt/mpg vs. the variable `wt/mpg`.
#     safe_vega_var(paste0("[e]", deparse(var)))

#   } else {
#     # var is a constant
#     ""
#   }
# }

# #' @S3method prop_scale prop_var
# prop_scale.prop_var <- function(x, default_scale) {
#   if (isTRUE(x$scale)) {
#     default_scale
#   } else if (is.character(x$scale)) {
#     x$scale
#   } else {
#     NA
#   }
# }

# #' @S3method prop_domain prop_var
# prop_domain.prop_var <- function(x, data) {
#   list(
#     data = data,
#     field = paste0("data.", prop_name(x))
#   )
# }

# #' @S3method prop_vega prop_var
# prop_vega.prop_var <- function(x, default_scale) {
#   scale <- prop_scale(x, default_scale)
#   compact(list(
#     field = paste0("data.", prop_name(x)),
#     scale = if (!is.na(scale)) scale,
#     mult = x$mult,
#     offset = x$offset
#   ))
# }

# # Given a prop_var object, return a string representation of the value
# # @examples
# # p <- props(x ~ mpg, y = 10)
# # as.character.prop_var(p$x)
# #' @S3method as.character prop_var
# as.character.prop_var <- function(x, ...) {
#   if (!is.prop_var(x)) {
#     stop("x is not a prop_var object", call. = FALSE)
#   }
#   deparse(x[[1]])
# }

# as.prop_var <- function(x) UseMethod("as.prop_var")
# #' @S3method as.prop_var character
# as.prop_var.character <- function(x) prop_var(as.name(x))
# #' @S3method as.prop_var name
# as.prop_var.name <- function(x) prop_var(x)
# #' @S3method as.prop_var call
# as.prop_var.call <- function(x) prop_var(x)
# #' @S3method as.prop_var prop_var
# as.prop_var.prop_var <- function(x) x
