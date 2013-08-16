#' Property: reactive
#'
#' @param x A delayed reactive object
#' @inheritParams prop_var
#' @param constant is it constant or variable
#' @export
#' @examples
#' prop_reactive(input_select(c("red", "blue")), TRUE, FALSE)
#' prop_reactive(input_select(c("a", "b")), TRUE, TRUE)
#' prop_reactive(input_select(c("mpg", "wt")), FALSE, TRUE)
#' # If the data frame has colums 'colour' and 'colour2', which contain actual
#' # colors:
#' prop_reactive(input_select(c("colour", "colour2")), FALSE, FALSE)
# prop_reactive <- function(x, constant = TRUE, scale = TRUE, offset = NULL,
#                           mult = NULL) {
#   assert_that(is.delayed_reactive(x))
#   assert_that(is.flag(scale) || is.string(scale))

#   prop("prop_reactive",
#     dr = x,
#     constant = constant,
#     scale = scale,
#     offset = offset,
#     mult = mult,
#     value = function() stop("Delayed reactive not yet advanced!")
#   )
# }

# #' @S3method format prop_reactive
# format.prop_reactive <- function(x, ...) {
#   paste0("<reactive> ", x$dr$id)
# }

# #' @S3method print prop_reactive
# print.prop_reactive <- function(x, ...) cat(format(x, ...), "\n", sep = "")

# #' @rdname prop_reactive
# is.prop_reactive <- function(x) inherits(x, "prop_reactive")

# #' @S3method prop_value prop_reactive
# prop_value.prop_reactive <- function(x, data, processed = FALSE) {
#   val <- x$value()

#   if (x$constant) {
#     rep(val, nrow(data))
#   } else {
#     data[[val]]
#   }
# }

# #' @S3method prop_name prop_reactive
# prop_name.prop_reactive <- function(x) {
#   x$dr$id
# }

# #' @S3method prop_scale prop_reactive
# prop_scale.prop_reactive <- prop_scale.prop_var

# #' @S3method prop_domain prop_reactive
# prop_domain.prop_reactive <- prop_domain.prop_var

# #' @S3method prop_vega prop_reactive
# prop_vega.prop_reactive <- prop_vega.prop_var

# # Given a prop_reactive object, return a string representation of the value
# # @examples
# # p <- props(x = prop_reactive(input_select(c("red", "blue")), TRUE, FALSE), y = 10)
# # as.character.prop_reactive(p$x)
# #' @S3method as.character prop_reactive
# as.character.prop_reactive <- function(x, ...) {
#   if (!is.prop_reactive(x)) {
#     stop("x is not a prop_reactive object", call. = FALSE)
#   }
#   x$dr$id
# }

# as.prop_reactive <- function(x) UseMethod("as.prop_reactive")
# #' @S3method as.prop_reactive name
# as.prop_reactive.name <- function(x) prop_reactive(x)
# #' @S3method as.prop_reactive call
# as.prop_reactive.call <- function(x) prop_reactive(x)
# #' @S3method as.prop_reactive prop_reactive
# as.prop_reactive.prop_reactive <- function(x) x
