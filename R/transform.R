# This is to be called by transform_* functions, like transform_smooth,
# transform_bin, etc.
transform <- function(type, ...) {
  structure(list(...), class = c(paste0("transform_", type), "transform"))
}

#' @S3method print transform
print.transform <- function(x, ...) str(x)


# Returns a string representing the transform type. For example, if it has
# class "transform_smooth", then this returns "smooth".
transform_type <- function(transform) {
  classes <- class(transform)
  type <- classes[grep("^transform_", classes)]
  sub("^transform_", "", type)
}


# Apply transformation to a data object, dispatching on transform type. A
# method should be implemented for each type of transform.
apply_transform <- function(transform, data, mapping) UseMethod("apply_transform")


is_constant <- function(x) all(x == x[1])
