#' S3 class: transform
#' 
#' This is a type of \code{\link{pipe}}.
#' 
#' @export
#' @keywords internal
transform <- function(type, ...) {
  type <- c(paste0("transform_", type), "transform")
  pipe(type, ...)
}


# Returns a string representing the transform type. For example, if it has
# class "transform_smooth", then this returns "smooth".
transform_type <- function(transform) {
  classes <- class(transform)
  type <- classes[grep("^transform_", classes)]
  sub("^transform_", "", type)
}


# Apply transformation to a data object, dispatching on transform type. A
# method should be implemented for each type of transform.
apply_transform <- function(transform, data, mapping) {
  flow(transform, mapping, data) 
}
