#' Transformation: identity.
#'
#' This transformation simply evaluates each property in the context of the
#' data.
#' @export
#' @examples
#' transform_identity()
#' sluice(transform_identity(), props(x ~ disp), mtcars)
transform_identity <- function() {
  transform("identity")
}

#' @S3method format transform_identity
format.transform_identity <- function(x, ...) {
  paste0(" -> identity")
}

#' @S3method compute transform_identity
compute.transform_identity <- function(x, props, data) {
  values <- lapply(props, prop_value, data)
  names(values) <- vapply(props, prop_name, character(1))

  data.frame(values)
}
