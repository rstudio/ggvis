# This is to be called by transform_* functions, like transform_smooth,
# transform_bin, etc.
transform <- function(type, ...) {
  structure(list(...), class = c(paste0("transform_", type), "transform"))
}
print.transform <- function(x, ...) str(x)


# Returns a string representing the transform type. For example, if it has
# class "transform_smooth", then this returns "smooth".
transform_type <- function(transform) {
  classes <- class(transform)
  type <- classes[grep("^transform_", classes)]
  sub("^transform_", "", type)
}


# Apply transformation to a data object, dispatching on data type
apply_transform <- function(data, transform, mapping) UseMethod("apply_transform")

#' @S3method apply_transform data.frame
apply_transform.data.frame <- function(data, transform, mapping) {
  # Identify constant variables, extract and add back in
  constant_vars <- vapply(data, is.constant, logical(1))
  
  transformed <- compute(transform, data, mapping)
  carry_over <- data[1, constant_vars, drop = FALSE]
  rownames(carry_over) <- NULL
  
  cbind(transformed, carry_over)
}

#' @S3method apply_transform split_data_dflist
apply_transform.split_data_dflist <- function(data, transform, mapping) {
  structure(
    lapply(data, apply_transform, transform = transform, mapping = mapping),
    class = c("split_data_dflist", "split_data")
  )
}


# Compute transformation. This is an S3 generic; a method should be implemented
# for each type of transform.
compute <- function(transform, data, mapping) UseMethod("compute")


is.constant <- function(x) all(x == x[1])
