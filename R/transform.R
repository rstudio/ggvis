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

check_prop <- function(trans, props, data, prop_name, types = NULL) {
  name <- class(trans)[[1]]
  prop <- props[[prop_name]]

  if (is.null(prop)) {
    stop(name, "() needs ", prop_name, " property", call. = FALSE)
  }
  if (is.null(types)) return(invisible(TRUE))

  type <- prop_type(data, prop)
  if (!(type %in% types)) {
    stop(name, "() needs ", prop_name, " property to be of type ",
      paste(types, collapse = "/"), call. = FALSE)
  }

  invisible(TRUE)
}

preserve_constants <- function(input, output) UseMethod("preserve_constants")

preserve_constants.data.frame <- function(input, output) {
  is_constant <- constant_vars(input)
  constants <- input[1, is_constant, drop = FALSE]
  rownames(constants) <- NULL

  merge_df(constants, output)
}

preserve_constants.split_df <- function(input, output) {
  is_constant <- constant_vars(input)

  preserve <- function(input, output) {
    constants <- input[1, is_constant, drop = FALSE]
    rownames(constants) <- NULL
    merge_df(constants, output)
  }
  structure(Map(preserve, input, output), class = "split_df")
}

constant_vars <- function(data) UseMethod("constant_vars")
#' @S3method constant_vars data.frame
constant_vars.data.frame <- function(data) {
  vapply(data, all_same, logical(1), USE.NAMES = FALSE)
}
#' @S3method constant_vars split_df
constant_vars.split_df <- function(data) {
  n <- length(data)

  vec <- unlist(lapply(data, constant_vars), use.names = FALSE)
  mat <- matrix(vec, nrow = n, byrow = TRUE)
  colSums(mat) == n
}

#' @S3method pipe_id transform
pipe_id.transform <- function(x) {
  paste(transform_type(x), digest(x, algo = "crc32"), sep = "_")
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
