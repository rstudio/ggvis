#' @export
is.prop <- function(x) inherits(x, "prop")

#' Given a property and a dataset, get the value of the property.
#'
#' @param x a vega property
#' @param data a object containing data
#' @param processed Has this data object been processed so that new columns
#'   have been calculated and unused columns have been dropped?
prop_value <- function(x, data, processed) {
  UseMethod("prop_value")
}

#' The name of the property.
#' 
#' Used for naming the variable it produces in the vega data frame
#' 
#' @param x a vega property
prop_name <- function(x) {
  UseMethod("prop_name")
}

#' The scale (if any) that this property needs
#' 
prop_scale <- function(x, default_scale) {
  UseMethod("prop_scale")
}

#' Generate a vega object for the individual mark.
#'
#' Will be inserted in to the vega properties object.
#' @param x a vega property
#' @param default_scale default scale to use if not already specified in vega
#'   object
#' @keywords internal
prop_vega <- function(x, default_scale) {
  UseMethod("prop_vega")
}

#' Determine the variable type given a data frame and property.
#'
#' @param data The data object.
#' @param prop The property object.
#' @param processed Has this data object been processed so that new columns
#'   have been calculated and unused columns have been dropped?
#' @keywords internal
prop_type <- function(data, prop, processed = FALSE) {
  UseMethod("prop_type")
}
#' @S3method prop_type split_df
prop_type.split_df <- function(data, prop, processed = FALSE) {
  types <- vapply(data, prop_type, prop = prop, processed = processed,
    FUN.VALUE = character(1))
  if (!all_same(types)) {
    stop("Inconsistent types", call. = FALSE)
  }
  types[1]
}
#' @S3method prop_type data.frame
prop_type.data.frame <- function(data, prop, processed = FALSE) {
  prop_type(prop_value(prop, data, processed))
}
#' @S3method prop_type POSIXt
prop_type.POSIXt <- function(data, prop) "time"
#' @S3method prop_type Date
prop_type.Date <- function(data, prop) "date"
#' @S3method prop_type numeric
prop_type.numeric <- function(data, prop) "double"
#' @S3method prop_type integer
prop_type.integer <- function(data, prop) "integer"
#' @S3method prop_type complex
prop_type.complex <- function(data, prop) "complex"
#' @S3method prop_type character
prop_type.character <- function(data, prop) "character"
#' @S3method prop_type logical
prop_type.logical <- function(data, prop) "logical"
#' @S3method prop_type factor
prop_type.factor <- function(data, prop) "factor"
#' @S3method prop_type NULL
prop_type.NULL <- function(data, prop) "NULL"
#' @S3method prop_type default
prop_type.default <- function(data, prop) {
  stop("Unknown variable type: ", paste0(class(data), collapse = "/"))
}

#' Determine the numeric range of a variable
#'
#' @keywords internal
prop_range <- function(data, prop, na.rm = TRUE) {
  UseMethod("prop_range")
}
#' @S3method prop_range data.frame
prop_range.data.frame <- function(data, prop, na.rm = TRUE) {
  range(prop_value(prop, data), na.rm = na.rm)
}
#' @S3method prop_range split_df
prop_range.split_df <- function(data, var, na.rm = TRUE) {
  ranges <- vapply(data, prop_range, var, na.rm = na.rm,
    FUN.VALUE = numeric(2))
  range(ranges)
}
