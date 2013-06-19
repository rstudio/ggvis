#' @export
is.prop <- function(x) inherits(x, "prop")

#' Given a property and a dataset, get the value of the property.
#' 
#' @param x a vega property
#' @param data a object containing data
prop_value <- function(x, data) {
  UseMethod("prop_value")  
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
#' @keywords internal
prop_type <- function(data, prop) {
  UseMethod("prop_type")
}
#' @S3method prop_type split_df
prop_type.split_df <- function(data, prop) {
  types <- vapply(data, prop_type, prop = prop, FUN.VALUE = character(1))
  if (!all_same(types)) {
    stop("Inconsistent types", call. = FALSE)
  }
  types[1]
}
#' @S3method prop_type data.frame
prop_type.data.frame <- function(data, prop) {
  prop_type(prop_value(prop, data))
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
  ranges <- vapply(data, prop_range, na.rm = na.rm, 
    FUN.VALUE = numeric(2))
  range(ranges)
}
