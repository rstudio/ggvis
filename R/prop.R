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
prop_vega <- function(x, default_scale) {
  UseMethod("prop_vega")
}
