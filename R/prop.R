#' @export
is.prop <- function(x) inherits(x, "prop")

#' Given a property and a dataset, get the value of the property.
#' 
#' @param x a vega property
#' @param data a object containing data
prop_value <- function(x, data) {
  UseMethod("prop_value")  
}

